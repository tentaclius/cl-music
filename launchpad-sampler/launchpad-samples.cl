(ql:quickload :cl-collider)
(ql:quickload :cl-alsaseq)

(defpackage :cl-launchpad (:use :cl :sc))
(in-package :cl-launchpad)

;;;
(defstruct mr-midi-event
  type
  note
  velocity
  start
  duration
  channel)

(defun alsa-to-my-midi (message)
  (let* ((event-type (getf message :event-type))
         (event-data (getf message :event-data)))
    (case event-type
      (:SND_SEQ_EVENT_CONTROLLER
       (let ((param (getf event-data 'cl-alsaseq:param))
             (value (getf event-data 'calispel:value))
             (chan (getf event-data 'calispel:channel)))
         (make-mr-midi-event :type :control
                             :note param
                             :velocity value
                             :channel chan)))
      (:SND_SEQ_EVENT_NOTEON
       (let ((note (getf event-data 'cl-alsaseq:note))
             (velo (getf event-data 'cl-alsaseq:velocity))
             (chan (getf event-data 'calispel:channel)))
         (make-mr-midi-event :type :note_on
                             :note note
                             :velocity velo
                             :channel chan)))
      (:SND_SEQ_EVENT_NOTEOFF
       (let ((note (getf event-data 'cl-alsaseq:note))
             (velo (getf event-data 'cl-alsaseq:velocity))
             (chan (getf event-data 'calispel:channel)))
         (make-mr-midi-event :type :note_off
                             :note note
                             :velocity velo
                             :channel chan)))
      (:SND_SEQ_EVENT_PITCHBEND
       (let ((value (getf event-data 'calispel:value))
             (chan  (getf event-data 'calispel:channel)))
         (make-mr-midi-event :type :pitchbend
                             :velocity value
                             :channel chan))))))

(let ((midi-thread nil) (midi-seq nil) (in-port nil) (out-port nil) (midi-send-lock (bt:make-lock)))
  (defun restart-midi ()
    ;; stop
    (when in-port (cl-alsaseq:close-port (cffi:mem-ref midi-seq :pointer) in-port))
    (when out-port (cl-alsaseq:close-port (cffi:mem-ref midi-seq :pointer) out-port))
    (when midi-seq (cl-alsaseq:close-seq (cffi:mem-ref midi-seq :pointer)))
    (when (and (not (null midi-thread)) (bordeaux-threads:threadp midi-thread) (bordeaux-threads:thread-alive-p midi-thread))
      (bordeaux-threads:destroy-thread midi-thread))

    ;; start
    (setf midi-seq (cl-alsaseq:open-seq "CL-Launchpad")))

  (defun start-midi-reader (event-handler)
    (setf in-port (cl-alsaseq:open-port "CL-Launchpad" (cffi:mem-ref midi-seq :pointer) :input))
    (setf midi-thread 
          (bordeaux-threads:make-thread
            (lambda ()
              (loop
                (handler-case
                  (let ((msg (alsa-to-my-midi (cl-alsaseq:recv (cffi:mem-ref midi-seq :pointer)))))
                    (when msg (funcall event-handler msg)))
                  (error (c) (format t "ERROR: ~a~%" c))))))))

  (defun start-midi-writer ()
    (setf out-port (cl-alsaseq:open-port "CL-Launchpad" (cffi:mem-ref midi-seq :pointer) :output)))

  (defun midi-note-on (note &optional (velo 127) (chan 0))
    (when out-port
      (bt:with-lock-held (midi-send-lock)
                         (cl-alsaseq:send-note velo note chan :SND_SEQ_EVENT_NOTEON
                                               (cffi:mem-ref midi-seq :pointer) out-port))))

  (defun midi-note-off (note &optional (velo 0) (chan 0))
    (when out-port
      (bt:with-lock-held (midi-send-lock)
                         (cl-alsaseq:send-note velo note chan :SND_SEQ_EVENT_NOTEOFF
                                               (cffi:mem-ref midi-seq :pointer) out-port)))))

(defun init-midi (event-handler)
  (restart-midi)
  (start-midi-reader event-handler)
  (start-midi-writer))

(let ((sc-started nil))
  (defun sc-init (&optional (port 57110))
    (when (not sc-started)
      (setf sc-started t)
      (setf *sc-plugin-paths* nil)
      (setf *s* (make-external-server "localhost" :port port
                                      :server-options (make-server-options :realtime-mem-size (* 65536 4))))
      (when (null (all-running-servers))
        (server-boot *s*)
        (jack-connect)

        (defsynth
          sample-dur ((buffer 0) (rate 1) (start 0) (amp 0.5) (out 0) (loop 0) (gate 1) (dur 60) (attack 0.001) (release 0.1))
          (let* ((sig (play-buf.ar 2 buffer (* rate (buf-rate-scale.ir buffer))
                                   :start-pos (* start (/ (buf-frames.ir buffer) (buf-channels.ir buffer)))
                                   :loop loop
                                   :act :free))
                 (sig (* sig (env-gen.kr (linen attack (- dur attack release) release) :act :free)
                         (env-gen.kr (adsr 0.0001 0.0001 1 release) :gate gate :act :free))))
            (out.ar out (* amp sig)) ))))))

;;;

(defstruct padspec
  synth
  toggle
  off-color
  on-color
  (amp-sensitive nil)
  instance)

(def
  *active-color*     3
  *drumloops-color*  5
  *fx-color*         18
  *noise-color*      14
  *oneshot-color*    40
  *fn-on-color*      18
  *fn-off-color*     5)

(def *pad-map* (hshm))

(defun dispatch-play (padspec note velo channel)
  (case (padspec-toggle padspec)
    (:once
      (apply #'synth (if (padspec-amp-sensitive padspec)
                       (append (padspec-synth padspec) [:amp (/ velo 127)])
                       (padspec-synth padspec))))
    (:gate
      (setf (padspec-instance padspec)
            (apply #'synth (if (padspec-amp-sensitive padspec)
                             (append (padspec-synth padspec) [:amp (/ velo 127)])
                             (padspec-synth padspec)))))
    (:mono
      (csnt note (apply #'synth
                        (if (padspec-amp-sensitive padspec)
                          (append (padspec-synth padspec) [:amp (/ velo 127)])
                          (padspec-synth padspec)))))
    (:function
      (funcall (padspec-synth padspec) padspec note (/ velo 127))))
  (when (not (functionp (padspec-synth padspec)))
    (midi-note-on note (padspec-on-color padspec))))

(defun dispatch-stop (padspec note velo channel)
  (if (not (functionp (padspec-synth padspec)))
    ;; synth, stop it
    (progn
      (let ((s (padspec-instance padspec)))
        (when s
          (when (is-playing-p s) (release s))
          (setf (padspec-instance padspec) nil)))
      (midi-note-on note (padspec-off-color padspec)))
    ;; function, call it again
    (funcall (padspec-synth padspec) padspec note 0)))

(defun play-pad (note velo channel)
  (let ((padspec (href *pad-map* note)))
    (if (and padspec (> velo 0))
      (dispatch-play padspec note velo channel)
      (dispatch-stop padspec note velo channel))))

(defun event-handler (msg)
  (when msg
    (case (mr-midi-event-type msg)
      ((:note_on :note_off)
       (midi-note-on (mr-midi-event-note msg) (mr-midi-event-velocity msg) (mr-midi-event-channel msg))))))

(defun pad-layout (mappings)
  (loop :for file :in mappings
        :do (setf (gethash (getf :note file) *pad-map*)
                  (make-padspec :synth (list 'sample-dur
                                             :buffer (cbuf (getf file :file))
                                             :attack 0.007
                                             :release (getf file :release 0.1)
                                             :amp (getf file :amp 1/2)
                                             :rate (getf file :rate 1))
                                :toggle (getf file :toggle :once)
                                :on-color 3
                                :off-color (getf file :color 40)
                                :amp-sensitive (getf file :sensitive t)))))

(init-midi 'event-handler)

(init-midi (lambda (msg) (format t "~a~%" msg)))

; 36 37 38 39 68 69 70 71
; 40 41 42 43 72 73 74 75
; 44 45 46 47 76 77 78 79
; 48 49 50 51 80 81 82 83
; 52 53 54 55 84 85 86 87
; 56 57 58 59 88 89 90 91
; 60 61 62 63 92 93 94 95
; 64 65 66 67 96 97 98 99

