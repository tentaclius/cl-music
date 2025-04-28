(ql:quickload :cl-collider)
(ql:quickload :cl-alsaseq)

(defpackage :cl-launchpad (:use :cl :sc))
(in-package :cl-launchpad)

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

(defun midi-init ()
  (restart-midi)
  (start-midi-reader 'midi-event-handler)
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
          sample-dur ((buffer 0) (rate 1) (start 0) (gain 0.5) (out 0) (loop 0) (gate 1) (dur 60) (attack 0.001) (release 0.1))
          (let* ((sig (play-buf.ar 2 buffer (* rate (buf-rate-scale.ir buffer))
                                   :start-pos (* start (/ (buf-frames.ir buffer) (buf-channels.ir buffer)))
                                   :loop loop
                                   :act :free))
                 (sig (* sig (env-gen.kr (linen attack (- dur attack release) release) :act :free)
                         (env-gen.kr (adsr 0.0001 0.0001 1 release) :gate gate :act :free))))
            (out.ar out (* gain sig)) ))))))

(let ((bufs (make-hash-table :test #'equal)))
  (defun cbuf (path)
    (bufnum (let ((val (gethash path bufs)))
              (or val
                  (setf (gethash path bufs) (buffer-read path)))))))

(defstruct (pad (:constructor create-pad))
  note
  synth
  toggle
  off-color
  on-color
  amp-sensitive
  instance)

(defun make-pad (&key note synth (toggle :oneshot) (off-color 40) (on-color 3) (amp-sensitive t)
                      file (attack 0.007) (release 0.1) (gain 1/2) (rate 1))
  (create-pad :synth (or synth
                         (list 'sample-dur
                               :buffer (cbuf file)
                               :attack attack
                               :release release
                               :gain gain
                               :rate rate))
              :note note
              :toggle toggle
              :on-color on-color
              :off-color off-color
              :amp-sensitive amp-sensitive))

(defun dispatch-play (pad note velo)
  (case (pad-toggle pad)
    (:oneshot
      (apply #'synth (if (pad-amp-sensitive pad)
                       (append (pad-synth pad) (list :gain (/ velo 127)))
                       (pad-synth pad))))
    (:gate
      (setf (pad-instance pad)
            (apply #'synth (if (pad-amp-sensitive pad)
                             (append (pad-synth pad) (list :gain (/ velo 127)))
                             (pad-synth pad)))))
    (:mono
      (let ((p (pad-instance pad)))
        (and p (is-playing-p p)
             (release p)))
      (setf (pad-instance pad)
            (apply #'synth
                   (if (pad-amp-sensitive pad)
                     (append (pad-synth pad) (list :gain (/ velo 127)))
                     (pad-synth pad)))))
    (:function
      (funcall (pad-synth pad) pad note (/ velo 127))))
  (when (pad-on-color pad)
    (midi-note-on note (pad-on-color pad))))

(defun dispatch-stop (pad note)
  (if (not (functionp (pad-synth pad)))
    ;; synth, stop it
    (progn
      (let ((s (pad-instance pad))) (and s (is-playing-p s) (release s)))
      (setf (pad-instance pad) nil)
      (midi-note-on note (pad-off-color pad)))
    ;; function, call it again
    (progn
      (and (pad-off-color pad) (midi-note-on note (pad-off-color pad)))
      (funcall (pad-synth pad) pad note 0))))

(let ((pad-map (make-hash-table)))
  (defun pad-map-redraw ()
    (loop :for n :from 36 :to 100 :do (midi-note-off n))
    (maphash (lambda (note pad) (midi-note-on note (pad-off-color pad)))
             pad-map))
  (defun pad-get (note)
    (gethash note pad-map))
  (defun pad-map (lst)
    (loop :for pad :in lst
          :do (setf (gethash (pad-note pad) pad-map) pad))
    (pad-map-redraw))
  (defun pad-remap (&optional lst)
    (setf pad-map (make-hash-table))
    (pad-map lst)))

(defun play-pad (note velo)
  (let ((pad (pad-get note)))
    (when pad
      (if (and pad (> velo 0))
        (dispatch-play pad note velo)
        (dispatch-stop pad note)))))

(defun midi-event-handler (msg)
  (when msg
    (case (mr-midi-event-type msg)
      ((:note_on :note_off)
       (play-pad (mr-midi-event-note msg) (mr-midi-event-velocity msg))))))

(export '(midi-init sc-init pad-map make-pad pad-remap pad-map-redraw easy-init))

; 36 37 38 39 68 69 70 71
; 40 41 42 43 72 73 74 75
; 44 45 46 47 76 77 78 79
; 48 49 50 51 80 81 82 83
; 52 53 54 55 84 85 86 87
; 56 57 58 59 88 89 90 91
; 60 61 62 63 92 93 94 95
; 64 65 66 67 96 97 98 99
