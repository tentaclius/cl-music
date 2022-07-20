;; {{{
(equire "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :play (:use :cl :sc :mylisp :midi-looper))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)
;
(def *mr* (mk-midi-reader "CLlaunchpad"))
(def *midi-channel* 0)
;
(defun light-pad (note color)
  (midi-note-on *mr* note color))
;
(defstruct padspec
  synth
  toggle
  off-color
  on-color
  (amp-sensitive nil)
  instance)
;
(defun toggle-ctl (n ctl)
  (if (> (cbus-get ctl) 0)
      (progn (writeln 't) (cbus-set ctl 0) (light-pad n *fn-off-color*))
      (progn (writeln 'f) (cbus-set ctl 1) (light-pad n *fn-on-color*))))

;;; recording stuff
;; {{{

(defpackage :pad-recorder
  (:use :cl :mylisp :sc)
  (:import-from :play :light-pad))
(in-package :pad-recorder)

(defsynth recorder ((bus 1) (buffer 0) (dur 15))
  (record-buf.ar (delay-n.ar (in.ar bus 2) 1 0.003)
                 buffer
                 :run (env-gen.kr (linen 0 dur 0)
                                  :gate (changed.ar (in.ar bus))
                                  :act :free)))

(def
  *sample-rate* 48000
  *record-button* 36
  *color-record-btn-default* 7
  *color-record-btn-active* 3
  *color-record-btn-recording* 20
  *color-pad-recording* 7
  *color-pad-filled* 5)

(def
  *pad-selection-p* nil
  *record-pad* nil
  *record-synth* nil
  *buffer-map* (make-hash-table)
  *synth-map* (make-hash-table)
  *master-bus* nil)

(defun pad-selection-mode-on ()
  (writeln 'pad-selection-mode-on)
  (setf *pad-selection-p* t)
  (light-pad *record-button* *color-record-btn-active*))

(defun pad-selection-mode-off ()
  (writeln 'pad-selection-mode-off)
  (setf *pad-selection-p* nil)
  (light-pad *record-button* *color-record-btn-default*))

(defun start-recording (pad-num)
  (writeln "REC:" pad-num)
  (setf *record-pad* pad-num)
  (when (is-playing-p *record-synth*) (free *record-synth*))
  (hset *buffer-map* pad-num (buffer-alloc (* 15 *sample-rate*) :chanls 2))
  (setf *record-synth* (synth 'recorder :bus *master-bus* :buffer (href *buffer-map* pad-num)))
  (light-pad pad-num *color-pad-recording*))

(defun stop-recording ()
  (writeln 'stop-recording)
  (when (is-playing-p *record-synth*) (free *record-synth*))
  (light-pad *record-button* *color-record-btn-default*)
  (light-pad *record-pad* *color-pad-filled*)
  (setf *record-synth* nil)
  (setf *record-pad* nil))

(defun init (bus)
  (setf *master-bus* bus)
  (light-pad *record-button* *color-record-btn-default*))

(defun dispatch (event)
  (writeln 'dispatch)
  (let ((pad (mylisp:mr-midi-event-note event))
        (velo (mylisp:mr-midi-event-velocity event)))
    (cond
      ;; stop recording
      ((and *record-synth* (> velo 0) (= pad *record-button*))
       (stop-recording)
       (writeln 'end)
       t)
      ;; record target selection button pressed
      ((and (= *record-button* pad) (> velo 0))
       (pad-selection-mode-on)
       (writeln 'end)
       t)
      ;; record target selection button released
      ((and (= *record-button* pad) (= velo 0))
       (pad-selection-mode-off)
       (writeln 'end)
       t)
      ;; start recording on the selected pad
      ((and *pad-selection-p* (> velo 0))
       (start-recording pad)
       (pad-selection-mode-off)
       (writeln 'end)
       t)
      ;; play recorded buffer
      ((> velo 0)
       (writeln "PLAY:" pad)
       (when-let
         ((buffer (href *buffer-map* pad)))
         (hset *synth-map* pad (synth 'sample :buffer buffer :dur 15 :attack 0.003 :release 0.1 :amp 1))
         (writeln 'end)
         t))
      ;; stop the player
      ((= velo 0)
       (writeln "STOP:" pad)
       (when-let
         ((snt (href *synth-map* pad)))
         (release snt)
         (hset *synth-map* pad nil)))
      ;; otherwise, process the event yourself
      (t (writeln 'end) nil))))

(export '(init dispatch))

(in-package :play)

;; }}}

(proxy :mixer
       (-<> (abus-in :master)
            (progn
              (out.ar (abus :record) <>)
              (out.ar 0 <>))
            ) :pos :tail)

; 64 65 66 67  96 97 98 99
; 60 61 62 63  92 93 94 95
; 56 57 58 59  88 89 90 91
; 52 53 54 55  84 85 86 87
; 48 49 50 51  80 81 82 83
; 44 45 46 47  76 77 78 79
; 40 41 42 43  72 73 74 75
; 36 37 38 39  68 69 70 71
(def
  *active-color*     3
  *drumloops-color*  5
  *fx-color*         18
  *noise-color*      14
  *oneshot-color*    40
  *fn-on-color*      18
  *fn-off-color*     5
  )
(labels ((s (synth &optional (toggle :gate) (off-color 0) (on-color *active-color*) (amp-sensitive nil))
           (make-padspec :synth synth :toggle toggle :off-color off-color :on-color on-color :amp-sensitive amp-sensitive)))
  (def *pad-map*
    (hshm
      39 (s ['bd :out (abus :master)])
      )))

;; MIDI mappings

(defun midi-map (msg)
  (let ((note (mr-midi-event-note msg))
        (velo (mr-midi-event-velocity msg))
        (chan (mr-midi-event-channel msg)))
    (when (not (pad-recorder:dispatch msg))
      (case (mr-midi-event-type msg)
        (:note_on
         (when-let
           ((padspec (href *pad-map* note)))
           (if (> velo 0)
               ;; (velo > 0) start a synth
               (progn
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
                    (setf (padspec-instance padspec)
                          (csnt note (apply #'synth
                                            (if (padspec-amp-sensitive padspec)
                                                (append (padspec-synth padspec) [:amp (/ velo 127)])
                                                (padspec-synth padspec))))))
                   (:function
                    (funcall (padspec-synth padspec) padspec note (/ velo 127))))
                 (when (not (functionp (padspec-synth padspec)))
                   (light-pad note (padspec-on-color padspec))))
               ;; (velo = 0) stop the synth
               (if (not (functionp (padspec-synth padspec)))
                   ;; synth, stop it
                   (progn
                     (when-let ((s (padspec-instance padspec)))
                               (when (is-playing-p s) (release s))
                               (setf (padspec-instance padspec) nil))
                     (light-pad note (padspec-off-color padspec)))
                   ;; function, call it again
                   (funcall (padspec-synth padspec) padspec note 0)
                 ))))
        (:note_off)
        (:control)))))

(start-midi-reader *mr* 'midi-map)
(pad-recorder:init (abus :master))

;; }}}

(cl-alsaseq:send-note 127 60 0 :SND_SEQ_EVENT_NOTEON
           (midi-reader-seq mr) (midi-reader-out-port mr))

(cl-alsaseq:send-note 127 60 0 :SND_SEQ_EVENT_NOTEON
           (midi-reader-seq *mr*) (midi-reader-out-port *mr*))

(light-pad 30 127)

*mr*

(stop)
