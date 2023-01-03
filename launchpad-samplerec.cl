;; {{{
(require "mylisp" "init.cl")
(defpackage :play (:use :cl :sc :mylisp))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)
;
(def *mr* (mk-midi-reader "CLlaunchpad"))
(def *midi-channel* 0)
;
(defun light-pad (pad color)
  (midihelper:send-event (midihelper:ev-noteon *midi-channel* pad color)))
;(let ((mr (start-midi-writer (mk-midi-reader "CLlaunchpad"))))
;  (defun light-pad (note color)
;    (midi-note-on mr note color)))
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
  (record-buf.ar (in.ar bus 2) buffer))

(def
  *sample-rate* 48000
  *record-button* 99
  *color-record-btn-default* 7
  *color-record-btn-active* 3
  *color-record-btn-recording* 20
  *color-pad-recording* 56
  *color-pad-filled* 40
  *color-pad-playing* 3)

(def
  *pad-selection-p* nil
  *record-pad* nil
  *record-synth* nil
  *buffer-map* (make-hash-table)
  *record-buffer* nil
  *synth-map* (make-hash-table)
  *master-bus* nil
  *out-bus* nil)

(defun pad-selection-mode-on ()
  (setf *pad-selection-p* t)
  (light-pad *record-button* *color-record-btn-active*))

(defun pad-selection-mode-off ()
  (setf *pad-selection-p* nil)
  (light-pad *record-button* *color-record-btn-default*))

(defun choose-rec-pad (pad-num)
  (setf *record-pad* pad-num)
  (light-pad pad-num *color-pad-recording*))

(defun start-recording ()
  (when (is-playing-p *record-synth*) (free *record-synth*))
  (setf *record-buffer* (buffer-alloc (* 30 *sample-rate*) :chanls 2))
  (setf *record-synth* (synth 'recorder :bus *master-bus* :buffer *record-buffer* :pos :tail))
  (light-pad *record-pad* *color-pad-recording*))

(defun stop-recording ()
  (when (is-playing-p *record-synth*) (free *record-synth*))
  (hset *buffer-map* *record-pad* *record-buffer*)
  (light-pad *record-button* *color-record-btn-default*)
  (light-pad *record-pad* *color-pad-filled*)
  (setf *record-buffer* nil)
  (setf *record-synth* nil)
  (setf *record-pad* nil))

(defun start-playing (pad velo)
  (declare (ignorable velo))
  (when-let ((buffer (href *buffer-map* pad)))
    (when-let ((snth (href *synth-map* pad))) (when (is-playing-p snth) (release snth)))
    (hset *synth-map* pad (synth 'sample-dur :buffer buffer :out *out-bus*
                                 :dur 15 :attack 0.003 :release 0.1 :amp 1))
    (light-pad pad *color-pad-playing*)
    t))

(defun stop-playing (pad)
  (when-let
    ((snt (href *synth-map* pad)))
    ;(release snt)
    ;(hset *synth-map* pad nil)
    (light-pad pad *color-pad-filled*)
    t))

(defun init (in-bus out-bus)
  (setf *master-bus* in-bus)
  (setf *out-bus* out-bus)
  (loop :for k :being :the :hash-keys :in *buffer-map*
        :do (light-pad k *color-pad-filled*))
  (light-pad *record-button* *color-record-btn-default*))

(defun dispatch (event)
  (let ((pad (mylisp:mr-midi-event-note event))
        (velo (mylisp:mr-midi-event-velocity event)))
    (cond
      ;; stop recording
      ((and *record-pad* (> velo 0) (= pad *record-button*))
       (stop-recording)
       t)
      ;; record target selection button pressed
      ((and (not *record-synth*) (= *record-button* pad) (> velo 0))
       (pad-selection-mode-on)
       t)
      ;; record target selection button released
      ((and (= *record-button* pad) (= velo 0))
       (pad-selection-mode-off)
       t)
      ;; start recording on the selected pad
      ((and *pad-selection-p* (> velo 0))
       (choose-rec-pad pad)
       (pad-selection-mode-off)
       t)
      ;; play recorded buffer
      ((> velo 0)
       (when (and *record-pad* (not *record-synth*)) (start-recording))
       (start-playing pad velo))
      ;; stop the player
      ((= velo 0)
       (stop-playing pad))
      ;; otherwise, process the event yourself
      (t nil))))

(export '(init dispatch))

(in-package :play)

;; }}}

(proxy :mixer
       (-<> (abus-in :master)
            (out.ar (abus :record) <>)
            ) :pos :tail)
(proxy :monitor
       (-<> (abus-in :record)) :pos :tail)

(def
  *active-color*     3
  *passive-color*    16
  )
(def *pad-map* (make-hash-table))
(let ((note-list (list 36 37 38 39  68 69 70 71  40 41 42 43  72 73 74 75
                        44 45 46 47  76 77 78 79  48 49 50 51  80 81 82 83
                        52 53 54 55  84 85 86 87  56 57 58 59  88 89 90 91
                        60 61 62 63  92 93 94 95  64 65 66 67  96 97 98 99))
      (sample-list (list
                     "/home/aerdman/Mus/samples/selection-drums/bass.wav"
                     "/home/aerdman/Mus/samples/selection-drums/bottle.wav"
                     "/home/aerdman/Mus/samples/selection-drums/clap.wav"
                     "/home/aerdman/Mus/samples/selection-drums/cowbell.wav"
                     "/home/aerdman/Mus/samples/selection-drums/crash.wav"
                     "/home/aerdman/Mus/samples/selection-drums/hh-closed.wav"
                     "/home/aerdman/Mus/samples/selection-drums/hh-open.wav"
                     "/home/aerdman/Mus/samples/selection-drums/hh-pedal.wav"
                     "/home/aerdman/Mus/samples/selection-drums/kick-electro-2.wav"
                     "/home/aerdman/Mus/samples/selection-drums/kick-electro.wav"
                     "/home/aerdman/Mus/samples/selection-drums/knock.wav"
                     "/home/aerdman/Mus/samples/selection-drums/maracas.wav"
                     "/home/aerdman/Mus/samples/selection-drums/pop.wav"
                     "/home/aerdman/Mus/samples/selection-drums/shaker.wav"
                     "/home/aerdman/Mus/samples/selection-drums/sleigh.wav"
                     "/home/aerdman/Mus/samples/selection-drums/snap.wav"
                     "/home/aerdman/Mus/samples/selection-drums/snare_dull.wav"
                     "/home/aerdman/Mus/samples/selection-drums/snare-hit.wav"
                     "/home/aerdman/Mus/samples/selection-drums/snare-rim.wav"
                     "/home/aerdman/Mus/samples/selection-drums/snare.wav"
                     "/home/aerdman/Mus/samples/selection-drums/tom-low.wav"
                     "/home/aerdman/Mus/samples/selection-drums/tom-middle.wav"
                     "/home/aerdman/Mus/samples/selection-drums/tom.wav"
                     ))
      ;(sample-list (list
      ;               "~/Mus/samples/selection-ethno/BD0050.WAV"
      ;               "~/Mus/samples/selection-ethno/hangA3.28.wav"
      ;               "~/Mus/samples/selection-ethno/hangD4.24.wav"
      ;               "~/Mus/samples/selection-ethno/hangD#4.24.wav"
      ;               "~/Mus/samples/selection-ethno/hangF#4.26.wav"
      ;               "~/Mus/samples/selection-ethno/hangG4.27.wav"
      ;               "~/Mus/samples/selection-ethno/hangA4.34.wav"
      ;               "~/Mus/samples/selection-ethno/hangA#4.35.wav"
      ;               "~/Mus/samples/selection-ethno/hangC#5.28.wav"
      ;               "~/Mus/samples/selection-ethno/hangD5.31.wav"
      ;               "~/Mus/samples/selection-ethno/tabla10.wav"
      ;               "~/Mus/samples/selection-ethno/tabla11.wav"
      ;               "~/Mus/samples/selection-ethno/tabla12.wav"
      ;               "~/Mus/samples/selection-ethno/tabla13.wav"
      ;               "~/Mus/samples/selection-ethno/tabla14.wav"
      ;               "~/Mus/samples/selection-ethno/tabla1.wav"
      ;               "~/Mus/samples/selection-ethno/tabla3.wav"
      ;               "~/Mus/samples/selection-ethno/tabla4.wav"
      ;               "~/Mus/samples/selection-ethno/tabla5.wav"
      ;               "~/Mus/samples/selection-ethno/tabla7.wav"
      ;               "~/Mus/samples/selection-ethno/tabla8.wav"
      ;               ))
      )
  (loop :for pad :in note-list :for smpl :in sample-list
        :do (hset *pad-map* pad
                  (make-padspec :synth ['sample :out (abus :master) :buffer (cbuf smpl) :attack 0.0001]
                                :toggle :once
                                :off-color *passive-color*
                                :on-color *active-color*
                                :amp-sensitive nil))))

; 64 65 66 67  96 97 98 99
; 60 61 62 63  92 93 94 95
; 56 57 58 59  88 89 90 91
; 52 53 54 55  84 85 86 87
; 48 49 50 51  80 81 82 83
; 44 45 46 47  76 77 78 79
; 40 41 42 43  72 73 74 75
; 36 37 38 39  68 69 70 71

;; MIDI mappings

(defun midi-map (msg)
  (let ((note (mr-midi-event-note msg))
        (velo (mr-midi-event-velocity msg))
        (chan (mr-midi-event-channel msg)))
    (declare (ignorable chan))
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

;; }}}

;(start-midi-reader *mr* 'midi-map)
;(pad-recorder:init (abus :record) (abus :master))

;;;

;; with midihelper
;(pad-recorder:init (abus :record) (abus :master))
(pad-recorder:init (abus :record) 0)
(midihelper:stop-midihelper)
(defun midi-helper-map (messages)
  (dolist (message messages)
    (when-let ((msg (alsa-to-my-midi message)))
              (midi-map msg)
              )))
(midihelper:start-midihelper :master 96 'midi-helper-map)

;;;

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil))
      (sim (per-beat i
             (seq 'hh ['hh :amp 0.2]))))))

(drums :start)
(drums :stop)

;(stop)
