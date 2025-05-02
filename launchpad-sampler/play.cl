(ql:quickload :arrow-macros)
(load "../clseqs/all.lisp")
(load "./launchpad-samples.cl")
(defpackage :play (:use :cl :sc :clseqs :arrow-macros :cl-launchpad) (:shadowing-import-from :cl-launchpad #:sc-init))
(in-package :play)
;
(sc-init)
(clock-bpm 30)
(metro-start)
(named-readtables:in-readtable :sc)
(init-synths)
(midi-init)
(def _ nil)

(defsynth
  sawww ((freq 440) (dur 1) (a 0.001) (r 1) (amp 0.5) (out 0) (gate 1))
  (-<> (+ freq (* (sin-osc.kr 5) (line.kr 0.001 3 dur)))
       (saw.ar)
       (* amp
          (env-gen.kr (env [0 1 0.6 0.6 0] [a 0.1 dur r]) :act :free)
          (env-gen.kr (asr 0 1 r) :gate gate :act :free))
       (lpf.ar (* freq 5))
       (pan2.ar)
       (out.ar out <>)))
;
(defsynth
  chord ((freq 440) (dur 1) (a 0.01) (r 1) (amp 0.5) (out 0) (gate 1))
  (-<> (+ (sin-osc.ar freq)
          (sin-osc.ar (* freq 1.5))
          (sin-osc.ar (* freq 2)))
       (* 1/3)
       (* amp
          (env-gen.kr (env [0 1 0.6 0.6 0] [a 0.1 dur r]) :act :free)
          (env-gen.kr (asr 0 1 r) :gate gate :act :free))
       (pan2.ar)
       (out.ar out <>)))
;
(defun metronome-on ()
  (metro-add
    :metronome
    (SA [:fn (m-play-synth 'metronome :release nil) :attr [:amp 1/8 :out 3]]
        (S C-6 C-5 C-5 C-5))))
;
(defun metronome-off ()
  (metro-add :metronome))

; 64 65 66 67 96 97 98 99
; 60 61 62 63 92 93 94 95
; 56 57 58 59 88 89 90 91
; 52 53 54 55 84 85 86 87
; 48 49 50 51 80 81 82 83
; 44 45 46 47 76 77 78 79
; 40 41 42 43 72 73 74 75
; 36 37 38 39 68 69 70 71

(let ((saw-instance nil))
  (defun make-sawww (note n)
    (make-pad :note note :toggle :function
              :off-color 55 :on-color 32
              :synth (λ(pad note velo)
                       (when (> velo 0)
                         (and saw-instance (is-playing-p saw-instance) (release saw-instance))
                         (setf saw-instance (synth 'sawww :freq (midicps (+ C-2 (sc *pentatonic* n))) :dur 2 :r 0.4 :amp (* velo 1/2))))))))
;;
(let ((chord-instance nil))
  (defun make-sin (note n)
    (make-pad :note note :toggle :function
              :off-color 56 :on-color 33
              :synth (λ(pad note velo)
                       (when (> velo 0)
                         (and chord-instance (is-playing-p chord-instance) (release chord-instance))
                         (setf chord-instance (synth 'chord :freq (midicps (+ C-3 (sc *pentatonic* n))) :dur 2 :r 0.5 :amp (* velo 1/2))))))))
;;
(pad-remap
  [(make-pad :note 75 :file "~/Mus/Samples/selection/HatClosed-Med.wav" :amp 1/4)
   (make-pad :note 70 :file "~/Mus/Samples/selection/SideStick-Hard.wav")
   ;(make-pad :note 71 :synth (λ(pad note velo) (when (> velo 0) (synth 'bd :amp velo))) :toggle :function)
   (make-pad :note 71 :synth '(bd))
   (make-sawww 36 0)
   (make-sawww 40 1)
   (make-sawww 44 2)
   (make-sawww 48 3)
   (make-sawww 52 4)
   (make-sin 37 0)
   (make-sin 41 1)
   (make-sin 45 2)
   (make-sin 49 3)
   (make-sin 53 4)
   (make-pad :note 99 :on-color 60 :off-color 6 :toggle :light)
   (make-pad :note 98 :on-color 69 :off-color 70 :toggle :light)
   (make-pad :note 95 :on-color 60 :off-color 6 :toggle :light)
   (make-pad :note 94 :on-color 69 :off-color 70 :toggle :light)
   (make-pad :note 91 :on-color 60 :off-color 6 :toggle :light)
   (make-pad :note 90 :on-color 69 :off-color 70 :toggle :light)
   (make-pad :note 87 :on-color 60 :off-color 6 :toggle :light)
   (make-pad :note 86 :on-color 69 :off-color 70 :toggle :light)
   (make-pad :note 64 :synth (λ(p n v) (metronome-on)) :toggle :light)
   (make-pad :note 65 :synth (λ(p n v) (metronome-off)) :toggle :light)
   ])
