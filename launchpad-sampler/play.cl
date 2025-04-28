(ql:quickload :arrow-macros)
(load "../clseqs/all.lisp")
(load "./launchpad-samples.cl")
(defpackage :play (:use :cl :sc :clseqs :arrow-macros :cl-launchpad) (:shadowing-import-from :cl-launchpad #:sc-init))
(in-package :play)

(sc-init)
(clock-bpm 60)
(metro-start)
(named-readtables:in-readtable :sc)
(init-synths)
(midi-init)
(def _ nil)

(defsynth
  sawww ((freq 440) (dur 1) (a 0.001) (r 1) (gain 0.5) (out 0) (gate 1))
  (-<> (saw.ar freq)
       (* gain
          (env-gen.kr (env [0 1 1 0] [a dur r]) :act :free)
          (env-gen.kr (asr 0 1 r) :gate gate :act :free))
       (lpf.ar (* freq 4))
       (out.ar out <>)))

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
                         (setf saw-instance (synth 'sawww :freq (midicps (+ C-2 (sc *pentatonic* n))) :dur 1 :r 0.4 :gain 1)))))))
;;
(pad-remap
  [(make-pad :note 75 :file "~/Mus/Samples/selection/HatClosed-Med.wav")
   (make-pad :note 70 :file "~/Mus/Samples/selection/SideStick-Hard.wav")
   (make-pad :note 71 :synth (λ(pad note velo) (when (> velo 0) (synth 'bd :amp 1))) :toggle :function)
   (make-sawww 36 0)
   (make-sawww 40 1)
   (make-sawww 44 2)
   (make-sawww 48 3)
   (make-sawww 52 4)])
