(require "mylisp" "init.cl")
(defpackage :play (:use :cl :sc :mylisp :sc-extensions))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(bpm 60)
(metro-start)

;;;;

(let ((midi-attr [:d 0 :s 1 :r 1/2 :amp 0.06]))
  (flet ((to-midi (n) (midicps (+ 50 (sc *pentatonic* n)))))
    (defun m-note-fn (e)
      (append 
        (if (listp e)
            (append [:freq (midicps (+ 50 (to-midi (car e))))] (cdr e))
            [:freq (to-midi e)])
        midi-attr))))

;;;;

(metro-add
  :snd
  (λ(i)
    (ev-map 'm-note-fn
            (SA [:fn (m-play-synth 'ssin) :note-len 2/3]
                (per-beat i
                          (S 0 (1+ (mod i 3)) 5 0 3 (+ 3 (random 5))))))))

(metro-add
  :beat
  (SA [:fn (m-play-drum)]
      '(bd :amp 0.2)  '(bd :amp 0.013) '(bd :amp 0.02)
      '(bd :amp 0.15) '(bd :amp 0.013) '(bd :amp 0.02)))

(metro-add
  :snare
  (SA [:fn (m-play-drum)]
      'snare))

(proxy :drone (with-controls ((f (midicps (- 50 24))))
    (-<> (dyn-klang.ar [[f (* f 2) (* f 3) (* f 5) (* f 7) (* f 8) (* f 9) (* f 10)]])
         (bpf.ar (range (sin-osc.kr 0.1) (- (* f 2) 20) (+ (* f 10) 20)) 0.08)
         (* 0.3)
         (* 0.6 (env-gen.kr (adsr 0.002 0.1 0.6 0.4) :gate (lf-pulse.ar 6 0 0.1)))
         ;(freeverb.ar :room 0.7 :mix 0.7)
         pan2.ar)))

(metro-add :beat)
(metro-add :snare)
(release :drone)
(metro-add :snd)
