(require "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :play (:use :cl :sc :mylisp :midi-looper))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)

(proxy :sins-px
       (-<> (abus-in :sins-out)
            (+ <> (* 1/3 (comb-n.ar <> 3 0.17 3)))
            (compander.ar (abus-in :drums-out) 0.1 1 0.01 0.01 0.2)
            ) :pos :tail)
(regpattern :sins
  (play-note 'ssin
             :attr [:amp 0.2 :r 0.1 :a 0.006 :out (abus :sins-out)]
             :note-fn (λ(n) [:freq (midicps (+ 50 (sc *pentatonic* n)))]))
  (λ(i) 
    (let ((s1 (seq 0 1 2 (per-beat i 1 5 7 5) (per-beat i 6 4 3) (per-beat i 8 10 -1 3 6))))
      (sim s1 (seq-map (λ(e) (+ 2 (random 3) e)) s1))))
  1)

(pstart :sins)
(pstop :sins)

(proxy :drums-px
       (-<> (abus-in :drums-out)
            ) :pos :tail)
(regpattern :drums
  (play-drum :amp 0.6 :out (abus :drums-out))
  (λ(i) (sim (seq 'bd 'bd)
             ;(seq nil (rand-el nil 'hh) 'snare)
             ;(seq nil (seq 'hh (once-every i 4 0 'hh)) nil 'hh)
             )))

(pstart :drums)
(pstop :drums)

(proxy :drone
       (-<> (pulse.ar (midicps (- 50 24)) (range (sin-osc.kr 0.1) 0.1 0.45))
            (* (line.kr 0 1 10))
            (rlpf.ar (range (sin-osc.kr 0.11) 500 1100) 0.1)
            (* 0.1)
            pan2.ar))

(release :drone)


(stop)
