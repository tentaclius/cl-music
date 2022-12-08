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
  (play-note 'fm-bass
             :attr [:amp 0.2 :r 0.01 :a 0.006 :out (abus :sins-out) :q 1 :depth 300]
             :note-fn (λ(n) [:freq (midicps (+ 50 (sc *pentatonic* n)))]))
  (λ(i) 
    (let ((s1 (seq 0 1 2 (per-beat i 1 5 7 5) (per-beat i 6 4 3) (per-beat i 8 0 -1 3 6))))
      (sim (seq-map (λ(e) (list (+ 0 e) :amp (/ 2 (+ 20 (random 20))))) s1)
           (seq-map (λ(e) (list (+ 2 e) :amp (/ 1 (+ 20 (random 20))) :start 1/3)) s1)
           )))
  1)

(pstart :sins 4)
(pstop :sins)

(proxy :drums-px
       (-<> (abus-in :drums-out)
            ) :pos :tail)
(regpattern :drums
  (play-drum :amp 0.6 :out (abus :drums-out))
  (λ(i) (sim (seq 'bd 'bd)
             ;(seq nil (seq 'hh (once-every i 4 0 'hh)) nil 'hh)
             )))

(pstart :drums 4)
(pstop :drums)

(proxy :drone
       (-<> (pulse.ar (midicps (- 50 24)) (range (sin-osc.kr 0.1) 0.1 0.45))
            (rlpf.ar (range (sin-osc.kr 0.11) 500 1100) 0.1)
            (* 0.1)
            (+ (* 0.3 (sin-osc.ar (midicps (- 50 24)))))
            ;(* (line.kr 0 1 10))
            pan2.ar))

(release :drone)

(proxy :test-px
  (-<> (abus-in :test-chan)
       ) :pos :tail)
(regpattern :test
  (play-note 'ssin :attr [:out (abus :test-chan) :amp 0.1]
             :note-fn (λ(n) [:freq (midicps (+ 54 (sc *pentatonic* n)))]))
  (λ(i)
    (sim [(seq 0 1 2 3) :start 1/3 :dur 0.1 :s 0.7 :r 0.1]
         (seq 2 3 4 5))))

(pstart :test)
(pstop :test)

(stop)
