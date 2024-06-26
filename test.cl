(require "mylisp" "init.cl")
(require "clseqs" "./clseqs/clseqs.cl")
(defpackage :play (:use :cl :sc :mylisp :sc-extensions :clseqs))
(in-package :play)
;
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 100)
(metro-start)

;;;;

(def _ nil)

(metro-add :drums
  (λ(i)
    (SA (list :fn (m-play-drum))
        (per-beat i
                  (S 'bd _ 'snare _)
                  (S _ 'bd 'snare _)
                  (S 'bd _ 'snare _)
                  (S _ 'bd 'snare 'bd)
                  ))))

(metro-add :drums)

(metro-add :bass
  (λ(i)
    (SA (list :fn (m-play-synth 'fm-bass :note-fn (λ(f) (midicps (+ 38 (sc *pentatonic* f)))))
              :note-len 1/2
              :attr (list :q 1 :depth 100 :a 0.01 :d 0.1 :s 0.3 :r 0.3))
        (per-beat i
                  (S 0 1 2)
                  (S 0 (random 3) (random 5))
                  (S (random 6) (random 3) (random 5))
                  (S (random 6) (random 3) (random 5))
                  ))))

(metro-add :bass)

(metro-add :drums
  (SA (list :fn (m-play-drum))
      'bd))

