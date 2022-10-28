(load "lib.cl")
(in-package :sc-user)
(ql:quickload :ltk)
(use-package :sc-extensions)
(use-package :bdef)
(named-readtables:in-readtable :sc)
(init)   ;; start new server
(bpm 60)

;;;;

(def ab1 (bus-audio :chanls 2))

(proxy :ab1
  (-<> (in.ar ab1 2)
       (rlpf.ar 300)
       ):pos :tail)

(defpattern ssw
  (play-note 'ssaw
             :attr [:out ab1]
             :note-fn (λ(n) [:freq (midicps (+ 40 (sc *chromatic* n)))]))
  (λ(i) 
    (sim
      (once-every i 16 5 (seq nil nil nil [12 :amp 1/4]))
      (per-beat i
                (sim [0 :dur 2] [7 :dur 2])
                (seq nil)
                (sim [4 :dur 2] [11 :dur 2])
                (seq nil)
                (sim [0 :dur 2] [7 :dur 2])
                (seq nil)
                (sim [4 :dur 2] [11 :dur 2])
                (seq nil)
                (sim [0 :dur 2] [7 :dur 2])
                (seq nil)
                (sim [4 :dur 2] [11 :dur 2])
                (seq nil)
                (sim [2 :dur 4] [9 :dur 4])
                nil nil nil
                ))))

(ssw :start 8)
(ssw :stop)


;; Drums

(def bd ['bd :freq 200 :bass 20 :dur 0.09])

(defpattern drums
  (play-drum)
  (λ(i) (sim (seql (loop :repeat 8 :collect ['hh :amp 1/8]))
             (once-every i 4 3 (seq nil (seq nil ['hh :amp 1/2])))
             (per-beat i
                       (seq bd bd)))))

(drums :start)

(drums :stop)
(ssw :stop)

;;;

(stop)
