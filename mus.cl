(load "lib.cl")
(in-package :sc-user)
(use-package :sc-extensions)
(use-package :bdef)
(named-readtables:in-readtable :sc)
(init)   ;; start new server
(bpm 60)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *root* 54)
(defun my-scale (n) (sc *minor* n))
(defun fq (n) (midicps (+ *root* (my-scale n))))

(defsynth ssn1 ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> (sin-osc.ar fq)
              (+ (* 1/20 (sin-osc.ar (* fq 2))))
              (+ (* 1/30 (sin-osc.ar (* fq 3))))
              (* amp (env-gen.kr (perc a d)))
              (+ (* 1/15 (saw.ar fq) (* amp (env-gen.kr (perc a (/ d 3))))))
              (+ <> (* 1/5 (comb-l.ar <> 0.6 0.2 1)))
              (* (env-gen.kr (perc a (* d 50)) :act :free))
              pan2.ar (out.ar out <>)))))

(release s1)
(def s1 (synth 'ssn1 :freq 80 :a 0.001 :d 1))

(def *root* 0)

;; drums

(def b1 ['bd :freq 100 :bass 10 :dur 0.05 :amp 1])
(def h1 ['hh :dur 0.04 :amp 0.1])
(def h2 ['hh :dur 0.1 :amp 0.2])

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
             ;(seq (euclidian 4 6 h1 'hh))
             ;(per-beat i
             ;          (seq nil h1)
             ;          (seq nil (seq h1 h2)))
             (once-every i 2 0
                         (seq nil nil nil h2))
             (per-beat i
                       (seq b1 b1)))))

(drums :start)
(drums :stop)

(defsynth inst1 ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> (sin-osc.ar freq)
              ;(+ (* 1/3 (sin-osc.ar (* 3 freq))))
              ;(+ (* 1/7 (sin-osc.ar (* 6 freq))))
              (+ (* 1/2 (sin-osc.ar (* 4 freq)) (range (sin-osc.kr 6) 0.1 1)))
              (+ <> (* 1/20 (comb-l.ar <> 0.6 0.2 1)))
              (* 1/3 amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
              pan2.ar (out.ar out <>)))))

(def out1 (bus-audio :chanls 2))

(proxy :out1
  (-<> (in.ar out1 2)
       (rlpf.ar (range (sin-osc.kr 0.1) 200 1000))
       (freeverb.ar :room 0.7 :mix (line.kr 0.6 0 7))
       (* (line.kr 1 0 6))
       ) :pos :tail)

(defpattern drn1 :inf
  (play-note 'inst1
             :attr [:out out1 :amp 0.241 :q 1 :depth 400 :a 0.0001]
             :note-fn (λ(n) [:freq (midicps (+ *root* -12 (my-scale (+ n 0))))]))
  (λ(i) (per-beat i
                  (seq [0 4 6 2 7 6])
                  (seq (mapcar (λ(x) (+ x 2)) [0 4 6 2 7 6]))
                  (seq (mapcar (λ(x) (+ x 0)) [0 4 6 2 7 6]))
                  (seq (mapcar (λ(x) (+ x 5)) [0 4 6 2 7 6]))
                  )))

(drn1 :start)
(drn1 :stop)

(defpattern ssw :inf
  (play-note 'ssaw
             :attr [:amp 0.08]
             :note-fn (f_ [:freq (fq (+ _ 0))]))
  (λ(i) (per-beat i
                  (seq 4 3 0)
                  (seq [[0 :freq0 (fq 10) :slide 1/4] [0 :freq0 (fq -5) :slide 1/4] 0])
                  (seq 3 [1 :freq0 (fq -10) :slide 1/8])
                  (seq 7 1 2 8 9)
                  )))

(ssw :start)
(ssw :stop)

(let ((f0 (fq -7)))
  (proxy :drone
         (-<> (* 1/2 (sin-osc.ar f0))
              (+ (* 1/3 (sin-osc.ar (* f0 2)) (range (sin-osc.kr 0.10) 0.1 0.9)))
              ;(+ (* 1/4 (sin-osc.ar (* f0 3)) (range (sin-osc.kr 0.11) 0.1 0.9)))
              (+ (* 1/5 (sin-osc.ar (* f0 4)) (range (sin-osc.kr 0.12) 0.1 0.9)))
              (+ (* 1/7 (sin-osc.ar (* f0 7)) (range (sin-osc.kr 0.13) 0.1 0.9)))
              (* 0.14)
              (* (line.kr 1 0 5))
              pan2.ar
              )))

;;;;

(stop)
