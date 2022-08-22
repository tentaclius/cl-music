(load "~/src/cl-music/lib.cl")
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

(def *port-map* (hshm 114 :drums
                      18 :drn1
                      19 :saw
                      16 :drone))

(cbus-set :drums 127)
(cbus-set :drums-trg 1)
(cbus-set :drn1 127)
(cbus-set :drn1-trg 1)
(cbus-set :drone 127)
(cbus-set :drone-trg 1)

(proxy :mixer
       (+ (* (in.kr (cbus :drums-trg)) (/ (in.kr (cbus :drums)) 127) (in.ar (abus :drums) 2))
          (* (in.kr (cbus :drn1-trg)) (/ (in.kr (cbus :drn1)) 127) (in.ar (abus :drn1) 2))
          (* (in.kr (cbus :saw-trg)) (/ (in.kr (cbus :saw)) 127) (in.ar (abus :saw) 2))
          (* (in.kr (cbus :drone-trg)) (/ (in.kr (cbus :drone)) 127) (in.ar (abus :drone) 2))
          ) :pos :tail)

;; drums
(def b1 ['bd :freq 200 :bass 30 :dur 0.02 :amp 0.7])
(def b2 ['bd :freq 50 :bass 10 :dur 0.03 :amp 0.6])
(def h1 ['hh :dur 0.04 :amp 0.1])
(def h2 ['hh :dur 0.1 :amp 0.2])

(defpattern drums
  (play-drum :out (abus :drums))
  (λ(i)
    (sim 
         (seql (euclidian 4 6 h1 'hh))
         (once-every i 2 1
                     (seq nil nil nil h2))
         (per-beat i
                   (seq b1 b1)
                   nil
                   (seq b1 b1 b1)
                   (seq b1 nil (seq b1 b2)) )
         )))

(drums :start)
(drums :stop)

(defsynth inst1 ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> (sin-osc.ar freq)
              (+ (* 1/2 (sin-osc.ar (* 4 freq)) (range (sin-osc.kr 6) 0.1 1)))
              ;(+ (* 1/3 (sin-osc.ar (* 3 freq))))
              (+ (* 1/7 (sin-osc.ar (* 8 freq))))
              (+ <> (* 1/20 (comb-l.ar <> 0.6 0.2 1)))
              (* 1/3 amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
              pan2.ar (out.ar out <>)))))

(proxy :out1
  (-<> (in.ar (abus :out1) 2)
       (fold (- 0 (/ (in.kr (cbus :fold)) 127)) (/ (in.kr (cbus :fold)) 127))
       (rlpf.ar (range (sin-osc.kr 0.1) 200 1000))
       (freeverb.ar :room 0.7 :mix (line.kr 0.7 0 7))
       (out.ar (abus :drn1) <>)
       ):pos :tail)

(defpattern drn1
  (play-note 'inst1
             :attr [:out 0 :amp 0.241 :q 1 :depth 400 :a 0.0001]
             :note-fn (λ(n) [:freq (midicps (+ *root* -12 (my-scale (+ n 0))))]))
  (λ(i) (per-beat i
                  (seq 0 4 6 2 7 6)
                  (seql (mapcar (λ(x) (+ x 2)) [0 4 6 2 7 6]))
                  (seql (mapcar (λ(x) (+ x 0)) [0 4 6 2 7 6]))
                  (seql (mapcar (λ(x) (+ x 5)) [0 4 6 2 7 6]))
                  )))

(drn1 :start)
(drn1 :stop)

(defpattern ssw
  (play-note 'fm-bass
             :attr [:out 0 :amp 0.15 :lpf 5 :q 1 :depth 200]
             :note-fn (λ(x) [:freq (fq (+ x -7))]))
  (λ(i) (per-beat i
                  (seq 4 3 0)
                  (seq [0 :freq0 (fq 10) :slide 1/4] [0 :freq0 (fq -5) :slide 1/4] 0)
                  (seq 3 [1 :freq0 (fq -10) :slide 1/8])
                  (seq 7 1 2 8 9)
                  )))

(ssw :start)
(ssw :stop)

(let ((f0 (fq -7)))
  (proxy :drone
         (-<> (* 1/2 (sin-osc.ar f0))
              (+ (* 1/3 (sin-osc.ar (* f0 2)) (range (sin-osc.kr 1 (* -1/2 pi)) 0.1 0.9)))
              (+ (* 1/4 (sin-osc.ar (* f0 3)) (range (sin-osc.kr 0.11 (* -1/2 pi)) 0.1 0.9)))
              ;(+ (* 1/5 (sin-osc.ar (* f0 4)) (range (sin-osc.kr 0.12 (* -1/2 pi) 0.1 0.9)))
              (+ (* 1/7 (sin-osc.ar (* f0 8)) (range (sin-osc.kr 0.13 (* -1/2 pi)) 0.1 0.9)))
              (* 0.40)
              ;(* (line.kr 1 0 5))
              ;(* (line.kr 0 1 5))
              pan2.ar
              (out.ar (abus :drone) <>)
              )))

(release :drone)
(drn1 :stop)
(ssw :stop)
(drums :stop)

(drums :stop 1)

;;;;

(stop)
