(progn
(require "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :play (:use :cl :sc :mylisp :midi-looper))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60))

;; DRONE
(let ((fq (midicps (- 54 12))))
  (proxy :drone
       (-<> (mix [(sin-osc.ar fq 0 0.9)
                  (sin-osc.ar (* 2 fq) 0 (var-lag.kr (lf-noise0.kr 5)))
                  (sin-osc.ar (* 3 fq) 0 (var-lag.kr (lf-noise0.kr 5)))
                  ;(saw.ar (+ 440 (range (var-lag.kr (lf-noise0.kr 4)) -8 5)) 0.4)
                  (-> (saw.ar (+ (* 2/3 fq) (range (var-lag.kr (lf-noise0.kr 4)) -2 2)) 0.7)
                      (lpf.ar (* 2/3 fq 7)))
                  ])
            (* 0.1)
            greyhole.ar
            splay.ar
            )))

(release :drone)

;; DRUMS
(defpattern drums
  (play-drum :amp 0.81)
  (λ(i)
    (let ((o nil)
          (b ['bd :dur 0.07 :amp 0.5])
          (h ['hh :amp 0.2 :dur 0.04]))
      (sim
        ;(once-every i 2 0 (seq o o (seq h h) o))
        ;(seq o 'snare)
        (per-beat i
          (seq b b))))))

(drums :start)
(drums :stop)

;; BASS
(proxy :pulse-bass-fx
       (-<> (abus-in :pulse-bass)
            ;(+ <> (* 1/4 (comb-n.ar <> 2 0.19 2)))
            (* 60) (fold -1 1) (/ 40)
            freeverb.ar
            ))
(defsynth pulse-bass ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out (abus :pulse-bass)) (gate 1) (lpf 70)
              (a 0.001) (d 0.1) (s 0.1) (r 0.5))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (pulse.ar fq)
         (+ (* 1/2 (sin-osc.ar fq)))
         (+ (* 1/2 (saw.ar (+ fq (* 2 (sin-osc.kr 1.1))))))
         (rlpf.ar (* fq (x-line.kr 20 6 0.1)) 1.19)
         (* 1/2 amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(defpattern puls
  (play-note 'pulse-bass
             :release t
             :attr []
             :note-fn (λ(n) [:freq (midicps (+ 54 -24 (sc *pentatonic* n)))]))
  (λ(i)
    (per-beat i 
              (seq 0 2 3 (random 8))
              (seq 0 1 (random 4) -1))))

(puls :start)
(puls :stop)

(stop)
