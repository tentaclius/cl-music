(require "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :play (:use :cl :sc :mylisp :midi-looper))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)

(proxy :ding-fx
       (-<> (abus-in :ding)
            ;(+ (* 0.03 (saw.ar (range (var-lag.kr (lf-noise0.kr 4)) 218 222))))
            (greyhole.ar)
            ) :pos :tail)
(defsynth ding ((freq 440) (amp 0.7) (gate 1) (ifreq 1)
                           (a 0.004) (r 0.5))
  (let ((igate (sin-osc.kr ifreq)))
    (-<> (sin-osc.ar freq)
         (* amp (env-gen.kr (perc a r) :gate igate))
         (+ (* amp 0.007 (env-gen.kr (perc 0.001 0.2) :gate igate)
               (white-noise.ar)))
         (* (env-gen.kr (adsr 0.001 0.001 1 0.1) :gate gate :act :free))
         pan2.ar (abus-out :ding <>))))

(spawn
  (nsynth :ding1 'ding :ifreq 1/2 :freq (midicps 52))
  (sleep (* 1/10 (random 10)))
  (nsynth :ding2 'ding :ifreq 2/3 :freq (midicps 64))
  (sleep (* 1/10 (random 10)))
  (nsynth :ding3 'ding :ifreq 3/7 :freq (midicps 68))
  (sleep (* 1/10 (random 10)))
  (nsynth :ding4 'ding :ifreq 3/5 :freq (midicps 69)))

(nsynth :ding1)
(nsynth :ding2)
(nsynth :ding3)
(nsynth :ding4)

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil)
          (b ['bd :dur 0.04])
          (h ['hh :dur 0.05])
          (g ['hh :dur 0.03 :freq 11000])
          (s ['snare :d 0.1]))
      (sim (seq b b)
           ;(seq o s)
           ;(once-every i 8 1 (seq o o (seq o b) o))
           ;(seql (rotate (euclidian 3 8 h g) (random 4)))
           ))))

(drums :start)
(drums :stop)

;; DRONE
(let ((fq (midicps (- 54 12))))
  (proxy :drone
       (with-controls ((fq fq))
         (-<> (mix [(sin-osc.ar fq 0 0.9)
                  (sin-osc.ar (* 2 fq) 0 (var-lag.kr (lf-noise0.kr 5)))
                  (sin-osc.ar (* 3 fq) 0 (var-lag.kr (lf-noise0.kr 5)))
                  ;(saw.ar (+ (* fq 3) (range (var-lag.kr (lf-noise0.kr 4)) -8 5)) 0.4)
                  (-> (saw.ar (+ (* 2/3 fq) (range (var-lag.kr (lf-noise0.kr 4)) -2 2)) 0.7)
                      (lpf.ar (* 2/3 fq 7)))
                  ])
            (* 0.1)
            greyhole.ar
            splay.ar
            ))))

(release :drone)

(def drone-fqs (gen-list [(- 54 12) (- 54 10) (- 54 9) (- 54 10)]))

(defpattern drun
  (play-drum)
  (λ(i)
    (when (zerop (mod i 4))
      (at-beat (clock-quant 1) (ctrl :drone :fq (midicps (funcall drone-fqs)))))
    (let ((o nil))
      (sim (per-beat i
                     nil
             ;(seq 'hh 'hh)
             )))))

(drun :start)
(drun :stop)

(stop)
