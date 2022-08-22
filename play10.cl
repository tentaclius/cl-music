(require "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :play (:use :cl :sc :mylisp :midi-looper))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)

(defsynth ssw ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (saw.ar fq)
         (+ (* 1/3 (saw.ar (* fq 3))))
         (+ (* 1/2 (pink-noise.ar)))
         (lpf.ar (* fq (line.kr 10 3 0.2)))
         (* amp (env-gen.kr (perc 0.009 1) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(defpattern bass
  (play-note 'ssw
             :release t
             :attr []
             :note-fn (λ(n) [:freq (midicps (+ 35 0 n))]))
  (λ(i)
    (let ((o nil))
      (per-beat i 
                (seq 0 o o 0)
                (seq o o o o)))))


(defpattern drums
  (play-drum :amp 0.3)
  (λ(i)
    (let ((o nil)
          (h ['hh :dur 0.04])
          (b ['bd :dur 0.05 :freq 500]))
      (sim
        (per-beat i
                  (seq b b)
                  (seq b b)
                  (seq b b)
                  (seq b (seq b b o o)))
        (per-beat i
                  (seq o o o o)
                  (seq o o o o o o h h))))))

(defsynth clear-saw ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.001) (d 0.2) (s 1) (r 0))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (* 1/2 (saw.ar fq))
         (+ (sin-osc.ar fq))
         (lpf.ar (* fq 30))
         (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(defpattern clear-saw
  (play-note 'clear-saw
             :release t
             :attr [:amp 0.06]
             :note-fn (λ(n) [:freq (midicps (+ 35 12 (sc *pentatonic* (+ 0 n))))]))
  (λ(i)
    (per-beat i
              (seq 1 0 -1 -2)
              (seq 0 2 4 5)
              (seq 1 0 -1 5)
              (seq 0 2 0 4)
              )))

(defpattern clear-saw
  (play-note 'clear-saw
             :release t
             :attr [:amp 0.06 :dur 1/12 :r 0.4 :s 0.3 :d 0.25]
             :note-fn (λ(n) [:freq (midicps (+ 35 12 (sc *pentatonic* (+ n 0))))]))
  (λ(i)
    (per-beat i
              (seq 1 0 -1 -2 0 0 2 2)
              (seq 0 2 4 (random 5) 3 5 (random 7) 1)
              (seq 1 0 -1 5 2 2 1 3)
              (seq 0 2 0 4 -1 -1 (random 7) 1)
              )))

(clear-saw :start 4)
(bass :start 4)
(drums :start 4)

(clear-saw :stop 4)
(drums :stop 4)
(bass :stop)

(let ((q (ceiling (clock-beats))))
  (clock-add q (λ() (drums :stop)))
  (clock-add (+ q 2) (λ() (drums :start 4) (clear-saw :start 4))))

(stop)
