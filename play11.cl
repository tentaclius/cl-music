(require "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :play (:use :cl :sc :mylisp :midi-looper))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)

;;;

(proxy :drone
       (-<> (midicps 52)
            (dyn-klang.ar [(mapcar (λ(f) (* f <>)) [1 2 3 4 3/2])
                           [(range (sin-osc.kr 0.1) 0.3 0.8)
                            (range (sin-osc.kr 0.11) 0.2 0.5)
                            (range (sin-osc.kr 0.12) 0.2 0.5)
                            (range (sin-osc.kr 0.13) 0.1 0.4)
                            (range (sin-osc.kr 0.09) 0.01 0.3)
                            ]])
            (* 0.2 (line.kr 0 1 10))
            pan2.ar))

(release :drone)

(defsynth ssw ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.001) (d 0.2) (s 0.7) (r 0.001))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (saw.ar fq)
         (lpf.ar (* 7 fq))
         (+ (* 1/3 (sin-osc.ar fq)))
         (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(proxy :ssw-fx
       (-<> (abus-in :ssw)
            (+ <> (* 1/3 (allpass-n.ar <> 4 0.12 2)))
            ):pos :tail)
(defpattern ssw
  (play-note 'ssw
             :release t
             :attr [:amp 0.4 :dur 1/10 :q 1 :depth 100 :out (abus :ssw)]
             :note-fn (λ(n) [:freq (midicps (+ 52 -12 (sc *pentatonic* n)))]))
  (λ(i)
    (per-beat i
              ;(seql (euclidian 5 8 0 2))
              ;(seql (euclidian 5 8 0 3))
              ;(seql (euclidian 5 8 0 4))
              ;(seql (euclidian 5 8 0 5))
              ;(seql (euclidian 3 8 1 4))
              ;(seql (euclidian 3 8 1 4))
              ;(seql (euclidian 3 8 2 5))
              ;(seql (euclidian 3 8 2 5))
              (seq 3 0 -1 0 5 4 0 0)
              (seq 0 4 5 0 0 4 0 0)
              (seq (1+ (random 5)) 0 3 0 0 0 1 1)
              (seq 0 0 4 0 1 0 2 0)
              )))

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil)
          (d ['bd :amp 0.7 :dur 0.125])
          (h ['hh :amp 0.3 :dur 0.05])
          (s ['snare :d 0.15 :amp 0.6]))
      (sim (seq o s)
           ;(seql (euclidian 3 8 h ['hh :amp 0.04]))
           ;(seql (loop :repeat 8 :collect h))
           (per-beat i
             (seq d d)
             (seq d d)
             )))))

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil)
          (d ['bd :amp 0.7 :dur 0.0805])
          (h ['hh :amp 0.3 :dur 0.05])
          (s ['snare :d 0.1 :amp 0.6]))
      (sim (per-beat i
             (seq d d)
             )))))

(ssw :start)
(ssw :stop 4)

(drums :start)
(drums :stop)

(release :drone)

(stop)
