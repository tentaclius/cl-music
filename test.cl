(require "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :play (:use :cl :sc :mylisp :midi-looper))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)

(defsynth ticks ((freq 440) (dur 0.1) (out 0) (amp 0.4))
  (-<> (white-noise.ar)
       (bpf.ar freq)
       (* amp (env-gen.kr (linen 0.001 dur 0.001) :act :free))
       pan2.ar (out.ar out <>)))

(defun rep (s n)
  (seql (loop :repeat n :collect s)))

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil)
          (d ['bd :dur 0.1 :amp .4])
          (h ['hh :dur 0.05]))
      (sim o
           (once-every i 4 3 (seq o o (seq h h) o))
           (per-beat i
                     (seq d d))))))

(drums :start 1)
(drums :stop)

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil)
          (c ['ticks :dur 0.004 :freq 1000 :amp 0.1])
          (z ['ticks :dur 0.008 :freq 500 :amp 0.1])
          (d ['bd :dur 0.1 :amp 0.4])
          (k ['bd :dur 0.1 :amp 0.05])
          (s ['snare :d 0.1]))
      (sim (per-beat i
             (seq d d)
             (seq d d)
             (seq d (rep d (rand-el 1 2 4 8 4)))
             (seq d d))
           (seq o s)
           (per-beat i
             (seq c)
             (seq (rep ['ticks :dur (/ (random 10) 1000) :freq (+ 400 (random 1000))]
                       (random 10))
                  (rep z (random 10))
                  ))))))

(drums :start 8)
(drums :stop)

;; nice!
(proxy :pulse-bass-fx
       (-<> (abus-in :pulse-bass)
            (+ <> (* 1/4 (comb-n.ar <> 2 0.19 2)))
            ;(* 60)
            ;(fold 0.2 1)
            ;(/ 10)
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

(dur (clock-beats) 1/6 ['pulse-bass :freq 50 :amp 1])

(defpattern puls
  (play-note 'pulse-bass
             :release t
             :attr []
             :note-fn (λ(n) [:freq (midicps (+ 54 -12 (sc *pentatonic* n)))]))
  (λ(i)
    (per-beat i 
              (seq 0 2 3 (random 8))
              (seq 0 1 (random 4) -1))))

(puls :start)
(puls :stop)

(defpattern drums
  (play-drum :amp 0.81)
  (λ(i)
    (let ((o nil))
      (sim (once-every i 2 0 (seq o o (seq 'hh 'hh) o))
           ;(seq o 'snare)
           (per-beat i
             (seq 'bd 'bd))))))

(drums :start)
(drums :stop)

(release :test)

(defpattern bass
  (play-note 'pulse-bass
             :attr [:amp 0.60 :dur 1/14 :a 0.009 :d 0.008 :r .5 :lpf 7]
             :note-fn (λ(n) [:freq (midicps (+ 60 -24 (sc *minor* n)))]))
  (λ(i)
    (per-beat i
              (seq 0 2 0 0)
              (seq 1 0 3 4)
              (seq 0 2 0 3)
              (seq (random 5) (random 5) 0 0)
              )))

(bass :start)
(bass :stop)

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil))
      (sim (seq o 'snare)
           (per-beat i
             (seq 'bd 'bd))))))

(drums :start)
(drums :stop)

(defpattern ssin
  (play-note 'ssin
             :release t
             :attr [:r 1 :amp 0.3]
             :note-fn (λ(n) [:freq (midicps (+ 60 (sc *minor* n)))]))
  (λ(i)
     (per-beat i
      (seq (sim 0 2 4 6) nil nil (sim 0 2 5))
      (seq (sim 0 2 4) nil nil)
      (seq nil nil nil (sim 0 3 4))
      nil
      )))

(ssin :start)
(ssin :stop)

(defpattern bass
  (play-note 'pulse-bass
             :release t
             :attr [:out (abus :feedback)]
             :note-fn (λ(n) [:freq (midicps (+ 60 -24 (sc *pentatonic* n)))]))
  (λ(i)
    (per-beat i
              (seq 0 0 0 0)
              (seq 0 0 0 0)
              (seq 0 0 0 0)
              (seq 0 0 0 0)
              (seq 2 2 2 2)
              (seq 2 2 2 2)
              (seq 3 3 3 3)
              (seq 1 1 1 1)
              )))

(bass :start)
(bass :stop)

(defpattern ssin
  (play-note 'pulse-bass
             :release t
             :attr [:out (abus :feedback)]
             :note-fn (λ(n) [:freq (midicps (+ 54 -12 (sc *pentatonic* n)))]))
  (λ(i)
    (per-beat i
              (seq 0 nil 1 2)
              (seq nil 0 4 nil)
              (seq 0 6 1 2)
              (seq nil 0 4 5)
              )))

(ssin :start 8)
(ssin :stop)

(cbus-set :fb 20)
(cbus-set :clip 0.5)
(cbus-set :lpf 300)

(proxy :feedback-eff
       (-<> (abus-in :feedback)
            ;(clip (- 0 (cbus-in :clip)) (cbus-in :clip))
            (lpf.ar (cbus-in :lpf))
            (+ <> (* 1/10 (comb-n.ar <> 2 0.13 (cbus-in :fb))))
            )
       :pos :tail)

;;;

(def *service* (make-instance 'calispel:channel))
(spawn ;; functionally pure status service
  (labels ((msg-loop (n)
             (match (calispel:? *service*)
                    (:inc (msg-loop (1+ n)))
                    (:dec (msg-loop (1- n)))
                    ((list :get ch) (calispel:! ch n) (msg-loop n))
                    (:get (writeln n) (msg-loop n))
                    ((list :set x) (msg-loop x))
                    (t (msg-loop n)))))
    (msg-loop 0)))

(def ch (make-instance 'calispel:channel))

(calispel:! *service* (list :get ch))
(calispel:? ch)

(calispel:! *service* :inc)

(match :a (:b 1))

(calispel:! *chan* 42)

;;;

(proxy :test
       (-<> (saw.ar 220)
            (+ (saw.ar (+ 220 (* 3.13 (sin-osc.kr 4)))))
            (* 1/3)
            pan2.ar))

(release :test)

;;;

(defsynth recorder ((bus 1) (buffer 0) (attack 0.001) (release 0.1) (dur 5))
  (record-buf.ar (in.ar bus)
                 buffer
                 :run (env-gen.kr (linen 0 dur 0)
                                  :gate (changed.ar (in.ar bus))
                                  :act :free)))

(def sample-rate 48000)
(def buf (buffer-alloc (* 70 sample-rate)))

(def bus (bus-audio))

(proxy :test
       (-<> (sin-osc.ar)
            pan2.ar
            (* (env-gen.kr (adsr 0.1 0.1 0.3 0.1)
                           :gate (trig.kr (changed.ar (in.ar bus)) 1)
                           ))))

(synth 'bd :out bus :dur 1.1)

(synth 'bd :dur 2)

(synth 'recorder :bus bus :buffer buf :dur 2)

(synth 'sample-dur-1 :buffer buf :dur 5)

(server-query-all-nodes)

(stop)
