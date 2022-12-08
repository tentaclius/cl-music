(require "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :play (:use :cl :sc :mylisp :midi-looper))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)


(proxy :drone
       (let ((root (midicps (- 54 24))))
         (-<> (+ (saw.ar root)
                 (* 1/4 (pulse.ar (+ root 1))))
              (* 0.19)
              (+ (* 0.1 (white-noise.ar)))
              (lpf.ar (range (sin-osc.kr 1) 100 200))
              pan2.ar)))

(release :drone)


(defsynth sssw ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (let ((fq (x-line.kr freq0 freq slide))
        (env (env-gen.kr (adsr a d s r) :gate gate :act :free)))
    (-<> (saw.ar fq)
         (+ (* 1/2 (saw.ar (+ 1 fq))))
         (+ (* 1/3 (saw.ar (- 0.7 fq))))
         (lpf.ar (+ 100 (* env 1900)))
         (* 1/2 amp env)
         pan2.ar (out.ar out <>))))

(proxy :sssw-fx
       (-<> (abus-in :sssw)
            (+ <> (* 1/8 (comb-n.ar <> 2 0.14 2)))
            (lpf.ar (range (sin-osc.kr 0.2) 190 700))
            ;greyhole.ar
            freeverb.ar
            ) :pos :tail)

(def g (gen-xrand [1 3 3 4 5 6 6]))

(defpattern sssw
  (play-note 'sssw
             :release t
             :attr [:a 0.001 :d 0.2 :s 0.25 :r 1 :out (abus :sssw)]
             :note-fn (λ(n) [:freq (midicps (+ 54 -12 (sc *pentatonic* n)))]))
  (λ(i)
    (sim (seq 0 1 2 (funcall g))
         )))

(sssw :start 1)
(sssw :stop)

;;;;;

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil)
          (b ['bd :amp 0.4 :bass 70])
          (h ['hh])
          (s ['snare]))
      (sim 
        ;(seq o h s (per-beat i h h h (seq h h)))
        (per-beat i
             (seq b b))))))

(drums :start)
(drums :stop)

(proxy :puls-out
       (-<> (abus-in :puls-fx)
            ;freeverb.ar
            (+ <> (* 1/10 (comb-n.ar <> 2 0.17 5)))
            greyhole.ar
            ) :pos :tail)
(proxy :puls-lpf
       (-<> (sin-osc.kr 0.1)
            (range 1 10)
            (cbus-out :puls-lpf <>)))

(defsynth puls ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (pulse.ar fq)
         (lpf.ar (* fq (cbus-in :puls-lpf)))
         (+ (* 1/3 (saw.ar (* 1.5 fq))))
         (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(defpattern puls
  (play-note 'puls
             :release t
             :attr [:out (abus :puls-fx) :amp 0.4 :dur 1/16 :a 0.001 :r 0.1 :s 1]
             :note-fn (λ(n) [:freq (midicps (+ 54 -12 (sc *pentatonic* (+ n 0))))]))
  (λ(i)
    (seq 0 [1 :prob 1/3] 3 (rand-el [4 :prob 1/2] 6 7)))
  1/2)

(puls :start)
(puls :stop)


(defpattern bass
  (play-note 'saw-bass
             :release t
             :attr [:amp 0.3 :lpf 3 :res 0.2 :a 0.001 :d 0.4 :s 0.6 :r 0.1]
             :note-fn (λ(n) [:freq (midicps (+ 54 -24 (sc *pentatonic* n)))]))
  (λ(i)
    (per-beat i
              (seq 0 0 0 0 0 0 1 0)
              (seq 0 0 0 1 1 1 0 1)
              (seq 0 0 0 0 0 0 1 0)
              (seq 0 2 2 0 2 1 1 2)
              )))

(bass :start)
(bass :stop)


(defpattern drums
  (play-drum)
  (λ(i)
    (let ((_ nil)
          (b ['bd :amp 0.7 :freq 220 :dur 0.10 :d 0.10])
          (p ['bd :amp 0.25])
          (h ['hh])
          (s ['snare :d 0.1 :amp 0.3]))
      (sim 
        (seq _ h _ (per-beat i h h h (seq h h)))
        (seq _ s)
        (per-beat i
                  ;(seq b b)
                  (seq b (seq p) _ _ b (seq p) _ _)
                  )))))

(drums :start)
(drums :stop)


(defpattern ssn
  (play-note 'ssin
             :release t
             :attr [:dur 1/10 :amp 0.17 :a 0.01 :r 1]
             :note-fn (λ(n) [:freq (midicps (+ 54 (sc *pentatonic* n)))]))
  (λ(i)
    (seq [(random 7) :prob 2/3]))
  1/8)

(ssn :start)
(ssn :stop)

;;;

(sssw :start)
(drums :start)
(puls :start)
(ssn :start)
(bass :start)

(let ((q (ceiling (clock-beats))))
  (clock-add (+ q 1 -1/3) (λ() (drums :stop) (puls :stop)))
  (clock-add (+ q 2 -1/3) (λ() (drums :start) (ssn :start))))

(release :drone)
(bass :start)

(puls :stop)
(ssn :start)

(release :drone)

(sssw :stop)
(bass :stop)
(puls :stop)
(drums :stop)
(ssn :stop)

;;;;

(stop)
