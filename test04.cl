(require "mylisp" "init.cl")
(defpackage :play (:use :cl :sc :mylisp))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 70)

(defsynth sawbass ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.008) (d 0.2) (s 0.7) (r 0.5))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (var-saw.ar fq 0 0.4)
         (fold.ar -0.2 0.3)
         (* 3)
         (lpf.ar (* fq (+ 6 (* 20 (env-gen.kr (perc 0.01 0.5))))))
         (+ (* 1/2 (sin-osc.ar fq)))
         ;(+ (* 1/8 (sin-osc.ar (* fq 4))))
         (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(proxy :sawbass-fx
       (-<> (abus-in :sawbass-bus)
            (* 0.3)
            ))

(def mh (mk-midi-reader "CLarp"))
(start-midi-writer mh)

(regpattern :sawbass
  (play-midi mh)
  ;(play-note-attr `(sawbass dur 1/2 amp 0.3 out ,(abus :sawbass-bus)))
  (λ(i)
    (seq-map (λ(n) (A (+ C-3 (sc *pentatonic* (+ 0 n)))
                     :chan 0))
      (S 
        ;(S (A 0 :dur 2) 5 0 0)
        ;(S (A 0 :dur 1) (A 5 :dur 2) 0)
        (S 0 2 0 0)
        (S 0 5 (random 6) 0)
        ;(S 2 2 2 7)
        ;(S 2 2 2 2)
        ;(S 1 1 1 1)
        ;(S -1 4 -1 5)
        ))) 2)

(pstart :sawbass)
(pstop  :sawbass)
(pkill :sawbass)


(regpattern-ch :test
  ((play-note-attr '(saw-bass :dur 1/4 lpf 3 res 0.2))
   (play-note-attr '(sawbass amp 0.1)))
  ( 
    C-2 -
    C-2 C-2
    C-2 -
    C-1 Eb2
    C-2 -
    C-2 D-2
    C-2 -
    C-2 -
    ;
    C-2 -
    C-2 C-2
    C-2 -
    C-1 Eb2
    C-2 -
    C-2 F-3
    C-3 -
    C-2 -
   ) 1/4)

(pstart :test)
(pstop :test)

(regpattern :drums
  (play-drum)
  (λ(i)
    (let ((o nil)
          (b ['bd])
          (h ['hh])
          (s ['snare]))
      (per-beat i
        (seq b b)))))

(pstart :drums)
(pstop :drums)


(regpattern :song
  (play-note-attr '(ssin) :note-fn (λ(n) n) :release nil)
  (λ(i)
      (per-beat i
                (S (λ(b d) (writeln 'a) (pstop :sawbass) (pstart :drums)))
                (S (λ(b d) (writeln 'b) (pstop :drums) (pstart :sawbass)))))
  4)

(pstart :song)
(pstop :song)


(defsynth tk ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (* (blip.ar fq)
            (env-gen.kr (env [0 0 1 1 0] [0.001 0.001 1/8 0.001])))
         ;(+ (* (pulse.ar fq)
         ;      (env-gen.kr (env [0 0 1 1 0] [1/8 0.001 1/8 0.001]))))
         (+ (* (impulse.ar fq)
               (env-gen.kr (env [0 0 1 1 0] [2/8 0.001 1/8 0.001])
                           :act :free)))
         pan2.ar (out.ar out <>))))
(synth 'tk :freq 40 :freq0 220 :slide 1/4 :amp 1)

(stop)

(proxy :test
       (-<> (crackle.ar)
            pan2.ar)
       )
(release :test)


(ql:quickload :incudine)
(in-package :scratch)
(define-vug phasor (freq init)
  (:defaults 1 0)
  (with-samples ((phase init)
                 (inc (* freq *sample-duration*)))
    (prog1 phase
      (incf phase inc)
      (cond ((>= phase 1.0) (decf phase))
            ((minusp phase) (incf phase))))))

(proxy :test
       (-<> (saw.ar 330)
            (lambda (sig)
              (+ (rlpf.ar sig 330)
                 (bpf.ar sig 330 9.1)))
            (* 0.2)
            pan2.ar))

(release :test)
