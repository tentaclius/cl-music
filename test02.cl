(require "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :play (:use :cl :sc :mylisp :midi-looper))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)

;;;;

(defsynth ssin ((freq 440) (freq0 440) (amp 0.6)
                           (out 0) (gate 1))
  (let ((fq freq))
    (-<> (sin-osc.ar (+ fq (* 100 (sin-osc.ar (* 7 fq)))))
         ;(sin-osc.ar (* 2 fq))
         (lpf.ar (* fq 4))
         (* amp (env-gen.kr (adsr 0.04 0.1 0.3 0.4) :gate gate :act :free))
         (compander.ar (abus-in :drums-chan) 0.1 1 0.01 0.01 0.2)
         (out.ar out <>)
         )))

(defpattern test
  (play-note 'ssin :release t
             :note-fn (λ(n) [:freq (midicps (+ 50 (sc *minor* n)))]))
  (λ(i)
    (per-beat i
      (seq 0 0 0 0 0 (rand-el 0 0 1 4 3 6) 0 1)
      (seq 0 0 (random 5) 0 2 0 3 1)
      )) 2)

(test :start)
(test :stop)

(proxy :drums-proxy
       (-<> (abus-in :drums-chan)
            (* 1))
       :pos :tail)
(defpattern drums
  (play-drum :out (abus :drums-chan) :amp 0.8)
  (λ(i)
    (let ((o nil)
          (b ['bd])
          (h ['hh])
          (hh (seq 'hh 'hh))
          (s ['snare]))
      (sim (seq o hh o hh)
           (seq o s)
           (seq b b)
           ))))

(drums :start)
(drums :stop)

(proxy :puls
       (-<> (pulse.ar (midicps (- 50 12)) (range (sin-osc.kr 1) 0.1 0.8))
            (compander.ar (abus-in :drums-chan) 0.2 1 0.01 0.01 0.2)
            (lpf.ar 300)
            (* 0.4)
            pan2.ar))

(release :puls)

;;;;

(defsynth snth 
  ((freq 440) (freq-q 1) (slide 0) (amp 0.3)
   (out 0) (gate 1) (a 0.01) (d 0.2) (r 0.3)
   (detune 0) (width 0.5) (sub 0) (saw 0)
   (fiq 100) (fa 0.01) (fd 0.1) (fr 0.2)
   (lpf 11000) (res 1))
  (let ((fq (x-line.kr (* freq-q freq) freq slide)))
    (-<> (pulse.ar fq width) (* 2/3)
         (+ (* 1/3 (pulse.ar (+ fq detune) width)))
         (+ (* saw (saw.ar fq)))
         (lpf.ar (+ lpf (* fiq (env-gen.kr (linen fa fd fr)))) res)
         (+ (* sub (sin-osc.ar fq)))
         (* amp (env-gen.kr (linen a d r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))
;
(defpattern pls
  (play-note 'snth :release nil
             :attr [:a 0.007 :d 0.03 :r 0.2 :detune 0 :width 0.5 :sub 0.5 :saw 0.7
                       :lpf 220 :fiq 2100 :fa 0.001 :fd 0.01 :fr 0.2]
             :note-fn (λ(n) [:freq (midicps (+ 54 -24 (sc *pentatonic* n)))]))
  (λ(i)
    (seq 4 3 2 1 0)))

(pls :start)
(pls :stop)

;;;;

(stop)

;;;;

(ql:quickload :yason)

(def str "[{a:1, b:2}]")

(def p (yason:parse str))

(href (first p) "a")

;;;;

(defgeneric seq-schedule (ss snth-fn start duration))
(defmethod seq-schedule ((ss mylisp::%seq) (snth-fn function) (start number) (duration number))
  (when-let* ((seq (seq-data ss))
              (delta-t (and (not (null seq)) (/ duration (length seq)))))
             (loop :for el :in seq :for j :from 0 :do
                   (let ((tm (+ start (* j delta-t))))
                     (seq-schedule el snth-fn tm delta-t)))))

(defmethod seq-schedule ((ss mylisp::%sim) (snth-fn function) (start number) (duration number))
  (loop :for el :in seq :do
        (seq-schedule el snth-fn start duration)))

(defmethod seq-schedule ((ss number) (synth-fn function) (start number) (duration number))
  (at-beat start (funcall synth-fn start duration ss)))

(seq-schedule (seq 1 2 3)
              (λ(b d e) (at-beat b (synth 'ssin (midicps (+ 50 (sc *pentatonic* e))))))
              (1+ (clock-beats))
              1)

(1+ (clock-beats))
