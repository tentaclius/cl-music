(load "~/src/mus/lib.cl")
(in-package :sc-user)
(named-readtables:in-readtable :sc)
(init)   ;; start new server
;(connect)
(bpm 60)

;;;

(proxy :drone (with-controls ((f 110))
    (-<> (dyn-klang.ar [[f (* f 2) (* f 3) (* f 5) (* f 7) (* f 8) (* f 9) (* f 10)]])
         (bpf.ar (range (sin-osc.kr 0.1) (- (* f 2) 20) (+ (* f 10) 20)) 0.08)
         (* 0.2)
         ;(* 0.6 (env-gen.kr (adsr 0.002 0.1 0.6 0.4) :gate (lf-pulse.ar 6 0 0.1)))
         (freeverb.ar :room 0.7 :mix 0.7)
         pan2.ar)))

(release :drone)

(defsynth ssw ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.009) (d 0.1) (s 0.3) (r 0.7))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (saw.ar fq)
         (lpf.ar (* fq 2.5))
         (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(def bass-seq (gen-list (euclidian 11 18 0 5)))

(defpattern ssw
  (play-note 'ssw
             :attr [:a 0.007]
             :note-fn (f_ [:freq (midicps (+ (cpsmidi 55) 0 (sc *pentatonic* _)))]))
  (λ(i)
    (seql (loop :repeat 6 :collect (funcall bass-seq)))
    ))

(ssw :start)
(ssw :stop)


(def drums-fx (bus-audio :chanls 2))

(proxy :drums-fx
       (-<> (in.ar drums-fx 2)
            ;(+ <> (* 1/3 (allpass-n.ar <> 4 0.20 2)))
            ;(fold.ar -0.1 0.1)
            ):pos :tail)

(defsynth noise ((freq 440) (amp 0.3) (out 0) (gate 1) (a 0.0001) (d 0.02) (r 0.0001))
  (-<> (brown-noise.ar)
       (bpf.ar freq)
       (* amp (env-gen.kr (linen a d r) :gate gate :act :free))
       pan2.ar (out.ar out <>)))

(defpattern drums
  (play-drum :out drums-fx)
  (λ(i)
    (let ((o nil)
          (d ['bd :dur 0.02 :freq 220])
          (h ['noise :freq 2000 :d 0.001])
          (b ['noise :freq 100 :d 0.001])
          (s ['snare :d 0.05]))
      (sim (seql (rotate (euclidian 7 12 h b) (random 4)))
           (seq d nil (once-every i 4 0 (seq s s)))
           ))))

(drums :start)
(drums :stop 4)


(defpattern sinosc
  (play-note 'ssin
             :release t
             :attr [:out drums-fx]
             :note-fn (λ(n) [:freq (midicps (+ 54 (sc *pentatonic* n)))]))
  (λ(i)
    (seq 0 0 1 3)))

(sinosc :start)
(sinosc :stop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System

(stop)
