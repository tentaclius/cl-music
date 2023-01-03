(require "mylisp" "init.cl")
(defpackage :play (:use :cl :sc :mylisp))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)

(proxy :sins-px
       (-<> (abus-in :sins-out)
            (+ <> (* 1/3 (comb-n.ar <> 3 0.14 3)))
            (compander.ar (abus-in :drums-out) 0.1 1 0.01 0.01 0.2)
            (* 0.5)
            ;(* (line.kr 0 1 15))
            ) :pos :tail)

(regpattern :sins
  (play-note 'fm-bass
             :attr [:amp 0.2 :r 0.01 :a 0.006 :out (abus :sins-out)
                         :q 3 :depth 100]
             :note-fn (λ(n) [:freq (midicps (+ 50 (sc *pentatonic* n)))]))
  (λ(i) 
    (let ((s1 (seq 0 1 2 (per-beat i 1 5 7 5) (per-beat i 6 4 3) (per-beat i 8 0 -1 3 6))))
      (sim (seq-map (λ(e) (+ 0 e)) s1)
           (list (seq-map (λ(e) (+ 2 e)) s1) :start 1/3)
           )))
  1)

(pstart :sins 4)
(pstop :sins)

(proxy :drums-px
       (-<> (abus-in :drums-out)
            ) :pos :tail)

(regpattern :drums
       (play-drum :amp 0.6 :out (abus :drums-out))
       (λ(i) (sim (seq 'bd nil (seq 'bd (once-every i 4 0 'bd)) nil)
                  ;(seq nil 'hh nil 'hh)
                  ;(seq nil (seq 'hh (once-every i 4 0 'hh)) nil 'hh)
                  )))

(pstart :drums)
(pstop :drums)

(proxy :drone
       (-<> (+ (-<> (pulse.ar (midicps (- 50 24)) (range (sin-osc.kr 0.1) 0.1 0.45))
                    (rlpf.ar (range (sin-osc.kr 0.11) 500 1100) 0.1)
                    (* 0.4))
               (-<> (pulse.ar (midicps (- 50 5)) (range (sin-osc.kr 0.13) 0.1 0.45))
                    (rlpf.ar (range (sin-osc.kr 0.11) 500 1100) 0.071)
                    (* 0.2)
                    (delay-n.ar 0.5 0.5)))
            (+ (* 0.3 (sin-osc.ar (midicps (- 50 24)))))
            (* 0.5)
            (* (line.kr 0 1 10))
            pan2.ar
            greyhole.ar
            ))

(release :drone)

(proxy :bass-px
  (-<> (abus-in :bass-chan)
       (compander.ar (abus-in :drums-out) 0.2 1 0.01 0.01 0.2)
       ) :pos :tail)
(regpattern :bass
  (play-note 'saw-bass :attr [:out (abus :bass-chan) :amp 0.3 :lpf 2 :a 0.006 :r 0.7 :s 0.6 :d 0.2]
             :note-fn (λ(n) [:freq (midicps (+ 50 -12 0 (sc *pentatonic* n)))]))
  (λ(i)
    (seq [0 :amp 0.7] 0 0 [0 :amp 0.7] (random 3) 0)) 1/2)

(pstart :bass 4)
(pstop :bass)

(clock-add (clock-quant 4)
           (λ() (pstop :sins)
                (pstop :bass)
                (pstart :drums 1)
                (writeln 'dbg)))

(pstop :bass)
(pstart :sins 4)

(pstop :sins)
(pstop :drums 4)

;;;;

(stop)

(proxy :test
       (let ((fq 110))
         (-<> (pulse.ar fq)
              (* (range (pulse.ar (* fq 1/2)) 0 1))
              (lpf.ar (* 3 fq))
              (* 0.1)
              pan2.ar)))

(release :test)

;;;;

(defsynth noize1 ((hpf 10) (bpf 0) (bpf-r 1)
                           (a 0.01) (d 1/8) (r 0.01))
       (-<> (brown-noise.ar)
            (+ (pulse.ar 70))
            pan2.ar
            (* 20)
            (clip.ar -1/2 1/2)
            (hpf.ar hpf)
            (if~ (> bpf 0) (bpf.ar <> bpf bpf-r) <>)
            (* (env-gen.kr (linen a d r) :act :free))
            (out.ar 0 <>)))

(synth 'noize1 :bpf 400 :bpf-r 1)

(proxy :drums-px
  (-<> (abus-in :drums-chan)
       ) :pos :tail)
(regpattern :drums
  (play-drum :out (abus :drums-chan))
  (λ(i)
    (let ((n1 ['noize1 :d 1/20 :bpf 300 :bpf-r 0.2])
          (n2 ['noize1 :hpf 1800 :d 1/20]))
      (per-beat i
        (seql (rotate (euclidian 3 4 n1 n2)
                      (random 5))
              (list 'noize2 'noize2))))))

(pstart :drums)
(pstop :drums)

(defsynth noize2 ((f0 100) (f1 600) (dur 1/16) (a 0.01) (r 0.1))
  (-<> (x-line.ar f0 f1 dur)
       (sin-osc.ar)
       (* (env-gen.kr (linen a (- dur a) (/ r 2)) :act :free))
       pan2.ar (out.ar 0 <>)))
(synth 'noize2)

(cbus-set :fq 80)
(proxy :test
       (let ((sig  (pan2.ar
                     (* (pulse.ar 440)
                        (env-gen.kr (linen 0.1 1/2 0.1))))))
         (if~ 0 (lpf.ar sig 210) sig))
       :fade 0)

(stop)
