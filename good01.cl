(require "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :play (:use :cl :sc :mylisp :midi-looper))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)

(bss :stop)
(let ((fq (midicps (+ 54 -12 ))))
  (proxy :drone
       (-<> (+ (sin-osc.ar fq 0 0.3)
               (sin-osc.ar (* 2 fq) 0 (var-lag.kr (lf-noise0.kr 5)))
               (sin-osc.ar (* 3 fq) 0 (var-lag.kr (lf-noise0.kr 5)))
               (saw.ar (+ (* fq 6) (range (var-lag.kr (lf-noise0.kr 4)) -8 5)) 0.1)
               (-> (saw.ar (+ fq (range (var-lag.kr (lf-noise0.kr 4)) -4 4)) 0.4)
                   (lpf.ar (* fq 10)))
               )
            (* 0.4)
            pan2.ar
            greyhole.ar
            )))

(release :drone)

(proxy :ssnp-fx
       (-<> (abus-in :ssnp)
            (+ <> (* 1/8 (comb-n.ar <> 2 0.261 10)))
            ;greyhole.ar
            (rlpf.ar (range (sin-osc.kr 0.1) 400 1100) 0.07)
            (* (line.kr 0 1 10))
            ;(* (line.kr 1 0 10))
            ) :pos :tail)
(regpattern :ssnp
  (play-note 'ssaw
             :release t
             :attr [:dur 1/2 :a 0.001 :r 0.01 :amp 0.3 :out (abus :ssnp)]
             :note-fn (λ(n) [:freq (midicps (+ 54 (sc *pentatonic* n)))]))
  (λ(i)
    (sim
      (seq 5 0 2 0 1 (random 8))
      (per-beat i
                (seq -5 -4)
                (seq -3 -2 -1)))))

(pstart :ssnp)
(pstop :ssnp)

(proxy :dist-fx
       (-> (abus-in :dist)
           ) :pos :tail)
(regpattern :drums
  (play-drum)
  (λ(i)
    (let ((o nil)
          (d ['bd :dur 0.101])
          (b ['bd :dur 0.065 :amp 0.1])
          (h ['hh :amp 0.1 :freq 4000 :dur 0.04])
          (s ['snare :amp 0.41 :freq 900 :d 0.125 :out (abus :dist)]))
      (sim
        ;(seq o s)
        ;(seq o h o (per-beat i (seq h) (seq h h)))
        ;(per-beat i
        ;  (seq d b o d b b))
        (per-beat i
                  (seq d d)
                  (seq d d)
                  (seq d d)
                  (seq d o (seq d b) o))
        ))))

(pstart :drums)
(pstop :drums)

(regpattern :bss
  (play-note 'fm-bass
             :release t
             :attr [:q 1 :depth 100]
             :note-fn (λ(n) [:freq (midicps (+ 54 -24 (sc *minor* n)))]))
  (λ(i)
    (writeln i)
    (per-beat i
      (seq [0 :dur 1.9])
      (seq 0)
      (seq 2)
      (seq 3)
      ))
  2)

(pstart :bss)
(pstop :bss)

(progn
  (at-beat (clock-quant 1) (release :drone))
  (bss :start))

(release :drone)

(stop)
