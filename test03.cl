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
                         :q 1 :depth 100]
             :note-fn (λ(n) [:freq (midicps (+ 50 (sc *pentatonic* n)))]))
  (λ(i) 
    (let ((s1 (seq 0 1 2 (per-beat i 1 5 7 5) (per-beat i 6 4 3) (per-beat i 8 0 -1 3 6))))
      (sim (seq-map (λ(e) (+ 0 e)) s1)
           (A (seq-map (λ(e) (+ 2 e)) s1)
              :start 1/3)
           )))
  1)

(pstart :sins 4)
(pstop :sins)

(proxy :drums-px
       (-<> (abus-in :drums-out)
            ) :pos :tail)

(regpattern :drums
       (play-drum :amp 0.6 :out (abus :drums-out))
       (let ((o nil)
             (s ['snare :amp 0.5 :d 0.09]))
         (λ(i) (sim (seq 'bd nil (seq 'bd (once-every i 4 0 'bd)) nil)
                  ;(seq nil 'hh nil 'hh)
                  ;(seq nil (seq 'hh (once-every i 4 0 'hh)) nil 'hh)
                  ))))

(pstart :drums)
(pstop :drums)

(proxy :drone
       (-<> (+ (-<> (pulse.ar (midicps (- C-3 24)) (range (sin-osc.kr 0.1) 0.1 0.45))
                    (rlpf.ar (range (sin-osc.kr 0.11) 500 1100) 0.1)
                    (* 0.4))
               (-<> (pulse.ar (midicps (- C-3 5)) (range (sin-osc.kr 0.13) 0.1 0.45))
                    (rlpf.ar (range (sin-osc.kr 0.11) 500 1100) 0.071)
                    (* 0.2)
                    (delay-n.ar 0.5 0.5)))
            (+ (* 0.3 (sin-osc.ar (midicps (- 50 24)))))
            ;(* 0.5)
            (* (x-line.kr 0.0001 1 2))
            pan2.ar
            greyhole.ar
            ))

(release :drone)

(proxy :bass-px
  (-<> (abus-in :bass-chan)
       (compander.ar (abus-in :drums-out) 0.2 1 0.01 0.01 0.2)
       ) :pos :tail)
(regpattern :bass
  (play-note 'saw-bass :attr [:out (abus :bass-chan) :amp 0.3 :lpf 2 :a 0.007 :r 0.1 :s 0.6 :d 0.2]
             :note-fn (λ(n) [:freq (midicps (+ 50 -12 0 (sc *pentatonic* n)))]))
  (λ(i)
    (seq [0 :amp 0.7] 0 0 [0 :amp 0.7] (random 3) 0)))

(pstop :drums)
(clock-add (clock-quant 4)
           (λ() (pstop :sins)
                (pstart :bass)
                (release :drone)
                (pstart :drums 1)
                (writeln 'dbg)))

(pstop :bass)
(pstart :sins 4)

(pstop :sins)
(pstop :drums 4)

(pstart :sins)

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
              (list 'noize2 '(noize2 :f1 2000)))))))

(pstart :drums)
(pstop :drums)

(defsynth noize2 ((f0 100) (f1 800) (dur 1/32) (a 0.01) (r 0.1))
  (-<> (x-line.ar f0 f1 dur)
       (sin-osc.ar)
       (* (env-gen.kr (linen a (- dur a) (/ r 2)) :act :free))
       pan2.ar (out.ar 0 <>)))
(synth 'noize2)

;;;

(regpattern :test
  (play-note 'ssaw :attr [:lpf 2 :res 0.8])
  (λ(i)
    (seq-map (λ(n) (+ C-3 (sc *pentatonic* n)))
      (sim [(per-beat i
                      0 0 0 0 2 2 2 2 1 1 1 1 3 3 3 3
                      0 0 0 0 2 2 2 2 1 1 1 1 2 2 2 2)
            :a 0.2 :s 0 :d 4 :lpf 1]
           (seq 0 0 1 3 3 2)))))

(pstart :test)
(pstop :test)

(defsynth snare ((freq 1800) (amp 0.3)
              (out 0) (gate 1)
              (a 0.001) (d 0.2))
  (-<> (white-noise.ar)
       (+ (pulse.ar 60))
       (lpf.ar freq)
       ;(* (sin-osc.kr 6))
       (* amp (env-gen.kr (perc a d) :gate gate :act :free))
       pan2.ar (out.ar out <>)))

(clock-bpm 90)
(regpattern :drums
  (play-drum)
  (λ(i)
    (let ((- nil)
          (b ['bd])
          (h ['hh])
          (s ['snare])
          (k ['bd :dur 0.01 :d 0.01 :freq 1800]))
      (sim
        (seq h (per-beat i h h  (seq h h)) h h)
        (per-beat i
                (S b - s -)
                (S - b s (once-every i 8 7 s)))
        ))))

(pstart :drums)
(pstop :drums)

(regpattern :bass
  (play-note 'fm-bass :attr [:q 1 :depth 130])
  (let ((_ nil))
    (λ(i)
      (seq-map (λ(n) (when n (+ C-2 (sc *pentatonic* n))))
               (per-beat i
                         (S 0 _ 0 _)
                         (S _ 0 (per-beat i 5 0 5) 0)
                         )))))

(pstart :bass)
(pstop :bass)

(release :drone)

(proxy :test
       (let ((k (range (sin-osc.kr 1) 0 1)))
         (-<> (white-noise.ar)
              (rlpf.ar (x-line.ar 10000 10 1/4) 0.2)
              pan2.ar)))

(release :test)

(proxy :test
       (let ((e (env-gen.kr (adsr 0.1 0.1 0.5 0.5) :gate (cbus-in :gate))))
         (-> (sin-osc.ar (* e 440))
             pan2.ar)))

(cbus-set :gate 1)
(cbus-set :gate 0)

;;;

(defmacro regpattern-ch (name chnls data &optional (spd 1))
  (let ((i-sym (gensym)))
   `(regpattern ,name
                (play-channels ,@chnls)
                (lambda (,i-sym) (cons :step (per-beat-n (length (list ,@chnls)) ,i-sym ,@data)))
                ,spd)))

(regpattern-ch :test
  ((play-note-attr '(ssin))
   (play-drum))
  ;
  (C-2 'bd
   nil 'hh
   G-2 'hh
   D-2 'hh
   )
  1/4)

(pstart :test)

(pstop :test)

(def
  - nil
  b '(bd)
  h '(hh)
  s '(snare))
(regpattern-ch :test
   ((play-drum)
    (play-drum)
    (play-note 'ssin)
    (play-note 'ssaw :attr '(:r 0.01)))
   ;
   ( b   -   C-2 (A C-2 :amp 1)
     -   -   A-2 C-2
     h   s   A-2 -
     -   -   A-2 -
     b   -   A-2 -
     -   s   C-3 C-2
     h   -   C-2 -
     h   -   F-2 -
     )
   1/8)

(regpattern-ch :test
               ((play-drum)
                (play-note-attr '(ssin :amp 0.1 :r 0.001)))
               ;
               ( b (A C-4 :a 0.001 :dur 1/8 :r 0.1 :amp 1/2)
                 - (A C-4 :dur 1/2 :r 1)))

(pstart :test)
(pstop :test)


;;;;

(defsynth sdrum ((f0 440) (f1 20) (dur 0.1))
  (let ((fq (x-line.kr f0 f1 slide)))
    (-<> (pulse.ar fq)
         (* amp (env-gen.kr (perc 0.001 (* dur 2)) :act :free))
         pan2.ar (out.ar out <>))))

(stop)
