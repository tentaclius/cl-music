(load "~/src/cl-music/lib.cl")
(in-package :sc-user)
(named-readtables:in-readtable :sc)
(init)   ;; start new server

(bpm 90)

;;;;

(def snareBuf (buffer-read "~/Mus/samples/snare.wav"))

(synth 'sample-dur-1 :buffer snareBuf :out 0)

(proxy :key-pad-synth-fx
       (-<> (in.ar (abus :key-pad) 2)
            (* (-> 10 sin-osc.ar (range 0.7 1)))
            (+ <> (* 2/3 (allpass-n.ar <> 4 0.17 1)))
            (freeverb.ar)
            ) :pos :tail)
(defsynth keys ((gate 1) (freq 440) (amp 0.5) (out (abus :key-pad)))
  (-<> (dyn-klang.ar [[freq (* freq 2) (* freq 3)] [1/2 1/4 1/8]])
       ;(+ (-> (white-noise.ar) (bpf.ar freq) (* 1/10)))
       (* amp (env-gen.kr (adsr 0.008 0.1 0.8 0.2) :act :free :gate gate))
       pan2.ar (out.ar out <>)))

(defsynth key-pad-synth ((gate 1) (freq 440) (amp 0.5) (out 0))
  (-<> (saw.ar freq)
       (+ (* 1/20 (pulse.ar (* 1/2 freq))))
       (+ (* 1/6 (bpf.ar (brown-noise.ar) freq 0.1)))
       (rlpf.ar (* freq 3) (line.ar 0.01 0.3 0.2))
       (* amp 1/2 (env-gen.kr (adsr 0.002 0.1 0.8 0.2) :act :free :gate gate))
       pan2.ar (out.ar out <>)))

(defsynth kick ((out 0) (amp 1))
  (-<> (x-line.ar 200 70 0.05)
       (sin-osc.ar)
       (+ (lpf.ar (white-noise.ar) 200))
       (* amp (env-gen.kr (perc 0.001 0.2) :act :free))
       pan2.ar (out.ar out <>)))

(defsynth snare ((out 0) (amp 0.5))
  (-<> (x-line.ar 300 70 0.02)
       (sin-osc.ar)
       (+ (lpf.ar (white-noise.ar) 1100))
       (* amp (env-gen.kr (perc 0.001 0.3) :act :free))
       pan2.ar (out.ar out <>)))

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil)
          (k 'kick)
          (s ['sample-dur-1 :buffer snareBuf :amp 0.5]))
      (sim (seql (loop :repeat 4 :collect ['hh :amp (/ (+ 2 (random 10)) 40)]))
           (per-beat i
             (seq k o o o)
             (seq s o k o)
             (seq o o k o)
             (seq o s o o)
             ;
             (seq k o o o)
             (seq s o k k)
             (seq o o k o)
             (seq s k o o)
             )))))

(drums :start)
(drums :stop)

;(defpattern keys
;  (play-note 'key-pad-synth
;             :release t
;             :attr [:amp 0.2]
;             :synth-fn (λ(b d s e)
;                         (let ((disp (/ (random 10) 150)))
;                                     (at-beat (+ b disp) (apply #'synth (cons s e)))))
;             :note-fn (λ(n) [:freq (midicps (- n 0))]))
;  (λ(i)
;    (per-beat i
;              (sim [64 :dur 1/8 :start 0] [62 :dur 2 :start 3/4])
;              nil
;              nil
;              (seq 54)
;              (seq [57 :dur 4/3])
;              (seq nil [52 :dur 4/3])
;              nil
;              nil
;              ;
;              (sim [64 :dur 1/8 :start 0] [62 :dur 2 :start 3/4])
;              nil
;              nil
;              (seq 54)
;              (seq [52 :dur 4/3])
;              (seq nil 54)
;              (seq 57 54)
;              (seq 52 59)
;              )))

(drums :start 4)
(keys :start 4)

(defpattern keys
  (play-note 'keys :attr [:out (abus :key-pad)])
  (λ(i) (per-beat i
          (seq (sim 62 59) nil (sim 54 50) (sim 62 59))
          (seq nil (sim 54 50) (sim 62 59) nil)
          (seq (sim 54 50) (sim 62 59) (sim 54 50) nil)
          (seq (sim 76 73) nil (sim 54 50) nil)
          (seq (sim 57 61) nil (sim 54 50) (sim 57 61))
          (seq nil (sim 54 50) (sim 61 57) nil)
          (seq (sim 50 54) (sim 57 61) (sim 54 50) nil)
          (seq (sim 73 69) nil nil)
          )))

(keys :stop)
(drums :stop)

(defpattern drums1
  (play-drum)
  (λ(i)
    (let ((o nil)
          (k 'kick)
          (s ['sample-dur-1 :buffer snareBuf :amp 0.5])
          (s1 ['sample-dur-1 :buffer snareBuf :amp 0.3])
          (s2 ['sample-dur-1 :buffer snareBuf :amp 0.1])
          (s3 ['sample-dur-1 :buffer snareBuf :amp 0.01])
          )
      (sim (per-beat i
             (seq o (seq k k k) o k)
             (seq s s1 s2 s3))))))

(drums1 :start 1 2)
(drums1 :stop)


(defpattern bass
  (play-note 'keys
             :release t
             :attr [:a 0.001 :q 1 :depth 100 :lpf 3 :out (abus :key-pad)]
             :note-fn (λ(n) [:freq (midicps n)]))
  (λ(i)
    (per-beat i
              (seq 47)
              nil
              (seq 38 41 45 49)
              (seq 50 49 45 41)
              ;
              (seq 38)
              (seq nil nil nil 61)
              (seq 57 nil 54 nil)
              (seq 49 45)
              )))

(keys :start 8)
(bass :start 8)
(drums :start 8)

(keys :stop)
(drums :stop)
(bass :stop)




(stop)
