(load "~/src/mus/lib.cl")
(in-package :sc-user)
(use-package :sc-extensions)
(use-package :bdef)
(named-readtables:in-readtable :sc)
(init)   ;; start new server
(bpm 60)

;;;;

(def drone-bus (bus-audio :chanls 2))
(def bass-bus  (bus-audio :chanls 2))
(def drums-bus (bus-audio :chanls 2))

(proxy :mix
       (+ ;(* (line.kr 0 1 5) (in.ar drone-bus 2))
          (* 1 (in.ar drone-bus 2))
          ;(* (line.kr 0 1 5) (in.ar bass-bus 2))
          (* 1 (in.ar bass-bus 2))
          ;(* (line.kr 0 1 5) (in.ar drums-bus 2))
          (* 1 (in.ar drums-bus 2))
          ) :pos :tail)

(release :mix)

;;;;

(def buf (buffer-read "/home/aerdman/Mus/1.wav"))
(defsynth drone ((out 0) (amp 0.9) (gate 1))
  (-<> (warp1.ar
         1                    ;; number of channels
         (bufnum buf)    ;; busnum of the bus
         (range (var-saw.ar 3) 1/8 1/4)    ;; buffer position (pointer)
         1.01                    ;; frequency scale
         0.1                  ;; grain window size
         -1                   ;; grain envelope buffnum (-1 = built-in)
         16                    ;; number of overlapping windows
         0.1                  ;; amount of randomness to the windowing functions
         4)
       (bpf.ar (exp (range (var-saw.kr 1/8) (log 200) (log 3000))) 0.22)
       (* amp (env-gen.kr (adsr 0.01 0.1 1 1) :act :free :gate gate))
       pan2.ar (out.ar out <>)))

(release drone)
(def drone (synth 'drone :out drone-bus))


(defsynth bd ((freq 330) (bass 40) (dur 0.09) (d 0.3) (out 0) (amp 0.5))
  (-<> (x-line.ar freq bass dur)
       (sin-osc.ar)
       (clip2 1)
       (* amp (env-gen.kr (env [0 0.7 1 0] [0.001 dur d]) :act :free))
       pan2.ar (out.ar out <>)))

(defpattern drums
  (play-drum :out drums-bus)
  (λ(i) (let ((b ['bd :dur 0.001 :amp 1/2]))
          (sim (once-every i 4 0 (seq nil nil nil (seq 'hh 'hh)))
               ;(seq nil ['snare :amp 0.6 :d 0.1])
               (per-beat i
                         (seq b b))
               ))))

(drums :start)
(drums :stop)


(defsynth hbass ((freq 110) (out 0))
  (-<> (saw.ar freq)
       (lpf.ar (x-line.ar 7000 (* freq 3) 1/8))
       (* 0.4 (env-gen.kr (perc 0.001 2) :act :free))
       pan2.ar (out.ar out <>)
       ))

(defsynth ssw ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (lpf-q 3)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> (pulse.ar fq)
              (lpf.ar (* fq lpf-q))
              (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
              pan2.ar (out.ar out <>)))))

(defpattern ssw
  (play-note 'fm-bass
             :attr [:out 0 :q 1 :depth 200 :amp 0.3 :a 0.0001 :r 0.01]
             ;:synth-fn (λ(b d s e) [(apply #'synth (cons s e)) (apply #'synth (cons 'ssaw (mergeplist e [:amp 0.5])))])
             :note-fn (λ(n) [:freq (midicps (+ 55 -19 (sc *pentatonic* n)))]))
  (λ(i)
    (per-beat i
              (seq nil (seq 0 0) (seq nil (once-every i 4 0 0)) (seq 0 1))
              (seq nil 3 nil (seq 0 5))
              )))

(ssw :start)
(ssw :stop)

(ssw :stop)
(drums :stop)

;;;;

(stop)
