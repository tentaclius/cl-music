(proxy :drone (with-controls ((f 110))
    (-<> (dyn-klang.ar [[f (* f 2) (* f 3) (* f 5) (* f 7) (* f 8) (* f 9) (* f 10)]])
         (bpf.ar (range (sin-osc.kr 0.1) (- (* f 2) 20) (+ (* f 10) 20)) 0.08)
         (* 0.2)
         ;(* 0.6 (env-gen.kr (adsr 0.002 0.1 0.6 0.4) :gate (lf-pulse.ar 6 0 0.1)))
         (freeverb.ar :room 0.7 :mix 0.7)
         pan2.ar)))

(ctrl :drone :f 60)

(proxy :drone
       (with-controls ((freq (midicps (+ 54 12))) (amp .3))
         (-<> (* 0.3 (sin-osc.ar freq))
             (+ (* 0.3 (sin-osc.ar (+ 2 freq))))
             (* amp)
             pan2.ar
             )))

(defpattern drone
  (λ(b d e) (when e 
              (let ((nn (float (+ 2 (random 12)))))
                (proxy :drone
                       (-<> (sin-n nn :freq-fn (λ(i) (* (midicps (- 48 (* 12 1))) i)))
                            (bpf.ar (range (sin-osc.kr 0.1) 200 1100) (range (sin-osc.kr 0.3) 0.3 0.6))
                            )))))
  (λ(i)
    (seq 1)))

(proxy :drone
       (with-controls ((drone-f (midicps 52)))
         (-<> (sin-osc.ar drone-f)
              (+ (* 0.4 (sin-osc.ar (* 1.5 drone-f))))
              (+ (* 0.041 (saw.ar (+ (range (sin-osc.kr 6) -8 14) (* 4 drone-f)))))
              (* 0.50)
              pan2.ar)))

(proxy :drone
       (-<> (pulse.ar (midicps 30))
            (bpf.ar 200 0.2)
            pan2.ar
            ))

(proxy :drone
       (let ((f (midicps 52)))
         (-<> (* (sin-osc.ar f) (-> 0.1 sin-osc.kr (range 0.2 1)))
              (+ (-> f (* 2) sin-osc.ar (* 1/2) (* (-> 0.11 sin-osc.kr (range 0.2 1)))))
              (+ (-> f (* 3) sin-osc.ar (* 1/3) (* (-> 0.12 sin-osc.kr (range 0.2 1)))))
              (+ (-> f (* 4) sin-osc.ar (* 1/4) (* (-> 0.13 sin-osc.kr (range 0.2 1)))))
              (+ (-> f (* 5) sin-osc.ar (* 1/5) (* (-> 0.15 sin-osc.kr (range 0.2 1)))))
              (* 1/5)
              pan2.ar)))

(def df 100)
(proxy :drone
       (-<> 0
            (+ (* 1/4 (sin-osc.ar (* df 1)) (range (sin-osc.kr 0.1) 0.1 0.9)))
            (+ (* 1/4 1/2 (sin-osc.ar (* df 2)) (range (sin-osc.kr 0.2) 0.1 0.9)))
            (+ (* 1/4 1/3 (sin-osc.ar (* df 3)) (range (sin-osc.kr 0.3) 0.1 0.9)))
            (+ (* 1/4 1/4 (sin-osc.ar (* df 4)) (range (sin-osc.kr 0.4) 0.1 0.9)))
            (bpf.ar (range (sin-osc.kr 1) 100 2000))
            (* 0.2)
            pan2.ar))

(def ff 110)
(proxy :drone
       (-<> (* [(sin-osc.ar ff) (sin-osc.ar (+ ff 2))] (range (sin-osc.kr 0.1) 0.2 0.9))
            (+ (* 1/2 [(sin-osc.ar (* 2 ff)) (sin-osc.ar (+ (* 2 ff) 1))] (range (sin-osc.kr 0.1) 0.2 0.9)))
            (+ (* 1/3 [(sin-osc.ar (* 3 ff)) (sin-osc.ar (+ (* 3 ff) 1))] (range (sin-osc.kr 0.11) 0.2 0.9)))
            (+ (* 1/4 [(sin-osc.ar (* 4 ff)) (sin-osc.ar (+ (* 4 ff) 1))] (range (sin-osc.kr 0.12) 0.2 0.9)))
            (+ (* 1/5 [(sin-osc.ar (* 5 ff)) (sin-osc.ar (+ (* 5 ff) 1))] (range (sin-osc.kr 0.13) 0.2 0.9)))
            (+ (* 1/7 [(sin-osc.ar (* 6 ff)) (sin-osc.ar (+ (* 8 ff) 1))] (range (sin-osc.kr 0.14) 0.2 0.9)))
            (* (sin-osc.kr 10))
            (* 0.121)))

(def f (midicps 30))
(proxy :drone
       (-<> (* (sin-osc.ar f) (range (sin-osc.kr 23.34) 0.6 0.9))
            ;(+ (* 1/2 (sin-osc.ar (* f 2)) (range (sin-osc.kr 0.1) 0.2 1)))
            ;(+ (* 1/3 (sin-osc.ar (* f 3))))
            ;(+ (* 1/4 (sin-osc.ar (* f 4)) (range (sin-osc.kr 0.2) 0.2 1)))
            ;(+ (* 1/5 (sin-osc.ar (* f 5)) (range (sin-osc.kr 0.3) 0.2 1)))
            ;(+ (* 1/6 (sin-osc.ar (* f 6)) (range (sin-osc.kr 0.4) 0.2 1)))
            (+ (* 1/7 (sin-osc.ar (* f 7)) (range (sin-osc.kr 0.5) 0.2 1)))
            (* 1/3)
            pan2.ar))

;; !
(def f (midicps 30))
(proxy :drone
       (-> (* 1/2 (sin-osc.ar (* f 1)) (range (sin-osc.kr 0.1 0)    .4 .9))
           (+ (* 1/4 (sin-osc.ar (* f 4)) (range (sin-osc.kr 0.1 (* 1/4 pi)) .4 .9)))
           (+ (* 1/6 (sin-osc.ar (* f 6)) (range (sin-osc.kr 0.1 (* 2/4 pi)) .4 .9)))
           (+ (* 1/8 (sin-osc.ar (* f 8)) (range (sin-osc.kr 0.1 (* 3/4 pi)) .4 .9)))
           (+ (* 1/9 (sin-osc.ar (* f 10)) (range (sin-osc.kr 0.2 (* 4/4 pi)) .4 .9)))
           (freeverb.ar :mix 0.6 :room 0.7)
           pan2.ar
           ))

(defun fq (n) (midicps (+ 54 (sc *pentatonic* n))))
(proxy :drone
       (-<> (sin-osc.ar (fq -5))
            (+ (sin-osc.ar (fq -2)))
            (+ (* 1/3 (sin-osc.ar (fq 0))))
            (* 1/8 (line.kr 0 1 5))
            pan2.ar))

(let ((f0 (fq -7)))
  (proxy :drone
         (-<> (* 1/2 (sin-osc.ar f0))
              (+ (* 1/3 (sin-osc.ar (* f0 2)) (range (sin-osc.kr 0.10) 0.1 0.9)))
              ;(+ (* 1/4 (sin-osc.ar (* f0 3)) (range (sin-osc.kr 0.11) 0.1 0.9)))
              (+ (* 1/5 (sin-osc.ar (* f0 4)) (range (sin-osc.kr 0.12) 0.1 0.9)))
              (+ (* 1/7 (sin-osc.ar (* f0 7)) (range (sin-osc.kr 0.13) 0.1 0.9)))
              (* 0.14)
              (* (line.kr 1 0 5))
              pan2.ar
              )))

;; !
(proxy :drone
       (-<> (midicps 52)
            (dyn-klang.ar [(mapcar (λ(f) (* f <>)) [1 2 3 4 3/2])
                           [(range (sin-osc.kr 0.1) 0.3 0.8)
                            (range (sin-osc.kr 0.11) 0.2 0.5)
                            (range (sin-osc.kr 0.12) 0.2 0.5)
                            (range (sin-osc.kr 0.13) 0.1 0.4)
                            (range (sin-osc.kr 0.09) 0.01 0.3)
                            ]])
            (* 0.2 (line.kr 0 1 10))
            pan2.ar))

(def *root* 54)
(defun my-scale (n) (sc *minor* n))
(defun fq (n) (midicps (+ *root* (my-scale n))))
(let ((f0 (fq -7)))
  (proxy :drone
         (-<> (* 1/2 (sin-osc.ar f0))
              (+ (* 1/3 (sin-osc.ar (* f0 2)) (range (sin-osc.kr 1 (* -1/2 pi)) 0.1 0.9)))
              (+ (* 1/4 (sin-osc.ar (* f0 3)) (range (sin-osc.kr 0.11 (* -1/2 pi)) 0.1 0.9)))
              ;(+ (* 1/5 (sin-osc.ar (* f0 4)) (range (sin-osc.kr 0.12 (* -1/2 pi) 0.1 0.9)))
              (+ (* 1/7 (sin-osc.ar (* f0 8)) (range (sin-osc.kr 0.13 (* -1/2 pi)) 0.1 0.9)))
              (* 0.20)
              ;(* (line.kr 1 0 5))
              ;(* (line.kr 0 1 5))
              pan2.ar
              (out.ar (abus :drone) <>)
              )))

;; !
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
;
(csnt :drone (synth 'drone))
(csnt :drone1 :stop)

;; !
(let ((fq (midicps (- 54 12))))
  (proxy :drone
       (-<> (mix [(sin-osc.ar fq 0 0.9)
                  (sin-osc.ar (* 2 fq) 0 (var-lag.kr (lf-noise0.kr 5)))
                  (sin-osc.ar (* 3 fq) 0 (var-lag.kr (lf-noise0.kr 5)))
                  ;(saw.ar (+ 440 (range (var-lag.kr (lf-noise0.kr 4)) -8 5)) 0.4)
                  (-> (saw.ar (+ (* 2/3 fq) (range (var-lag.kr (lf-noise0.kr 4)) -2 2)) 0.7)
                      (lpf.ar (* 2/3 fq 7)))
                  ])
            (* 0.1)
            greyhole.ar
            splay.ar
            )))

(release :drone)

(proxy :drone
       (let ((trig (sin-osc.kr (+ 7 (range (var-lag.kr (lf-noise0.kr 4)) -3 3)))))
         (-<> (-> (sin-osc.ar 440)
                  (+ (* (saw.ar 440) (env-gen.kr (perc 0.009 0.05) :gate trig))))
              (* 1/3 (env-gen.kr (perc 0.01 1/2) :gate trig))
              pan2.ar
              (* 0.5)
              greyhole.ar
            )))

(synth)

(release :drone)
(free :drone)
(csnt :drone :stop)

(stop)
