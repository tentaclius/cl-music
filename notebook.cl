;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delayed execution

(let ((tm (clock-quant 1))) (clock-add tm (lambda (beat) (at-beat beat (synth 'bd))) tm))
(let ((tm (+ (now) 1))) (callback tm (lambda (tm) (at tm (synth 'bd))) tm))
(spawn (sleep 1) (synth 'bd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; synths

;; sin oscilators by an array of frequencies and amplitudes
(proxy :test (with-controls ((f 160))
    (-<> (dyn-klang.ar [[f (* f 2) (* f 3)] [0.4 0.1 0.05]])
         (* 0.6 (env-gen.kr (adsr 0.002 0.1 0.6 0.4) :gate (lf-pulse.ar 6 0 0.1)))
         pan2.ar)))

;; delay
allpass-[lcn] <bufsize> <del-time> <decay>
(proxy :echo-bus
       (-<> (in.ar echo-bus 2)
            (+ <> (* 1/3 (allpass-n.ar <> 4 0.23 2)))))

;; distortions
(clip sig lo hi) fold wrap

;; cool echo-like effect
greyhole

;; change pitch/speed of a sample
(warp1.ar
  1                    ;; number of channels
  (bufnum bass-buf)    ;; busnum of the bus
  (line.ar 0 1 40 :act :free)    ;; buffer position (pointer)
  1                    ;; frequency scale
  0.1                  ;; grain window size
  -1                   ;; grain envelope buffnum (-1 = built-in)
  8                    ;; number of overlapping windows
  0.1                  ;; amount of randomness to the windowing functions
  2)

;; keep in mind!
exp-range

curverange ; check this out!

;; pitch-shift
(proxy :test
       (-<> (play-buf.ar 1 buf (buf-rate-scale.ir buf))
            (pitch-shift.ar 0.2 1.2)
            pan2.ar))

;; loop a sample
(proxy :test
  (-<> (loop-buf.ar 1 buf :rate (buf-rate-scale.ir buf) :start-pos 74000 :start-loop 74000 :end-loop 103014) ;(buf-frames.ir buf))
       (* (env-gen.kr (env [0 1 1 0] [5 1 5]))) pan2.ar))
