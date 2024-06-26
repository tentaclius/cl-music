(provide "clseqs-jack")
(in-package :clseqs)

(ql:quickload :cl-collider)
(ql:quickload :sc-extensions)
(ql:quickload :arrow-macros)

(use-package :sc)
(use-package :arrow-macros)

(in-package :sc)
(defugen (loop-buf "LoopBuf")
         (chanls bufnum &key (rate 1.0) (gate 1) (start-pos 0.0) (start-loop 0.0) (end-loop 0.0) (interpolation 2))
         ((:ar (multinew new 'multiout-ugen chanls bufnum rate gate start-pos start-loop end-loop interpolation))
          (:kr (multinew new 'multiout-ugen chanls bufnum rate gate start-pos start-loop end-loop interpolation))))
(in-package :clseqs)


(defvar *sc-started* nil)

(defun sc-init (&optional (port 57110))
  (when (not *sc-started*)
    (setf *sc-started* t)
    (setf sc:*sc-plugin-paths* nil)
    (setf sc:*s* (sc:make-external-server "localhost" :port port
                                          :server-options (sc:make-server-options :realtime-mem-size (* 65536 4))))
    (when (null (sc:all-running-servers))
      (sc:server-boot sc:*s*)
      (sc:jack-connect))))

(defun sc-connect (&optional (port 57110))
  (setf sc:*sc-plugin-paths* nil)
  (setf sc:*s* (sc:make-external-server "localhost" :port port :just-connect-p t))
  (sc:server-boot sc:*s*))

(defun init-synths ()
  ;; General synth for playing a buffer
  (defsynth sample ((buffer 0) (rate 1) (start 0) (amp 0.5) (out 0) (loop 0) (gate 1) (attack 0.1) (release 0.1))
            (let* ((sig (play-buf.ar 2 buffer (* rate (buf-rate-scale.ir buffer))
                                     :start-pos (* start (/ (buf-frames.ir buffer) (buf-channels.ir buffer)))
                                     :loop loop
                                     :act :free))
                   (sig (* sig (env-gen.kr (adsr attack 0.1 1 release) :gate gate :act :free))))
              (out.ar out (* amp sig)) ))

  (defsynth sample-1 ((buffer 0) (rate 1) (start 0) (amp 0.5) (out 0) (loop 0) (gate 1) (attack 0.1) (release 0.1))
            (let* ((sig (play-buf.ar 1 buffer (* rate (buf-rate-scale.ir buffer))
                                     :start-pos (* start (/ (buf-frames.ir buffer) (buf-channels.ir buffer)))
                                     :loop loop
                                     :act :free))
                   (sig (* sig (env-gen.kr (adsr attack 0.1 1 release) :gate gate :act :free))))
              (out.ar out (* amp (pan2.ar sig))) ))

  (defsynth sample-dur ((buffer 0) (rate 1) (start 0) (amp 0.5) (out 0) (loop 0) (gate 1) (dur 60) (attack 0.001) (release 0.1))
            (let* ((sig (play-buf.ar 2 buffer (* rate (buf-rate-scale.ir buffer))
                                     :start-pos (* start (/ (buf-frames.ir buffer) (buf-channels.ir buffer)))
                                     :loop loop
                                     :act :free))
                   (sig (* sig (env-gen.kr (linen attack (- dur attack release) release) :act :free)
                           (env-gen.kr (adsr 0.0001 0.0001 1 release) :gate gate :act :free))))
              (out.ar out (* amp sig)) ))

  (defsynth sample-dur-1 ((buffer 0) (rate 1) (start 0) (amp 0.5) (out 0) (loop 0) (gate 1) (dur 60) (attack 0.001) (release 0.1))
            (let* ((sig (play-buf.ar 1 buffer (* rate (buf-rate-scale.ir buffer))
                                     :start-pos (* start (/ (buf-frames.ir buffer) (buf-channels.ir buffer)))
                                     :loop loop
                                     :act :free))
                   (sig (* sig (env-gen.kr (linen attack (- dur attack release) release) :act :free)
                           (env-gen.kr (adsr 0.0001 0.0001 1 release) :gate gate :act :free))))
              (out.ar out (* amp (pan2.ar sig))) ))

  (defsynth sample-loop ((buffer 0) (rate 1) (amp 0.5) (out 0)
                                    (start-pos 0) (start-loop 0) (end-loop 0) (interpolation 2)
                                    (attack 0.01) (release 0.1) (gate 1))
            (let* ((sig (loop-buf.ar 2 buffer
                                     :rate (* rate (buf-rate-scale.ir buffer))
                                     :gate gate
                                     :start-pos start-pos
                                     :start-loop start-loop
                                     :end-loop end-loop
                                     :interpolation interpolation
                                     ))
                   (sig (* sig amp (env-gen.kr (adsr attack 0.1 1 release) :gate gate :act :free))))
              (out.ar out sig)))

  ;; Instruments

  (defsynth fm-bass ((freq 220) (freq0 220) (slide 0) (out 0) (amp 0.5) (gate 1)
                                (q 3) (depth 400)
                                (a 0.01) (d 0.2) (s 0.6) (r 0.4))
            (let ((fq (x-line.kr freq0 freq slide)))
              (-<> (sin-osc.ar (* q fq))
                   (* depth) (+ fq)
                   (sin-osc.ar)
                   (* amp (env-gen.kr (adsr a d s r) :act :free :gate gate))
                   pan2.ar (out.ar out <>))))

  (defsynth saw-bass ((freq 440) (freq0 440) (slide 0) (out 0) (amp 0.5) (gate 1)
                                 (lpf 7) (res 1)
                                 (a 0.01) (d 0.2) (s 0.4) (r 0.4))
            (let ((fq (x-line.kr freq0 freq slide)))
              (-<> 
                (saw.ar fq)
                (rlpf.ar (* fq lpf) res)
                (+ (* 1/2 (sin-osc.ar fq)))
                (* amp (env-gen.kr (adsr a d s r) :act :free :gate gate))
                pan2.ar (out.ar out <>))))

  (defsynth ssin ((freq 440) (freq0 440) (slide 0) (out 0) (amp 0.5) (gate 1)
                             (a 0.01) (d 0.2) (s 0.4) (r 0.4))
            (-<> (x-line.kr freq0 freq slide)
                 (sin-osc.ar)
                 (* amp (env-gen.kr (adsr a d s r) :act :free :gate gate))
                 pan2.ar (out.ar out <>)))

  (defsynth ssaw ((freq 440) (freq0 440) (slide 0) (out 0) (amp 0.5) (gate 1)
                             (lpf 7) (res 1)
                             (a 0.01) (d 0.2) (s 0.4) (r 0.4))
            (-<> (x-line.kr freq0 freq slide)
                 (saw.ar)
                 (rlpf.ar (* freq lpf) res)
                 (* amp (env-gen.kr (adsr a d s r) :act :free :gate gate))
                 pan2.ar (out.ar out <>)))

  ;; drums
  (defsynth bd ((freq 330) (bass 10) (dur 0.05) (d 0.1) (out 0) (amp 0.5))
            (-<> (x-line.ar freq bass dur)
                 (sin-osc.ar)
                 (+ (* 0.7 (sin-osc.ar 60)))
                 (* 0.9 amp (env-gen.kr (env (list 0 1 1 0) (list 0.0001 dur d)) :act :free))
                 pan2.ar
                 (out.ar out <>)))

  (defsynth hh ((out 0) (amp 0.3) (dur 0.1) (freq 8000))
            (-<> (white-noise.ar)
                 (hpf.ar freq)
                 (* amp (env-gen.kr (perc 0.0 dur) :act :free))
                 pan2.ar (out.ar out <>)))

  (defsynth snare ((freq 1100) (amp 0.3)
                               (out 0) (gate 1)
                               (a 0.001) (d 0.2))
            (-<> (white-noise.ar)
                 (lpf.ar freq)
                 (* amp (env-gen.kr (perc a d) :gate gate :act :free))
                 pan2.ar (out.ar out <>)))

  (defsynth clap ((out 0) (amp 0.5) (freq 1000))
            (-<> (white-noise.ar)
                 (* amp (env-gen.kr (perc 0 0.16) :act :free))
                 (bpf.ar freq)
                 pan2.ar
                 (out.ar out <>)))

  (defsynth metronome ((freq 440) (amp 0.3) (out 0))
            (-<> (sin-osc.ar freq)
                 (+ (* 1/20 (white-noise.ar)))
                 (* amp (env-gen.kr (perc 0.001 0.1) :act :free))
                 pan2.ar (out.ar out <>)))

  (defsynth recorder ((bus 1) (buffer 0) (dur 15))
            (record-buf.ar (delay-n.ar (in.ar bus 2) 1 0.003)
                           buffer
                           :run (env-gen.kr (linen 0 dur 0)
                                            :gate (changed.ar (in.ar bus 2))
                                            :act :free)))

  ) ;; init-synths 

(export '(sc-init sc-connect init-synths))

