(load "~/src/cl-music/lib.cl")
(in-package :sc-user)
(named-readtables:in-readtable :sc)
(init)   ;; start new server
;(connect)

(clock-bpm 30)

;;;

(defsynth ssin ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (sin-osc.ar fq)
         (+ (* 1/2 (sin-osc.ar (* fq 1.5))))
         (+ (* 1/10 (sin-osc.ar (* fq 3))))
         (* amp (env-gen.kr (perc 0.001 0.4) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(proxy :ssin-fx
       (-<> (in.ar (abus :ssin) 2)
            (greyhole.ar 0.1)
            ;(+ <> (* 1/2 (allpass-n.ar <> 4 0.22 2)))
            ))

(defpattern ssin
  (play-note 'ssin
             :release nil
             :attr [:out (abus :ssin)]
             :note-fn (λ(n) [:freq (midicps (+ 54 (sc *pentatonic* n)))]))
  (λ(i)
    (sim ;(seq 5 4 3 2 1 0)
         (per-beat i
                   (seq 0 1 2 3)
                   (seq 0 1 2 5)
                   (seq 0 1 2 3)
                   (seq 0 1 6 4)
                   ))))

(ssin :start)
(ssin :stop)

(proxy :saw
       (-<> (saw.ar (midicps 54))
            (+ (* 2/3 (saw.ar (+ 3 (midicps 54)))))
            (lpf.ar (* (midicps 54) 5))
            (* (range (sin-osc.kr 3) 0.2 1))
            (pan2.ar)
            (greyhole.ar)
            (* 1.2)
            ))

(release :saw)

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil))
      (sim (seq o 'snare)
           (once-every i 4 0  (seq nil nil nil 'bd))
           (per-beat i
             (seq 'bd 'bd)
             (seq (seq 'bd 'bd) nil 'bd nil)
             )))))

(drums :start)
(drums :stop)
