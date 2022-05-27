(load "~/src/mus/lib.cl")
(in-package :sc-user)
(use-package :sc-extensions)
(use-package :bdef)
(named-readtables:in-readtable :sc)
(init)   ;; start new server
(bpm 60)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def sinchord-bus (bus-audio :chanls 2))

(defsynth sinchord ((note 50) (amp 0.8) (out sinchord-bus))
    (-<> (* 1/2 (sin-osc.ar (midicps note)))
         (+ (* 1/3 (sin-osc.ar (midicps (+ 7 note)))))
         (+ (* 1/4 (sin-osc.ar (midicps (+ 12 note)))))
         ;(+ (* 1/30 (sin-osc.ar (* 3 (midicps note)))))
         (* amp (env-gen.kr (perc 0.008 0.5)))
         (* (env-gen.kr (linen 0 4 0) :act :free))
         pan2.ar (out.ar out <>)))

(proxy :sinchord-fx
       (-<> (in.ar sinchord-bus 2)
            (freeverb.ar :mix 0.5)
            (+ <> (* 1/4 (allpass-n.ar <> 3 0.2 2)))
            ):pos :tail)

(defpattern sinchord
  (play-note 'sinchord
             :attr [:amp 0.6]
             :note-fn (f_ [:note _]))
  (λ(i)
    (let ((- nil) (k 50) (h 53) (l 55)) 
      (sim (per-beat i
                     (seq k - - -  - - - -)
                     (seq - - - -  - - h -)
                     (seq k - - -  - - - -)
                     (seq - - - l  - - - -)
                     )))))

(defpattern sinchord
  (play-note 'sinchord
             :attr [:amp 0.9]
             :note-fn (f_ [:note (+ 50 _)]))
  (λ(i)
    (let ((- nil)) 
      (sim (per-beat i
                     (seq 0 - - -  - - - -)
                     (seq - - - -  - - 3 -)
                     (seq 0 - - -  - - - -)
                     (seq - - 5 -  - 5 - -)
                     (seq 0 - - -  - - - -)
                     (seq - - - -  - - 3 -)
                     (seq 0 - - -  - - - -)
                     (seq - - - 5  - 5 - -)
                     )))))

(sinchord :start 4)
(sinchord :stop)

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil)
          (d ['bd :amp 0.6 :dur 0.051])
          (s ['bd :amp 0.1 :dur 0.04])
          (h ['hh :amp 0.2])
          (x ['hh :amp 0.16 :dur 0.015]))
      (sim (once-every i 4 0 (seq o o o h))
           ;(seql (loop :repeat 8 :collect x))
           (per-beat i
                     (seq  d o o o  d o o o)
                     (seq  d o o o  d o o o)
                     )))))

(drums :start 4)
(drums :stop)


(defsynth bass ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.008) (d 0.2) (s 0.7) (r 0.3))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (saw.ar fq)
         (rlpf.ar (* fq 3) 0.7)
         (+ (sin-osc.ar fq))
         (* amp (env-gen.kr (perc a r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(def bass-seq (gen-list (euclidian 7 18 0 5)))

(defpattern bass
  (play-note 'bass
             :release nil
             :attr [:amp 0.2]
             :note-fn (f_ [:freq (midicps (+ 26 (sc *pentatonic* _)))]))
  (λ(i)
    (sim 
      (seql (loop :repeat 8 :collect (funcall bass-seq)))
      )))

(bass :start 4)
(bass :stop)

(sinchord :start)
(bass :start)
(drums :start)

(sinchord :stop)
(bass :stop)
(drums :stop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System

(stop)
