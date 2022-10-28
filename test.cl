(require "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :play (:use :cl :sc :mylisp :midi-looper))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)

(defsynth ticks ((freq 440) (dur 0.1) (out 0) (amp 0.4))
  (-<> (white-noise.ar)
       (bpf.ar freq)
       (* amp (env-gen.kr (linen 0.001 dur 0.001) :act :free))
       pan2.ar (out.ar out <>)))

(synth 'ticks)

(defsynth dz ((freq 50) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.008) (d 0.1) (r 0.008))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (saw.ar fq)
         (* amp (env-gen.kr (linen a d r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(let ((k 'ticks)
      (z 'dz))
  (def eg (gen-list (euclidian 5 12 k z))))
(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil)
          (k 'ticks)
          (z 'dz)
          (b 'bd))
      (sim
        (seq b)
        (per-beat i
             (seql (loop :repeat 6 :collect (funcall eg))))))))

(drums :start)
(drums :stop)

;; nice!
(proxy :pulse-bass-fx
       (-<> (abus-in :pulse-bass)
            ;(+ <> (* 1/4 (comb-n.ar <> 2 0.19 2)))
            ;(* 60)
            ;(fold 0.2 1)
            ;(/ 10)
            ;freeverb.ar
            ))
(defsynth pulse-bass ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out (abus :pulse-bass)) (gate 1) (lpf 70)
              (a 0.001) (d 0.1) (s 0.1) (r 0.5))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (pulse.ar fq)
         (+ (* 1/2 (sin-osc.ar fq)))
         (+ (* 1/2 (saw.ar (+ fq (* 2 (sin-osc.kr 1.1))))))
         (rlpf.ar (* fq (x-line.kr 40 1 0.20)) 0.19)
         (* 1/2 amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(dur (clock-beats) 1/6 ['pulse-bass :freq 100 :amp 1])

(defpattern puls
  (play-note 'pulse-bass
             :release t
             :attr []
             :note-fn (λ(n) [:freq (midicps (+ 54 -12 (sc *pentatonic* n)))]))
  (λ(i)
    (per-beat i 
              (seq 0 2 3 (random 8))
              (seq 0 1 (random 4) -1))))

(puls :start)
(puls :stop)

(defpattern drums
  (play-drum :amp 0.81)
  (λ(i)
    (let ((o nil))
      (sim ;(once-every i 2 0 (seq o o (seq 'hh 'hh) o))
           ;(seq o 'snare)
           (per-beat i
             (seq 'bd 'bd))))))

;;;

(proxy :test
       (-<> (saw.ar 220)
            (+ (saw.ar (+ 220 (* 3.13 (sin-osc.kr 4)))))
            (* 1/3)
            pan2.ar))

(release :test)

;;;

(def bf (buffer-read "~/Mus/31seconds.wav"))
(def bf (buffer-read "~/Mus/1.wav"))
(def bf (buffer-read "~/Mus/WavePoint-Vocal-AllNightLong.wav"))
(def bf (buffer-read "~/Mus/prodigyloop.wav"))

(def gn (gen-xrand (list 2 3 4 5 6 7)))

(defpattern smpl
  (λ(b d e)
    (when e (at-beat b (synth 'sample-dur :buffer bf
                              :attack 0.109 :release 0.11
                              :start (/ 1 (funcall gn)) :dur 0.15
                              :rate (rand-el 2/3 1/2 1 1.2 1.5)))))
  (λ(i)
    (per-beat i 
              (seql (euclidian 3 8 t))
              )))

(smpl :start)
(smpl :stop)

(stop)

(def g (gen-list [0 1 2 3 4 5]))

(defun chord (scale chord &optional (n 0))
  (siml (loop :for s :across chord
              :collect (sc scale (+ n s)))))

(def sept #(0 2 4 6))
(defpattern chrd
  (play-note 'saw-bass
             :release t
             :attr [:amp 0.1 :a 0.05 :r 0.9]
             :note-fn (λ(n) [:freq (midicps (+ 54 n))]))
  (λ(i)
    (seq (per-beat i
              (chord *minor* sept)
              (seq nil (seq-map (λ(n) [n :dur 3/4])
                                (chord *minor* sept (per-beat i 0 2 0 3)))
                   nil nil)))))

(chrd :start)
(chrd :stop)

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil))
      (sim ;(once-every i 4 0 (seq o o o (seq 'hh 'hh)))
        (per-beat i
             (seq 'bd 'bb (once-every i 2 0 'bb) 'bd 'bb o))))))

(drums :start)
(drums :stop)

(defsynth bb ((freq 60) (amp 0.1) (out 0) (a 0.06) (r 0.4))
  (let ((fq freq))
    (-<> (sin-osc.ar fq)
         (* amp (env-gen.kr (perc a r) :act :free))
         pan2.ar (out.ar out <>))))

(defpattern ssn
  (play-note 'saw-bass
             :release t
             :attr [:r 0.3 :d 0.1 :s 0.01 :lpf 4 :res 0.1 :amp 0.1]
             :note-fn (λ(n) [:freq (midicps (+ 54 (sc *pentatonic* n)))]))
  (λ(i) (sim (seq-map (λ(x) [x :s 0.1 :lpf 2 :res 0.3 :amp 0.4])
                      (seq (per-beat i -5 -7 -4 -3)))
             (seq (per-beat i 6 6 6 1 5 4 5 7) 3 4 2 5 (per-beat i 1 0)))))

(ssn :start)
(ssn :stop)

(stop)

;;;


(defsynth ssw ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (saw.ar)
         (rlpf.ar (* fq 1) 0.1)
         (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(defpattern test
  (play-note 'ssw
             :release t
             :attr []
             :note-fn (λ(n) [:freq (midicps (+ 54 (sc *pentatonic* n)))]))
  (λ(i)
    (seq 0 nil 0 nil)))

(test :start)
(test :stop)

(stop)

(defun chrd (&optional (mod nil) (shift 0) (root 0) (scale *chromatic*))
  (mapcar
    (curry (λ(n) (+ root (sc scale (+ n shift)))))
    (case mod
      ((nil :3) (list 0 2 4))
      ((:7)     (list 0 2 4 6))
      ((:5)     (list 0 4 7))
      ((:sus4)  (list 0 2 3))
      ((:sus2)  (list 0 1 4))
      ((:sus9)  (list 0 4 8))
      ((:7sus4) (list 0 2 3 6)))))

(defpattern test
  (play-note 'ssaw :attr [:amp 0.1 :a 0.001 :s 0.5 :d 0.1]
             :note-fn (λ(e) [:freq (midicps (+ 55 (sc *minor* e)))]))
  (λ(i)
    (per-beat i
      (seql (chrd :3 0))
      (seql (chrd :3 5))
      (seql (chrd :3 2))
      (seql (chrd :3 3))
      )) 2/4)
(test :start 1 4)

;;;

(proxy :ding-fx
       (-<> (abus-in :ding)
            (+ (* 0.03 (saw.ar (range (var-lag.kr (lf-noise0.kr 4)) 218 222))))
            (greyhole.ar)
            ) :pos :tail)
(defsynth ding ((freq 440) (amp 0.7) (gate 1) (ifreq 1)
                           (a 0.004) (r 0.5))
  (let ((igate (sin-osc.kr ifreq)))
    (-<> (sin-osc.ar freq)
         (* amp (env-gen.kr (perc a r) :gate igate))
         (+ (* amp 1/40 (env-gen.kr (perc 0.001 0.2) :gate igate)
               (white-noise.ar)))
         (* (env-gen.kr (adsr 0.001 0.001 1 0.1) :gate gate :act :free))
         pan2.ar (abus-out :ding <>))))

(nsynth :ding1 'ding :r 0.2 :ifreq 1)
(nsynth :ding2 'ding :r 0.14 :ifreq 1.1 :freq 220)
(nsynth :ding3 'ding :r 0.2 :ifreq 2/3 :freq 110)
(nsynth :ding4 'ding :r 0.1 :ifreq 3/5 :freq 175)

(nsynth :ding4)


(stop)

;;;

(def note-chan (make-instance 'channel))

(! note-chan 'hello)

(spawn (writeln (? note-chan)))

(spawn (! note-chan 'hello))
(spawn (! note-chan 'hi))

(? note-chan 0)

(defpattern chn
  (play-note 'ssin :note-fn (λ(n) [:freq (midicps (+ 54 (sc *pentatonic* n)))]))
  (λ(i)
    (sim (snt 'metronome)
         0
         (seq (? note-chan 0.001) (? note-chan 0.001) (? note-chan 0.001) (? note-chan 0.001))
         )))

(chn :start)
(chn :stop)

(spawn (loop :for i :from 0 :to 6 :do (! note-chan i)))

(list (? note-chan 0.001) (? note-chan 0.001) (? note-chan 0.001) (? note-chan 0.001))


(clock-bpm 120)

(defpattern drums
  (play-drum :amp 0.9)
  (λ(i)
    (let ((o nil)
          (b ['bd :dur 0.09 :bass 60])
          (h 'hh)
          (s 'snare))
      (sim 
           (per-beat i
             ;(seq b h b h b h b (seq h h))
             (seq b b b b)
             ))))
  2)

(drums :start)
(drums :stop)


(defsynth ssw ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (saw.ar fq)
         (+ (* 1/6 (pulse.ar (+ 1 (* fq 2)))))
         (lpf.ar (+ (* 2 fq) (* (env-gen.kr (linen 0.001 0.01 0.1)) 1700)))
         ;(lpf.ar (+ (* fq 1/20) (* (env-gen.kr (linen 0.1 0.1 0.1)) 10000)))
         (+ (* 1/2 (sin-osc.ar (/ fq 2))))
         (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(defpattern ssw
  (play-note 'ssw 
             :attr [:amp 0.1 :r 1]
             :note-fn (λ(n) [:freq (midicps (+ 54 -12 (sc *pentatonic* (+ n 0))))]))
  (λ(i)
    (sim
      ;(seql (euclidian (1+ (random 7)) 8 [-2 :amp 0.3]))
      (per-beat i
              (seql (euclidian 3 10 [0 :amp 0.7]))
              (seql (euclidian 3 10 [0 :amp 0.7]))
              (seql (euclidian 5 8 [1 :amp 0.7]))
              (seql (euclidian 3 8 [2 :amp 0.7]))
              )
    ))
  2)

(ssw :start)
(ssw :stop)


(defsynth ssw ((freq 440) (freq0 440) (slide 0) (amp 0.3) (out 0) (gate 1)
               (a 0.01) (d 0.2) (s 0.7) (r 0.1))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (+ (* 1/8  (pulse.ar fq))
            (* 1/10 (pulse.ar (+ fq 1)))
            (* 1/15 (pulse.ar (+ fq 2)))
            (* 1/20 (pulse.ar (+ fq 3))))
         (rlpf.ar (* fq 3.9) (/ fq 1000))
         (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(proxy :ssw-fx
       (-<> (abus-in :ssw)
            (+ <> (* 1/8 (comb-n.ar <> 2 0.361 10)))
            ;greyhole.ar
            ) :pos :tail)

(defpattern ssw
  (play-note 'ssw
             :release t
             :attr [:out (abus :ssw)]
             :note-fn (λ(n) [:freq (midicps (+ 54 -12 (sc *pentatonic* (+ n 0))))]))
  (λ(i)
    (per-beat i
              (seq 0 (random 5) 2 5 0 1)
              (seql (euclidian (1+ (random 5)) 8 0 2))
              (seq 0 1 1 3 5 (random 5))
              (seq 0 2 0 4 7 1)
              )))

(ssw :start)
(ssw :stop)

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil)
          (b ['bd])
          (h ['hh])
          (s ['snare]))
      (sim (seq o s o s)
           (per-beat i
             (seq b h (seql (loop :repeat 4 :collect b)) h (seq b b) (seq h h) b (seql (loop :repeat 8 :collect h)))
             (seq b h b h (seq b b) (seq h h) (seql (loop :repeat 6 :collect b)) h)
             ))))
  2)

(drums :start)
(drums :stop)

(stop)
