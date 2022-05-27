(load "~/src/mus/lib.cl")
(in-package :sc-user)
(named-readtables:in-readtable :sc)
(init)   ;; start new server
;(connect)
(bpm 60)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO
;; - compressor
;; - granular synthesis
;; - CL FFI, try using libjack midi

(defsynth noise ((freq 440) (amp 0.3) (out 0) (gate 1) (a 0.0001) (d 0.02) (r 0.0001))
  (-<> (brown-noise.ar)
       (bpf.ar freq)
       (* amp (env-gen.kr (linen a d r) :gate gate :act :free))
       pan2.ar (out.ar out <>)))

(defsynth ssn ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.0071) (d 0.2) (s 0.7) (r 0.5))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (sin-osc.ar fq)
         (+ (* (saw.ar (* 1.5 fq)) 0.05))
         (+ (* (saw.ar (* 4 fq)) 0.02))
         (* amp (env-gen.kr (perc a r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(def ptn (gen-xrand (loop :for i :from 5 :to 10 :collect i)))

(defpattern ssn :inf
  (play-note 'ssn
             :release nil
             :attr [:out 0 :amp 0.2 :a 0.01 :r 0.18]
             :note-fn (f_ [:freq (midicps (+ 54 -12 (sc *pentatonic* _)))]))
  (λ(i)
    (when (= 0 (mod i 8))
      (funcall ptn 0))
    (sim nil
         (seq (loop :repeat 6 :collect (funcall ptn)))
         (per-beat i
                   nil
                   ))))

(def ssn-fx (bus-audio :chanls 2))

(proxy :ssn-fx
       (-<> (in.ar ssn-fx 2)
            ;(freeverb.ar :room 0.5 :mix 0.7 :damp 0.1)
            ;(+ <> (* 1/3 (allpass-n.ar <> 4 0.23 2)))
            ;(hpf.ar 600)
            ) :pos :tail)

(ssn :start)
(ssn :stop)

(defpattern drums :inf
  (play-drum)
  (λ(i)
    (let ((o nil)
          (d ['bd :dur 0.02 :freq 220])
          (h ['noise :freq 2000 :d 0.001])
          (b ['noise :freq 500 :d 0.02])
          (s ['snare :d 0.014 :freq 7000 :amp 0.1]))
      (sim nil
           ;(seq (rotate (euclidian 7 12 h b) (random 4)))
           (seq d nil (once-every i 4 1 (seq s s)))
           (seq nil b)
           ))))

(drums :start)
(drums :stop)

;;;;

(defclass %seq () (data))
(defun seq (&rest data)
  (let ((ss (make-instance '%seq)))
    (setf (slot-value ss 'data) data)
    ss))

(defclass %sim () (data))
(defun sim (&rest data)
  (let ((ss (make-instance '%sim)))
    (setf (slot-value ss 'data) data)
    ss))

(defun seq-data (ss)
  (slot-value ss 'data))

(seq-data s)

(defpattern test :inf
  (play-note 'ssin
             :attr []
             :note-fn (f_ [:freq (midicps (+ 54 (sc *pentatonic* _)))]))
  (λ(i)
    (seq 0 2 (λ(b d) (synth 'ssin))
         (sim 0 3)
         0
         (snt 'ssaw :freq (midicps (+ 54 (sc *pentatonic* 5)))))))

(test :start)
(test :stop)

(proxy :test
       (-<> (saw.ar 220)
            (lpf.ar (-> (sin-osc.kr 0.2 (* -1/2 pi)) (range 100 500)))
            (* 0.2) pan2.ar))

(release :test)


(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil))
      (sim (per-beat i
                     (seq 'bd))))))

(drums :start)
(drums :stop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System

(server-query-all-nodes)
(stop)
