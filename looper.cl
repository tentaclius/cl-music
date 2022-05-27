(load "~/src/mus/lib.cl")
(in-package :sc-user)
(ql:quickload :ltk)
(use-package :sc-extensions)
(use-package :bdef)
(named-readtables:in-readtable :sc)
(init)   ;; start new server
(bpm 60)

;;;;

(defsynth recorder ((freq 440) (bus 1) (buffer 0) (gate 1) (a 0.001) (r 0.001))
  (record-buf.ar
    (* (in.ar bus) (env-gen.kr (adsr a 0.1 1 r) :gate gate :act :free))
    buffer))

(labels ((rec-wait (beat i bus buffer rec-start rec-len)
           (if (< i rec-start)
               (progn
                 (princ "waiting") (terpri)
                 (at-beat beat (synth 'hh))
                 (clock-add (1+ beat) #'rec-wait (1+ beat) (1+ i) bus buffer rec-start rec-len))
               (progn
                 (princ "start recording") (terpri)
                 (clock-add (1+ beat) #'rec-record (1+ beat) (1+ i) buffer rec-start rec-len
                            (at-beat beat (synth 'recorder :buffer (bufnum buffer) :bus bus :pos :tail))))))
         (rec-record (beat i buffer rec-start rec-len snt)
           (if (< i (+ rec-start rec-len))
               (progn
                 (princ "recording") (terpri)
                 (clock-add (1+ beat) #'rec-record (1+ beat) (1+ i) buffer rec-start rec-len snt))
               (progn
                 (princ "stop recording/start playing") (terpri)
                 (at-beat beat (release snt))
                 (clock-add (1+ beat) #'rec-play (1+ beat) (1+ i) buffer rec-start rec-len
                            (at-beat beat (synth 'sample-1 :buffer buffer :amp 1 :pos :tail))))))
         (rec-play (beat i buffer rec-start rec-len snt)
           (if (= 0 (mod (- i rec-start) rec-len))
               (progn
                 (princ "restart playing") (terpri)
                 (at-beat beat (release snt))
                 (clock-add (1+ beat) #'rec-play (1+ beat) (1+ i) buffer rec-start rec-len
                            (at-beat beat (synth 'sample-1 :buffer buffer :amp 1 :pos :tail))))
               (progn
                 (princ "keep playing") (terpri)
                 (clock-add (1+ beat) #'rec-play (1+ beat) (1+ i) buffer rec-start rec-len snt)))) )
  (defun looper (beat bus buffer start len)
    (rec-wait beat 0 bus buffer start len)))

;;; -----------
;;; test

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
             (per-beat i
                       (seq 'hh)))))

(drums :start)
(drums :stop)

(def sample-rate 48000)
(def buf (buffer-alloc (* 70 sample-rate)))

(def bus (bus-audio :chanls 2))

(defsynth ssw ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> (saw.ar fq)
              (rlpf.ar (* fq 4))
              (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
              pan2.ar (out.ar out <>)))))

(defpattern ssw :inf
  (play-note 'ssw
             :attr [:out bus :a 0.0001 :r 0.1]
             :note-fn (f_ [:freq (midicps (+ 54 -24 (sc *pentatonic* _)))]))
  (λ(i) (per-beat i
                  (seq -1 0 1 2)
                  (seq -1 0 1 2)
                  (seq -1 0 1 2)
                  (seq -1 0 1 2 3 4)
                  )))

(ssw :start 4)
(ssw :stop)

(def bd ['bd :freq 200 :bass 20 :dur 0.09])

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
             (per-beat i
                       (seq bd 'hh 'snare 'hh)))))

(drums :start 4)
(drums :stop)

;; read line in into the buffer
(proxy :in
       (-<> (sound-in.ar 0)
            pan2.ar
            (out.ar bus <>)))

(proxy :bus-monitor
       (-<> (in.ar bus 2)
            pan2.ar
            ) :pos :tail)

(release :bus-monitor)

(dur (clock-quant 1) 4 (synth 'sample-1 :buffer buf))


(looper (clock-quant 4) bus buf 4 4)

;;;;

(stop)
