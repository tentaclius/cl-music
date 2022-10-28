(equire "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :play (:use :cl :sc :mylisp :midi-looper))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)
;
(def *running-synths* (hshm))
(def *ctrl-map* (hshm 114 :keys-feedback 112 :keys-delay))
(def main-synth 'keys)
;(def main-synth 'saw-bass)

(def snare-buf (buffer-read "/home/aerdman/Mus/samples/selection1/snare.wav"))
;
(def *drum-map* (hshm 39 ['bd]  68 ['bd] 
                      43 ['snare] 72 ['snare]
                      47 ['sample-dur-1 :buffer snare-buf :dur 2]
                      76 ['sample-dur-1 :buffer snare-buf :dur 2]
                      38 ['hh]
                      69 ['hh]
                      ))

;; Synths

(defsynth saw-bass ((freq 440) (freq0 440) (slide 0) (out 0) (amp 0.5) (gate 1)
                           (lpf 7) (res 1)
                           (a 0.01) (d 0.2) (s 0.4) (r 0.04))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> 
      (saw.ar fq)
      (rlpf.ar (* fq (cbus-in :q) 40/127) res)
      ;(+ (sin-osc.ar fq))
      (* amp (env-gen.kr (adsr a d s r) :act :free :gate gate))
      pan2.ar (out.ar out <>))))

(cbus-set :keys-feedback 3)
(cbus-set :keys-delay 17)
(proxy :keys
       (-<> (in.ar (abus :keys) 2)
            (* (-> 10 sin-osc.ar (range 0.7 1)))
            (+ <> (* 1/10 (comb-c.ar <> 2 (/ (cbus-in :keys-delay) 127) (cbus-in :keys-feedback))))
            ) :pos :tail)
(defsynth keys ((gate 1) (freq 440) (amp 0.5) (out (abus :keys)))
  (-<> (dyn-klang.ar [[freq (* freq 2) (* freq 3)] [1/2 1/4 1/8]])
       ;(+ (-> (white-noise.ar) (bpf.ar freq) (* 1/10)))
       (* amp (env-gen.kr (adsr 0.008 0.1 0.8 0.2) :act :free :gate gate))
       pan2.ar (out.ar out <>)))

;; Midi

(defun midi-map (messages)
  (dolist (message messages)
    (when-let ((msg (alsa-to-my-midi message)))
      (case (mr-midi-event-type msg)
        (:note_on
         (when-let* ((pad-number (mr-midi-event-note msg))
                     (velocity   (mr-midi-event-velocity msg))
                     (synth-spec (href *drum-map* pad-number)))
                    (when (> velocity 0)
                      (apply #'synth (append synth-spec [:amp (/ velocity 127)]))))
         ;(hset *running-synths* (mr-midi-event-note msg)
         ;      (synth main-synth :freq (midicps (mr-midi-event-note msg)) :amp (/ (mr-midi-event-velocity msg) 127)))
         )
        (:note_off
         (when-let ((snt (href *running-synths* (mr-midi-event-note msg))))
                   (release snt)))
        (:control
         (when-let* ((ctl-num (mr-midi-event-note msg))
                     (ctl-val (mr-midi-event-velocity msg))
                     (ctl-bus (href *ctrl-map* ctl-num)))
                    (writeln "CTRL_SET " ctl-bus " = " ctl-val)
                    (cbus-set ctl-bus ctl-val))))
      (writeln msg)
      ;(register-midi-event mr msg)
      )))

(midihelper:stop-midihelper)
(midihelper:start-midihelper :master 96 'midi-map)

;; Patterns

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil)
          (d ['bd :dur 0.05 :amp 0.4]))
      (sim nil
           (seq nil ['snare :d 0.1])
           (seql (euclidian 3 8 'hh ['hh :amp 0.1]))
           (per-beat i
             (seq d d)
             (seq d d)
             (seq d d)
             (seq d o o o  d d o o)
             )))))

(drums :start)
(drums :stop)

(defpattern bss
  (play-note 'fm-bass
             :release t
             :attr [:q 1  :amp 0.1 :depth 200 ]
             :note-fn (λ(n) [:freq (midicps (+ 54 -24 (sc *pentatonic* n)))]))
  (λ(i)
    (per-beat i
              (seq 4 3 2 0 1 -2)
              (seq 0 1 4 (random 6))
              )))

(bss :start)
(bss :stop)

(defun midi-player (beat)
  (dolist (m (recall-midi-events mr (ceiling (clock-beats))))
    (let ((dur (quantize-beat (mr-midi-event-duration m) 128))
          (start (quantize-beat (mr-midi-event-start m) 128)))
      (when (and start dur)
        (dur (+ beat start) dur
             [main-synth :freq (midicps (mr-midi-event-note m)) :amp (/ (mr-midi-event-velocity m) 127)]))))
  (clock-add (1+ beat) #'midi-player (1+ beat)))

(midi-player (clock-quant 8))

(def mr (make-midi-track))
(start-midi-recording mr 8)

(stop-midi-recording mr)

(defpattern metronome
  (play-note 'metronome
             :release nil
             :attr []
             :note-fn (λ(n) [:freq (midicps (+ 54 12 (sc *pentatonic* n)))]))
  (λ(i)
    (per-beat i
              (seq 5 0)
              (seq 0 0)
              (seq (seq 5 5) 0)
              (seq 0 0)
              )))

(metronome :start 8)
(metronome :stop)

(midihelper:send-event (midihelper:ev-noteon 0 36 44))
(midihelper:send-event (midihelper:ev-noteoff 0 36 0))

(loop :do
      (midihelper:send-event (midihelper:ev-noteon 0 36 44))
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sequencer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *seq (cl-alsaseq::open-seq "CL-midiout"))
(defparameter *seq* (cl-alsaseq::mem-ref *seq :pointer))
(defparameter *port* (cl-alsaseq::open-port "out" *seq* :output))

(def *grid-size* 8)
(def *off-color* 112)
(def *on-color* 75)
(def *cursor-on* 52)
(def *cursor-off* 93)
(def *beat-step* 1/4)
(def *init-delay* 0.2)
(def *light-delay* 0.2)
(def *midi-channel* 0)
(def *synth* ['ssin :amp 0.5 :q 1 :depth 200])
(def *synth-dur* 0.1)
(def *scale* *pentatonic*)
(def *root-note* 54)
;
(defun aref2 (ar i j)
  (aref (aref ar i) j))
;
(defun make-grid ()
  (let ((matrix (make-array *grid-size*)))
    (loop :for i :from 0 :to (1- *grid-size*)
          :do (setf (aref matrix i) (make-array *grid-size* :initial-element 0)))
    matrix))
;
(let* ((pad-to-midi-map
         (vector (vector 64 65 66 67  96 97 98 99)
                 (vector 60 61 62 63  92 93 94 95)
                 (vector 56 57 58 59  88 89 90 91)
                 (vector 52 53 54 55  84 85 86 87)
                 (vector 48 49 50 51  80 81 82 83)
                 (vector 44 45 46 47  76 77 78 79)
                 (vector 40 41 42 43  72 73 74 75)
                 (vector 36 37 38 39  68 69 70 71)))
       (midi-to-pad-map
         (let ((h (hshm)))
           (loop :for i :from 0 :to (1- *grid-size*)
                 :do (loop :for j :from 0 :to (1- *grid-size*)
                           :do (hset h (aref2 pad-to-midi-map i j) (list i j))))
           h)))
  (defun pad-to-midi (i j)
    (aref2 pad-to-midi-map i j))
  (defun midi-to-pad (pad)
    (href midi-to-pad-map pad)))
;
(let ((matrix (make-grid)))
  (defun matrix (i j &optional v)
    (if v
        (setf (aref (aref matrix i) j) v)
        (aref2 matrix i j))))
;
(defun light-pad (i j color)
  (midihelper:send-event (midihelper:ev-noteon *midi-channel* (pad-to-midi i j) color)))

(defun looper (beat j)
  (loop :for i :from 0 :to (1- *grid-size*)
        :do (when (> (matrix i (mod j *grid-size*)) 0)
              (bordeaux-threads:make-thread
                (let ((x i))
                  (lambda ()
                    (sleep 0.14)
                    (cl-alsaseq.quick:send-note-on 127 (+ *root-note* (sc *scale* (- *grid-size* 1 x))) *midi-channel* *seq* *port*)
                    (sleep 0.2)
                    (cl-alsaseq.quick:send-note-off 0 (+ *root-note* (sc *scale* (- *grid-size* 1 x))) *midi-channel* *seq* *port*))))
              (dur beat *synth-dur*
                   (append *synth* [:freq (midicps (+ *root-note* (sc *scale* (- *grid-size* 1 i))))]))
              ))
  (spawn
    (sleep *init-delay*)
    (loop :for i :from 0 :to (1- *grid-size*)
          :do (midihelper:send-event (midihelper:ev-noteon *midi-channel* (pad-to-midi i (mod j *grid-size*))
                                                           (if (= (matrix i (mod j *grid-size*)) 0) *cursor-off* *cursor-on*))))
    (sleep *light-delay*)
    (loop :for i :from 0 :to (1- *grid-size*)
          :do (midihelper:send-event (midihelper:ev-noteon *midi-channel* (pad-to-midi i (mod j *grid-size*))
                                                           (if (= (matrix i (mod j *grid-size*)) 0) *off-color* *on-color*)))))
  (clock-add (+ beat *beat-step*) #'looper (+ beat *beat-step*) (1+ j)))

(clock-add (clock-quant 1) #'looper (clock-quant 1) 0)


(defun midi-map (messages)
  (dolist (message messages)
    (when-let ((msg (alsa-to-my-midi message)))
      (case (mr-midi-event-type msg)
        (:note_on
         (when-let* ((note (mr-midi-event-note msg))
                     (velo (mr-midi-event-velocity msg))
                     (coord (midi-to-pad note))
                     (i (car coord))
                     (j (cadr coord)))
                    (when (> velo 0)
                      (matrix i j (if (= 0 (matrix i j)) velo 0))
                      (light-pad i j (if (= 0 (matrix i j)) *off-color* *on-color*)))))))))

(midihelper:stop-midihelper)
(midihelper:start-midihelper :master 96 'midi-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Drum Machine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *grid-width* 16)
(def *grid-height* 4)
(def *row-length* 8)
(def *off-color* [112 104])
(def *on-color* [75 24])
(def *cursor-on* [52 52])
(def *cursor-off* [3 3])
(def *beat-step* 1/8)
(def *init-delay* 0.2)
(def *light-delay* 0.1)
(def *midi-channel* 0)
(def *synths* [['bd] ['sample-dur-1 :buffer snare-buf] ['hh] ['metronome]])

(defun aref2 (ar i j)
  (aref (aref ar i) j))
;
(defun make-grid ()
  (let ((matrix (make-array *grid-height*)))
    (loop :for i :from 0 :to (1- *grid-height*)
          :do (setf (aref matrix i) (make-array *grid-width* :initial-element 0)))
    matrix))
;
(let* ((pad-to-midi-map
         (vector (vector 64 65 66 67  96 97 98 99  48 49 50 51  80 81 82 83)
                 (vector 60 61 62 63  92 93 94 95  44 45 46 47  76 77 78 79)
                 (vector 56 57 58 59  88 89 90 91  40 41 42 43  72 73 74 75)
                 (vector 52 53 54 55  84 85 86 87  36 37 38 39  68 69 70 71)
                 ))
       (midi-to-pad-map
         (let ((h (hshm)))
           (loop :for i :from 0 :to (1- *grid-height*)
                 :do (loop :for j :from 0 :to (1- *grid-width*)
                           :do (hset h (aref2 pad-to-midi-map i j) (list i j))))
           h)))
  (defun pad-to-midi (i j)
    (aref2 pad-to-midi-map i j))
  (defun midi-to-pad (pad)
    (href midi-to-pad-map pad)))
;
(let ((matrix (make-grid)))
  (defun matrix (i j &optional v)
    (if v
        (setf (aref (aref matrix (mod i *grid-height*)) (mod j *grid-width*)) v)
        (aref2 matrix (mod i *grid-height*) (mod j *grid-width*)))))
;
(defun light-pad (i j color)
  (when-let ((pad (pad-to-midi (mod i *grid-height*) (mod j *grid-width*))))
            (midihelper:send-event (midihelper:ev-noteon *midi-channel* (pad-to-midi (mod i *grid-height*) (mod j *grid-width*)) color))))
;
(defun update-pad (im jm &optional cursor)
  (let ((i (mod im *grid-height*))
        (j (mod jm *grid-width*)))
    (light-pad i j (if (= (matrix i j) 0)
                       (elt (if cursor *cursor-off* *off-color*) (floor (/ j *row-length*)))
                       (elt (if cursor *cursor-on* *on-color*) (floor (/ j *row-length*)))))))

(defun looper (beat j)
  (loop :for i :from 0 :to (1- *grid-height*)
        :do (when (> (matrix i (mod j *grid-width*)) 0)
              (at-beat beat (apply #'synth (elt *synths* i)))))
  (spawn
    (sleep *init-delay*)
    (loop :for i :from 0 :to (1- *grid-height*)
          :do (update-pad i j t))
    (sleep *light-delay*)
    (loop :for i :from 0 :to (1- *grid-height*)
          :do (update-pad i j)))
  (clock-add (+ beat *beat-step*) #'looper (+ beat *beat-step*) (1+ j)))

(clock-add (clock-quant 1) #'looper (clock-quant 1) 0)

(defun midi-map (messages)
  (dolist (message messages)
    (when-let ((msg (alsa-to-my-midi message)))
              (writeln msg)
      (case (mr-midi-event-type msg)
        (:note_on
         (when-let* ((note (mr-midi-event-note msg))
                     (velo (mr-midi-event-velocity msg))
                     (coord (midi-to-pad note))
                     (i (car coord))
                     (j (cadr coord)))
                    (when (> velo 0)
                      (matrix i j (if (= 0 (matrix i j)) velo 0))
                      (update-pad i j)
                      )))))))

(midihelper:stop-midihelper)
(midihelper:start-midihelper :master 96 'midi-map)


(loop :for i :from 0 :to 127
      :do (midihelper:send-event (midihelper:ev-noteoff *midi-channel* i 0)))

(clock-add (clock-quant 1) #'looper (clock-quant 1) 0)

(drums :start)
(drums :stop)

(stop)
