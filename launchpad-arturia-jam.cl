(require "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :play (:use :cl :sc :mylisp :midi-looper))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)

(defstruct knob
  key
  (min 0)
  (max 127)
  (step 1)
  (value 0)
  (expp nil))
(defun mk-knob (key &optional (min 0) (max 127) (step 1) (val 0) (expp nil))
  (make-knob :key key
             :min (if expp (sqrt min) min)
             :max (if expp (sqrt max) max)
             :step step
             :value (if expp (sqrt val) val)
             :expp expp))
;
(defun knob-val (knob)
  (if (knob-expp knob)
      (expt (knob-value knob) 2)
      (knob-value knob)))


(def snare-buf (buffer-read "/home/aerdman/Mus/samples/selection1/snare.wav"))
;
(def *grid-size* 8)
(def *off-color* 112)
(def *on-color* 75)
(def *cursor-on* 52)
(def *cursor-off* 3)
(def *beat-step* 1/8)
(def *init-delay* 0.18)
(def *light-delay* 0.1)
(def *midi-channel* 0)
(def *synth* ['saw-bass :amp 0.5])
(def *synth-dur* 0.2)
(def *scale* *pentatonic*)
(def *root-note* 33)
;
(def *keys-synth* 'pulse-bass)
(def *keys-channel* 1)
(def *running-synths* (hshm))
(def *switches* (hshm
                  36 :trig-bd
                  37 :trig-snare
                  38 :trig-hh
                  39 nil
                  40 nil
                  41 nil
                  42 nil
                  43 nil
                  ))
;
(def *knobs* (hshm
               114 (mk-knob :pads-lpf 20 1000 0.1 1000 t)
               112 (mk-knob :pads-res 0.01 1 0.01 1)
               74  (mk-knob :pads-env-a 0.001 1 0.001 0.009)
               71  (mk-knob :pads-env-d 0.001 1 0.001 0.1)
               18  (mk-knob :pads-env-r 0.001 1 0.001 0.01)
               ))

(cbus-set :pads-lpf 127)
(cbus-set :pads-res 127)

(proxy :mixer
       (+ (-> (abus-in :pad))
          (-> (* (abus-in :snare) (cbus-in :trig-snare)))
          (-> (* (abus-in :hh) (cbus-in :trig-hh)))
          (-> (* (abus-in :bd) (cbus-in :trig-bd)))))

(defun note-fq (n)
  (midicps (+ *root-note* (sc *pentatonic* n))))
;
(def *synth-map* (vector
                   (append *synth* [:amp 0.2 :out (abus :pad) :freq (note-fq 5)])
                   (append *synth* [:amp 0.2 :out (abus :pad) :freq (note-fq 4)])
                   (append *synth* [:amp 0.2 :out (abus :pad) :freq (note-fq 3)])
                   (append *synth* [:amp 0.2 :out (abus :pad) :freq (note-fq 2)])
                   (append *synth* [:amp 0.2 :out (abus :pad) :freq (note-fq 1)])
                   (append *synth* [:amp 0.2 :out (abus :pad) :freq (note-fq 0)])
                   ['bd :out (abus :bd)]
                   ['snare :out (abus :snare)]
                   ))

;; Synth for the keys
(cbus-set :keys-feedback 3)
(cbus-set :keys-delay 17)
(proxy :keys
       (-<> (in.ar (abus :keys) 2)
            (* (-> 10 sin-osc.ar (range 0.7 1)))
            (+ <> (* 1/10 (comb-c.ar <> 2 (/ (cbus-in :keys-delay) 127) (cbus-in :keys-feedback))))
            (* 0.4)
            ) :pos :tail)
(defsynth keys ((gate 1) (freq 440) (amp 0.5) (out (abus :keys)))
  (-<> (dyn-klang.ar [[freq (* freq 2) (* freq 3)] [1/2 1/4 1/8]])
       (* amp (env-gen.kr (adsr 0.008 0.1 0.8 0.2) :act :free :gate gate))
       pan2.ar (out.ar out <>)))
;
;; Synth for the pads
(defsynth saw-bass ((freq 440) (freq0 440) (slide 0) (out (abus :lpf)) (amp 0.5))
  (let ((fq (+ (x-line.kr freq0 freq slide))))
    (-<> 
      (pan2.ar (saw.ar fq) -0.5)
      (+ (* 1/2 (pan2.ar (saw.ar (+ 1 fq)) 0.5)))
      (rlpf.ar (cbus-in :pads-lpf) (cbus-in :pads-res))
      (+ (* 1/2 (sin-osc.ar fq)))
      (* amp (env-gen.kr (linen (cbus-in :pads-env-a)
                                (cbus-in :pads-env-d)
                                (cbus-in :pads-env-r))
                         :act :free))
      pan2.ar (out.ar out <>))))
;
(defsynth pulse-bass ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1) (lpf 70)
              (a 0.001) (d 0.2) (s 0.5) (r 0.5))
  (let ((fq (+ (* (cbus-in :pitch-bend) 50) (x-line.kr freq0 freq slide))))
    (-<> (pulse.ar fq)
         (+ (* 1/2 (sin-osc.ar fq)))
         (+ (* 1/2 (saw.ar (+ fq (* 2 (sin-osc.kr 1.1))))))
         (rlpf.ar (* fq (x-line.kr 20 6 0.1)) 1.19)
         (* 1/2 amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

;;;;

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

;; Looper. Highlights the pads and plays the notes
(defun looper (beat j)
  ;; Play the synths
  (loop :for i :from 0 :to (1- *grid-size*)
        :do (when (> (matrix i (mod j *grid-size*)) 0)
              (at-beat beat (apply #'synth (elt *synth-map* i)))))
  ;; light up the corresponding pads
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

(defun midi-map (messages)
  (dolist (message messages)
    (when-let ((msg (alsa-to-my-midi message)))
      ;(writeln msg)
      (let ((note (mr-midi-event-note msg))
            (velo (mr-midi-event-velocity msg))
            (chan (mr-midi-event-channel msg)))
        (case (mr-midi-event-type msg)
          (:note_on
           ;; Launchpad is on channel 0
           (when (= 0 (mr-midi-event-channel msg))
             (when-let* ((coord (midi-to-pad note))
                         (i (car coord))
                         (j (cadr coord)))
                        (when (> velo 0)
                          (matrix i j (if (= 0 (matrix i j)) velo 0))
                          (light-pad i j (if (= 0 (matrix i j)) *off-color* *on-color*)))))
           ;; Minilab is on channel 1
           (when (or (= 9 chan) (= 1 chan))
             (if (and (= 9 chan) (>= note 36) (<= note 43))
                 ;; a trigger
                 (when-let ((c (href *switches* note)))
                           (writeln c " = " 1)
                           (cbus-set c 1))
                 ;; a note
                 (progn (when-let* ((s (href *running-synths* note)) (p (is-playing-p s))) (release s))
                        (hset *running-synths* note (synth *keys-synth* :freq (midicps note) :amp (/ velo 127)))))))
          (:note_off
           (when (or (= 9 chan) (= 1 chan))
             (if (and (= 9 chan) (>= note 36) (<= note 43))
                 (when-let ((c (href *switches* note)))
                     (writeln c " = " 0)
                     (cbus-set c 0))
                 (when-let* ((s (href *running-synths* note))
                             (p (is-playing-p s)))
                      (release s)
                      (hset *running-synths* note nil)))))
          (:control
           (when-let* ((knob (href *knobs* note))
                       (val  (+ (knob-value knob) (* (knob-step knob) (- velo 64)))))
                      (setf (knob-value knob)
                            (cond
                              ((< val (knob-min knob)) (knob-min knob))
                              ((> val (knob-max knob)) (knob-max knob))
                              (t val)))
                      (cbus-set (knob-key knob) (knob-val knob))
                      (writeln (knob-key knob) " = " (knob-val knob))))
          (:pitchbend
           (cbus-set :pitch-bend (float (/ (mr-midi-event-velocity msg) 8192))))
          )))))

;;;;

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil))
      (sim (per-beat i
             (seql (loop :repeat 8 :collect ['hh :out (abus :hh) :amp (/ 1 (+ 5 (random 10)))])))))))

(drums :start)
(drums :stop)

(midihelper:stop-midihelper)
(midihelper:start-midihelper :master 96 'midi-map)

(clock-add (clock-quant 1) #'looper (clock-quant 1) 0)

(stop)
