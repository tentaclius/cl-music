(require "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :play (:use :cl :sc :mylisp :midi-looper))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)
;
(defstruct knob%
  key
  (min 0)
  (max 127)
  (step 1)
  (value 0))

(flet ((kn (key min max step value)
         (make-knob% :key key :min min :max max :step step :value value)))
 (def *knobs* (hshm
                112 (kn :osc1 0 1 0.001 0)
                74  (kn :osc2 0 1 0.001 0)
                71  (kn :osc3 0 1 0.001 0)
                76  (kn :osc4 0 1 0.001 0)
                77  (kn :osc5 0 1 0.001 0)
                93  (kn :osc6 0 1 0.001 0)
                ;
                114 (kn :fold 0.1 1 0.001 1)
                18  (kn :lpf-freq 50 7000 1 7000)
                19  (kn :lpf-res 0.01 1 0.001 1)
                )))

;;;; Synth
(defsynth wah ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> ;(sin-osc.ar)
         (dyn-klang.ar [[fq (* fq 2) (* fq 3) (* fq 4) (* fq 5) (* fq 6)]
                        [(in.kr (cbus :osc1)) (in.kr (cbus :osc2)) (in.kr (cbus :osc3)) (in.kr (cbus :osc4)) (in.kr (cbus :osc5)) (in.kr (cbus :osc6))]])
         (fold.ar (- 0 (cbus-in :fold)) (cbus-in :fold))
         (rlpf.ar (cbus-in :lpf-freq) (cbus-in :lpf-res))
         (* (line.kr 0 1 4) amp (env-gen.kr (adsr 2 1/2 1 3) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(def *running-synths* (hshm))
(def *pressed-keys*   0)
(def *scales-conv*    (hshm  112 :osc1  74 :osc2  71 :osc3  76 :osc4  77 :osc5  93 :osc6))

(defun midi-map (messages)
  (dolist (message messages)
    (when-let ((msg (alsa-to-my-midi message)))
      (let ((note (mr-midi-event-note msg))
            (velo (mr-midi-event-velocity msg))
            (chan (mr-midi-event-channel msg)))
        (case (mr-midi-event-type msg)
          ;; Control
          (:control
           (when-let* ((knob (href *knobs* note))
                       (val  (+ (knob%-value knob) (* (knob%-step knob) (- velo 64)))))
             (setf (knob%-value knob)
                   (cond
                     ((< val (knob%-min knob)) (knob%-min knob))
                     ((> val (knob%-max knob)) (knob%-max knob))
                     (t val)))
             (cbus-set (knob%-key knob) (knob%-value knob))
             (writeln (knob%-key knob) " = " (knob%-value knob))))
          ;; Note On
          (:note_on
           (when (<= *pressed-keys* 0)
             (loop for key being the hash-keys of *running-synths* do
                   (release (href *running-synths* key)))
             (setf *running-synths* (hshm)))
           (hset *running-synths* note (synth 'wah :freq (midicps note) :amp (/ velo 127)))
           (incf *pressed-keys*))
          ;; Note Off
          (:note_off
           (decf *pressed-keys*))
          (:pitchbend
           (control-set (cbus :pitch-bend) (float (/ (mr-midi-event-velocity msg) 8192)))))
        (writeln msg)))))

(midihelper:stop-midihelper)
(midihelper:start-midihelper :master 96 'midi-map)

(stop)
