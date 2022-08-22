(require "mylisp" "init.cl")

(defpackage :arturia (:use :cl :sc :mylisp))
(in-package :arturia)
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
;
(def *synth* 'ssin)
(def *knobs* (hshm
               112 (mk-knob :amp 0 1 0.005 1)
               74  (mk-knob :lpf-res 0.01 1 0.005 1)
               71  (mk-knob :comb-damp 0 1 0.001 0)
               76  (mk-knob :comb-delay 0.001 2 0.001 0.01)
               77  (mk-knob :comb-feedback 0 50 0.1 0)
               93  (mk-knob :osc6 0 1 0.001 0)
               73  nil
               75  (mk-knob :sub-sin 0.00 1.00 0.01 0.5)
               ;
               114 (mk-knob :fold 1 140 0.1 0)
               18  (mk-knob :lpf-freq 50 11000 0.5 11000 t)
               19  (mk-knob :attack 0.001 1 0.001 0.01)
               16  (mk-knob :decay 0.001 1 0.001 0.01)
               17  (mk-knob :sustain 0.001 1 0.001 0.6)
               91  (mk-knob :release 0.001 4 0.001 0.01)
               79  nil
               72  nil
               ))
;
(defun init-knobs (kn)
  (loop :for k being each hash-key of kn
        :do (when-let ((v (href kn k)))
                      (writeln (knob-key v) " = " (knob-val v))
              (cbus-set (knob-key v) (knob-val v)))))
;
(init-knobs *knobs*)

(def *synth-ports*
  (hshm
    1 (λ(note velo)
        (synth *synth* :freq (midicps note) :amp (/ velo 127)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYNTHESIZERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proxy :master
  (-<> (abus-in :master)
       (* (cbus-in :fold))
       (fold -1 2)
       (/ (cbus-in :fold))
       (* (cbus-in :amp))
       (rlpf.ar (cbus-in :lpf-freq) (cbus-in :lpf-res))
       (+ <> (* (cbus-in :comb-damp) (comb-c.ar <> 2 (cbus-in :comb-delay) (cbus-in :comb-feedback))))
       ) :pos :tail)
;
(defsynth snt ((freq 440) (freq0 440) (slide 0) (amp 0.9)
              (out (abus :master)) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (saw.ar fq)
         (+ (* (cbus-in :sub-sin) (sin-osc.ar fq)))
         (* amp (env-gen.kr (adsr (cbus-in :attack)
                                  (cbus-in :decay)
                                  (cbus-in :sustain)
                                  (cbus-in :release)) :gate gate :act :free))
         pan2.ar (out.ar out <>))))
;
(cbus-set :keys-feedback 3)
(cbus-set :keys-delay 17)
(proxy :keys
       (-<> (in.ar (abus :keys) 2)
            (* (-> 10 sin-osc.ar (range 0.7 1)))
            (+ <> (* 1/10 (comb-c.ar <> 2 (/ (cbus-in :keys-delay) 127) (cbus-in :keys-feedback))))
            (* 0.4)
            ) :pos :tail)
(defsynth keys ((gate 1) (freq 440) (amp 0.5) (out (abus :keys)))
  (-<> (dyn-klang.ar [[freq (* freq 2) (* freq 3) (+ freq (sin-osc.kr 1))] [1/2 1/4 1/8 1/2]])
       (* amp (env-gen.kr (adsr 0.008 0.1 0.8 0.2) :act :free :gate gate))
       pan2.ar (out.ar out <>)))
;
(defsynth pulse-bass ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1) (lpf 70)
              (a 0.001) (d 0.2) (s 0.5) (r 0.5))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (pulse.ar fq)
         (+ (* 1/2 (sin-osc.ar fq)))
         (+ (* 1/2 (saw.ar (+ fq (* 2 (sin-osc.kr 1.1))))))
         (rlpf.ar (* fq (x-line.kr 20 6 0.1)) 1.19)
         (* 1/2 amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))
;
(defsynth ssaw ((freq 440) (freq0 440) (slide 0) (out (abus :master)) (amp 0.5) (gate 1) (lpf 7) (res 1))
  (-<> (x-line.kr freq0 freq slide)
       (saw.ar)
       (* amp (env-gen.kr (adsr (cbus-in :attack) (cbus-in :decay) (cbus-in :sustain) (cbus-in :release)) :act :free :gate gate))
       pan2.ar (out.ar out <>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun drum-seq-1 (i)
  (per-beat i
            (seq 'bd 'hh 'snare 'hh)
            (seq (seq 'bd 'bd) 'hh 'snare 'hh)))

(let ((i 0)
      (running-synths (hshm)))
  (labels ((note (synth chan note velo)
             (let ((snt (href running-synths (cons note chan))))
               (when (is-playing-p snt)
                 (release snt)
                 (hset running-synths (cons note chan) nil)))
             (when (> velo 0)
               (hset running-synths (cons note chan) (apply #'synth synth)))))
    (defun chan-map (chan note velo)
      (writeln ":" i)
      (case chan
        ((1) (note ['ssin :freq (midicps note)] chan note velo))
        ((2) (when (> velo 0) (play-seq (play-drum) 'drum-seq-1 i) (incf i)))))))

;; Midi dispatch
(defun event-handler (msg)
  (when msg
    (let ((note (mr-midi-event-note msg))
          (velo (mr-midi-event-velocity msg))
          (chan (mr-midi-event-channel msg)))
      (case (mr-midi-event-type msg)
        ; Control
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
        ; Note
        (:note_on (chan-map chan note velo))
        (:note_off (chan-map chan note 0))
        ; Pitchbend
        (:pitchbend
         (cbus-set :pitch-bend (float (/ (mr-midi-event-velocity msg) 8192))))))))

;; init

(def mr (mk-midi-reader "CLarturia"))
(start-midi-reader mr 'event-handler)

(stop-midi-reader mr)

(server-query-all-nodes)

(stop)

;;


(defun pattern (i)
  (per-beat i
            (seq 'bd 'hh 'snare 'hh)
            (seq (seq 'bd 'bd) 'hh 'snare 'hh)))

(def ii 0)

(play-seq (λ(b d e) (synth e)) 'pattern (incf ii))

(fmakunbound 'play-seq)
