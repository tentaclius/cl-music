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

(def *synth* 'keys)
(def *knobs* (hshm
               knob01 (mk-knob :sin1amp 0 1 0.0005 0)
               knob02 (mk-knob :sin2amp 0 1 0.0005 0)
               knob03 (mk-knob :sin3amp 0 1 0.0005 0)
               knob04 (mk-knob :sin4amp 0 1 0.0005 0)
               knob05 (mk-knob :sin5amp 0 1 0.0005 0)
               knob06 (mk-knob :sin6amp 0 1 0.0005 0)
               knob07 (mk-knob :sin7amp 0 1 0.0005 0)
               knob08 (mk-knob :sin8amp 0 1 0.0005 0)
               knob09 (mk-knob :sin1lfo 0 100 0.1 0)
               knob10 (mk-knob :sin2lfo 0 100 0.1 0)
               knob11 (mk-knob :sin3lfo 0 100 0.1 0)
               knob12 (mk-knob :sin4lfo 0 100 0.1 0)
               knob13 (mk-knob :sin5lfo 0 100 0.1 0)
               knob14 (mk-knob :sin6lfo 0 100 0.1 0)
               knob15 (mk-knob :sin7lfo 0 100 0.1 0)
               knob16 (mk-knob :sin8lfo 0 100 0.1 0)
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

(defsynth keys ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 4) (d 0.2) (s 0.7) (r 6))
  (let ((fq (+ (cbus-in :pitch-bend) (x-line.kr freq0 freq slide))))
    (-<> (dyn-klang.ar [[fq (* 2 fq) (* 3 fq) (* 4 fq) (* 5 fq) (* 6 fq) (* 7 fq) (* 8 fq)]
                        [(* (if~ (< (cbus-in :sin1lfo) 1) 1 (range (sin-osc.kr (cbus-in :sin1lfo)) 0 1)) (cbus-in :sin1amp))
                         (* (if~ (< (cbus-in :sin2lfo) 1) 1 (range (sin-osc.kr (cbus-in :sin2lfo)) 0 1)) (cbus-in :sin2amp))
                         (* (if~ (< (cbus-in :sin3lfo) 1) 1 (range (sin-osc.kr (cbus-in :sin3lfo)) 0 1)) (cbus-in :sin3amp))
                         (* (if~ (< (cbus-in :sin4lfo) 1) 1 (range (sin-osc.kr (cbus-in :sin4lfo)) 0 1)) (cbus-in :sin4amp))
                         (* (if~ (< (cbus-in :sin5lfo) 1) 1 (range (sin-osc.kr (cbus-in :sin5lfo)) 0 1)) (cbus-in :sin5amp))
                         (* (if~ (< (cbus-in :sin6lfo) 1) 1 (range (sin-osc.kr (cbus-in :sin6lfo)) 0 1)) (cbus-in :sin6amp))
                         (* (if~ (< (cbus-in :sin7lfo) 1) 1 (range (sin-osc.kr (cbus-in :sin7lfo)) 0 1)) (cbus-in :sin7amp))
                         (* (if~ (< (cbus-in :sin8lfo) 1) 1 (range (sin-osc.kr (cbus-in :sin8lfo)) 0 1)) (cbus-in :sin8amp))
                         ]])
         (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (case chan
        ((1) (note [*synth* :freq (midicps note)] chan note velo))
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

