(require "mylisp" "init.cl")
(defpackage :arpegiator (:use :cl :sc :mylisp))
(in-package :arpegiator)
(named-readtables:in-readtable :sc)
(sc-init)
(def sample-rate 48000)

(def *step* 0)
(def *pressed-keys* (list))
(def *playing-seq* (list))
(def *num-pressed* 0)

(defun event-handler (msg)
  (case (mr-midi-event-type msg)
    ((:note_on)
     (incf *num-pressed*)
     (push (mr-midi-event-note msg) *pressed-keys*))
    ((:note_off)
     (decf *num-pressed*)
     (when (= *num-pressed* 0)
       (setf *playing-seq* (reverse *pressed-keys*))
       (setf *pressed-keys* nil)))
    ((:control)
     (knob-set (knob-name (mr-midi-event-note msg)) (mr-midi-event-velocity msg))
     (writeln (knob-name (mr-midi-event-note msg)) " = " (knob-value (knob-name (mr-midi-event-note msg))))
     )))

(def mh (mk-midi-reader "CLarp"))
(start-midi-reader mh 'event-handler)
(start-midi-writer mh)

(regpattern :arpegiator
  (play-note 'trig :release nil)
  (λ(i) 
    (when (= 0 (mod i 4)) (setf *step* 0))
    (prog1
      (seql (loop :for i :from *step* :to (+ *step* 5)
                  :collect (elt *playing-seq* (mod i (length *playing-seq*)))))
      (incf *step* 6)))
  2)

(pstart :arpegiator)
(pstop :arpegiator)

;;;;

(defsynth trig ((freq 440) (amp 1))
  (cbus-out :freq freq)
  (cbus-out :amp amp)
  (cbus-out :synth-gate (env-gen.kr (perc 0.001 0.1) :act :free)))

(cbus-set :freq 440)
(cbus-set :amp 1)
(cbus-set :lfo-freq 1)
(cbus-set :lfo-v-freq 0)
(cbus-set :lfo-v-cutoff 0)
(cbus-set :lfo-v-res 0)
(cbus-set :lfo-v-amp 0)
(cbus-set :attack 0.001)
(cbus-set :release 1)
(cbus-set :tri/pls 0)
(cbus-set :pw 0.4)
(cbus-set :sub 0.2)
(cbus-set :noise 0.1)
(cbus-set :lpf-freq 11000)
(cbus-set :lpf-res 1)
(cbus-set :env-vcf 100)

(defun knob-name (name)
  (href (hshm knob02 :tri/pls
              knob03 :pw
              knob04 :lpf-freq 
              knob05 :attack   
              knob06 :release  
              knob07 :lfo-freq 
              knob10 :sub      
              knob11 :noise    
              knob12 :lpf-res  
              knob13 :env-vca  
              knob14 :env-vcf  
              knob16 :amp)
        name))

(proxy :OSC
       (let* ((lfo (sin-osc.kr (knob :lfo-freq 1 10 60 400)))
              (env (env-gen.kr (perc (knob :attack 0.01 0.001 0.001 1) (knob :release 0.5 0.001 0.001 1))
                               :gate (cbus-in :synth-gate)))
              (fq  (+ (cbus-in :freq) (range lfo 0 (knob :lfo-v-freq 0 1 0 400)))))
         (-<> 
           ;; Oscillator
           (+ (* (knob :tri/pls 1 0.1 0 1)
                 (pulse.ar fq (+ (knob :pw 0.2 0.01 0.01 0.5)
                                 (range lfo 0 (knob :lfo-v-pw 0 0.01 0 0.5)))))
              (* (- 1 (knob :tri/pls 1 0.1 0 1)) (saw.ar fq)))
           (+ (* (knob :sub 0 0.1 0 1) (sin-osc.ar fq)))
           (+ (* (knob :noise 0 0.1 0 1) (white-noise.ar)))
           ;; VCF
           (rlpf.ar (+ (knob :lpf-freq 11000 1 60 11000 t)
                       (* env (knob :env-vcf 0 1 0 400))
                       (range lfo 0 (knob :lfo-v-cutoff 0 1 0 500)))
                    (+ (knob :lpf-res 1 0.01 0 1) (range lfo 0 (knob :lfo-v-res 0 0.01 0 1))))
           ;; Envelope
           (* (+ (knob :amp 0.5 0.01 0 1)
                 env
                 (range lfo 0 (knob :lfo-v-amp 0 0.01 0 1))))
           pan2.ar)))
