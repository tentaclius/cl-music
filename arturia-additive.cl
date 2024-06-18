(require "mylisp" "init.cl")
(defpackage :arturia (:use :cl :sc :mylisp))
(in-package :arturia)
(named-readtables:in-readtable :sc)
(sc-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYNTHESIZERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(knob-clear)
(defsynth fm-keys ((freq 440) (amp 0.3) (out 0))
  (-<> ;; Operator 1
       (sin-osc.ar (* freq (knob knob01 1 1 1 20)))
       (* (knob knob09 1 4 0 600)
          (env-gen.kr (perc (knob knob02 0.001 0.001 0.001 1)
                            (knob knob10 1 0.001 0.001 6))))
       ;; Operator 2
       (sin-osc.ar (+ (* freq (knob knob03 1 1 1 10)) <>))
       (* amp (knob knob11 1 4 0 600)
          (env-gen.kr (perc (knob knob04 0.001 0.001 0.001 1)
                            (knob knob12 1 0.001 0.001 6))))
       ;; Operator 3
       (sin-osc.ar (+ (* freq (knob knob05 1 1 1 10)) <>))
       (* amp (knob knob13 1 4 0 600)
          (env-gen.kr (perc (knob knob06 0.001 0.001 0.001 1)
                            (knob knob14 1 0.001 0.001 6))))
       ;; Operator 4 / carrier
       (sin-osc.ar (+ (* freq (knob knob07 1 1 1 10)) <>))
       (* amp (knob knob15 1 0.02 0 1)
          (env-gen.kr (perc (knob knob08 0.001 0.001 0.001 1)
                            (knob knob16 1 0.001 0.001 6))
                      :act :free))
       ;; out
       pan2.ar (out.ar out <>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Midi dispatch
(def *running-notes* (hshm))
(defun event-handler (msg)
  (when msg
    (let ((note (mr-midi-event-note msg))
          (velo (mr-midi-event-velocity msg)))
      (case (mr-midi-event-type msg)
        (:control (knob-set note velo))
        (:note_on
         (let ((snt (href *running-notes* note)))
           (when (is-playing-p snt)
             (release snt)
             (hset *running-notes* note nil))
           (when (> velo 0)
             (hset *running-notes* note (synth 'fm-keys
                                               :freq (midicps note)
                                               :amp (/ velo 127))))))
        (:note_off
         (let ((snt (href *running-notes* note)))
           (when (is-playing-p snt)
             (release snt)
             (hset *running-notes* note nil))))
        ; Pitchbend
        (:pitchbend
         (cbus-set :pitch-bend (float (/ (mr-midi-event-velocity msg) 8192))))))))

;; init

(def mr (mk-midi-reader "CLarturia"))
(start-midi-reader mr 'event-handler)

(stop-midi-reader mr)

(synth 'fm-keys :freq (midicps C-4))
