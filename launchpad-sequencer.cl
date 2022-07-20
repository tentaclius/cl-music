(require "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :play (:use :cl :sc :mylisp :midi-looper))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)

;; SETTINGS
(def *seq-area-bg* 112)
(def *seq-area-cursor* 3)
(def *seq-area-hl* 75)
(def *patt-area-bg* 104)
(def *patt-area-cursor* 3)
(def *patt-area-hl* 75)
(def *beat-step* 1/8)
(def *light-delay* 0.2)
(def *light-duration* 0.1)
(def *midi-channel* 0)

;; Asynchronouse configuration service, just for fun
(def *options-chan* (make-instance 'channel))
(spawn-named :options
  (let ((opts (list 
                :mode 'sequencer
                :hl-step nil
                :hl-synth nil)))
    (loop
      :do (handler-case
              (match (? *options-chan*)
                     ((list ret :mode)  (! ret (getf opts :mode) 1))
                     ((list ret :hl-synth)  (! ret (getf opts :hl-synth) 1))
                     ((list ret :hl-step)  (! ret (getf opts :hl-step) 1))
                     ((list :mode m)  (setf (getf opts :mode) m))
                     ((list :hl-step step)
                        (setf (getf opts :hl-step) step)
                        (setf (getf opts :mode) 'hl-step))
                     ((list :hl-synth synth)
                        (setf (getf opts :hl-synth) synth)
                        (setf (getf opts :mode) 'hl-synth))
                     ((list :btn-pressed btn)
                        (when (not (getf opts :button))
                          (setf (getf opts :button) btn)) )
                     ((list ret :btn-pressed)
                        (! ret (getf opts :button)))
                     ((list :btn-release)
                        (setf (getf opts :button) nil)))
            (error (c) (writeln "OPTION ERROR: " c))))))
;
(defun get-option (&rest msg)
  (let ((ret-ch (make-instance 'channel)))
    (! *options-chan* (cons ret-ch msg))
    (? ret-ch 1)))
(defun set-option (&rest msg)
  (! *options-chan* msg))

;;;

(defun looper (beat j)
  (loop :for i :from 0 :to (1- *grid-height*)
        :do (when (> (matrix i (mod j *grid-width*)) 0)
              (at-beat beat (apply #'synth (elt *synths* i)))))
  (spawn
    (sleep *light-delay*)
    (loop :for i :from 0 :to 4
          :do (update-pad i j t))
    (sleep *light-duration*)
    (loop :for i :from 0 :to (1- *grid-height*)
          :do (update-pad i j)))
  (clock-add (+ beat *beat-step*) #'looper (+ beat *beat-step*) (1+ j)))


