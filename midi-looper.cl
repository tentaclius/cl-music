(require "mylisp" "init.cl")
(provide "midi-looper")

(defpackage :midi-looper
  (:use :cl :sc :mylisp)
  (:export :mr-midi-event-type
           :mr-midi-event-note
           :mr-midi-event-velocity
           :mr-midi-event-start
           :mr-midi-event-duration
           :mr-midi-event-channel
           :mr-recordingp
           :start-midi-recording
           :stop-midi-recording
           :continue-midi-recording
           :register-midi-event
           :recall-midi-events
           :print-midi-events
           :alsa-to-my-midi
           :make-midi-track
           :quantize-beat))
(in-package :midi-looper)
(named-readtables:in-readtable :sc)

;; TODO
;; - control events

(defstruct (midi-track (:conc-name mr-))
  (start-beat 0)
  (data nil)
  (updatebeat nil)
  (beats 1)
  (running-notes (hshm))
  (recordingp nil))

(defun quantize-beat (n &optional (q 32))
  (/ (round (* n q)) q))

(defun start-midi-recording (self beats)
  (setf (mr-data self) (make-array beats :initial-element nil))
  (setf (mr-updatebeat self) (make-array beats :initial-element nil))
  (setf (mr-beats self) beats)
  (setf (mr-running-notes self) (hshm))
  (setf (mr-start-beat self) (-> (clock-beats) ceiling))
  (setf (mr-recordingp self) t))

;(defun clear-midi-prev-beat (self i beat)
;  (when-let* ((lastupd (-> self mr-updatebeat (elt i)))
;              (is-old (< lastupd (floor beat))))
;    (-> self mr-data (elt i) (setf nil))))

(defun stop-midi-recording (self)
  (setf (mr-recordingp self) nil))

(defun continue-midi-recording (self)
  (setf (mr-recordingp self) t))

(defun register-midi-event (self event)
  (when (mr-recordingp self)
    (case (mr-midi-event-type event)
      (:note_on
       (setf (mr-midi-event-start event) (clock-beats))
       (-> self mr-running-notes (hset (mr-midi-event-note event) event)))
      (:note_off
       (when-let* ((note (mr-midi-event-note event))
                   (start-event (-> self mr-running-notes (href note)))
                   (start-beat (mr-midi-event-start start-event))
                   (start-i (floor (mod (- start-beat (mr-start-beat self)) (mr-beats self)))))
                  (setf (mr-midi-event-duration start-event)
                        (- (clock-beats) start-beat))
                  (setf (mr-midi-event-start start-event) (- start-beat (floor start-beat)))
                  (-<> self mr-data (elt start-i) (push start-event <>)))))))

(defun recall-midi-events (self beat)
  (let ((i (mod (- beat (mr-start-beat self)) (mr-beats self))))
    (-> self mr-data (elt i))))

(defun print-midi-events (self &optional (q 64))
  (loop :for beat-data :across (mr-data self) :do
        (format t "~s~%" `(sim ,@(mapcar (λ(e) (list 'list
                                                     (mr-midi-event-note e)
                                                     :start (quantize-beat (mr-midi-event-start e) q)
                                                     :dur (quantize-beat (mr-midi-event-duration e) q)
                                                     :amp (/ (mr-midi-event-velocity e) 127)))
                                         (sort (copy-list beat-data) (λ(a b) (< (mr-midi-event-start a) (mr-midi-event-start b)))))))))

