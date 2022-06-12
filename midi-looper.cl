(load "~/src/cl-music/lib.cl")
(in-package :sc-user)
(named-readtables:in-readtable :sc)
(init)   ;; start new server
(bpm 60)
(ql:quickload :cl-alsaseq)

;;; LOOPER ;;;

(defstruct (midi-recorder (:conc-name mr-))
  (start-beat 0)
  (data nil)
  (updatebeat nil)
  (beats 1)
  (quants 4)
  (recordingp nil))

(defun start-midi-recording (self beats quants)
  (setf (mr-data self) (make-array (* beats quants) :initial-element nil))
  (setf (mr-updatebeat self) (make-array (* beats quants) :initial-element nil))
  (setf (mr-beats self) beats)
  (setf (mr-quants self) quants)
  (setf (mr-start-beat self) (-> (clock-beats) ceiling 1+))
  (setf (mr-recordingp self) t))

(defun clear-midi-prev-beat (self i beat)
  (when-let* ((lastupd (-> self mr-updatebeat (elt i)))
              (is-old (< lastupd (floor beat))))
    (-> self mr-data (elt i) (setf nil))))

(defun stop-midi-recording (self)
  (setf (mr-recordingp self) nil))

(defun continue-midi-recording (self)
  (setf (mr-recordingp self) t))

(defun register-midi-event (self event)
  (when (mr-recordingp self)
    (let* ((beat (mod (- (clock-beats) (mr-start-beat self))
                      (mr-beats self)))
           (i (floor (* (mr-quants self) beat))))
      (clear-midi-prev-beat self i (floor (clock-beats)))
      (-> self mr-updatebeat (elt i) (setf (floor (clock-beats))))
      (push event (elt (mr-data self) i))
      )))

(defun recall-midi-event (self beat q)
  (let ((i (+ q (* (mr-quants self) (mod beat (mr-beats self))))))
    (-> self mr-data (elt i))))

;;; MIDI HANDLER ;;;

(def m (make-midi-recorder))

(start-midi-recording m 4 8)


(mr-updatebeat m)

(let ((i 0))
  (defun gen () (prog1 i (incf i))))

(register-midi-event m (gen))
(register-midi-event m (gen))
(mr-data m)

(recall-midi-event m 1 0)

