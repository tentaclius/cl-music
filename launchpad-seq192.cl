(equire "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :lp-seq192 (:use :cl :sc :mylisp :midi-looper))
(in-package :lp-seq192)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)

; 64 65 66 67  96 97 98 99
; 60 61 62 63  92 93 94 95
; 56 57 58 59  88 89 90 91
; 52 53 54 55  84 85 86 87
; 48 49 50 51  80 81 82 83
; 44 45 46 47  76 77 78 79
; 40 41 42 43  72 73 74 75
; 36 37 38 39  68 69 70 71

(def *osc-dev* (sc-osc:osc-device "localhost" 11113))
;
(def 
  *record-pad* 71
  *record-btn-color* 7
  *color-on* 3
  *color-off* 111
  *color-rec-on* 70
  *color-rec-off* 5
  *color-none* 0
  *midi-channel* 0)
;
(def
  *rec-pressed* nil
  *recording-p* nil
  *pad-status* (make-hash-table)
  *recording* nil)
(def pad-list (vector
    64 65 66 67  96 97 98 99
    60 61 62 63  92 93 94 95
    56 57 58 59  88 89 90 91
    52 53 54 55  84 85 86 87
    48 49 50 51  80 81 82 83
    44 45 46 47  76 77 78 79
    40 41 42 43  72 73 74 75
    36 37 38 39  68 69 70 71))
(def *pad-map* (make-hash-table))
(loop :for pad :across pad-list :for i :from 0
      :do (hset *pad-map* pad (cons (mod i 8) (floor (/ i 8)))))

(defun light-pad (pad color)
  (midi-note-on *midi-handle* pad color))
;
(defun midi-map (msg)
  (let ((note (mr-midi-event-note msg))
        (velo (mr-midi-event-velocity msg))
        (chan (mr-midi-event-channel msg)))
    (declare (ignorable chan))
    (case (mr-midi-event-type msg)
      (:note_on
       (cond
         ;; stop recording
         ((and (= note *record-pad*) *recording* (> velo 0))
          (sc-osc:send-message *osc-dev* "/sequence" "record_off")
          (light-pad *recording* (if (href *pad-status* *recording*)
                                     *color-on* *color-off*))
          (setf *recording* nil))
         ;; rec button pressed
         ((and (= note *record-pad*))
          (setf *rec-pressed* (> velo 0)))
         ;; select pad for recording
         ((and *rec-pressed*)
          (let ((coord (href *pad-map* note)))
            (sc-osc:send-message *osc-dev* "/sequence" "record_on" (car coord) (cdr coord))
            (light-pad note (if (href *pad-status* note)
                                *color-rec-on*
                                *color-rec-off*))
            (setf *recording* note)
            (setf *rec-pressed* nil)))
         ;; turn a pattern on or off
         ((> velo 0)
          (let ((coord (href *pad-map* note))
                (pad (href *pad-status* note)))
            (sc-osc:send-message *osc-dev* "/sequence" (if pad "off" "on") (car coord) (cdr coord))
            (light-pad note (if (equal *recording* note)
                                (if pad *color-rec-off* *color-rec-on*)
                                (if pad *color-off* *color-on*)))
            (hset *pad-status* note (not pad))))))
      (:note_off)
      (:control))))

;(defun midi-helper-map (messages)
;  (dolist (message messages)
;    (when-let ((msg (alsa-to-my-midi message)))
;              (midi-map msg))))
;(midihelper:start-midihelper :master 96 'midi-helper-map)

(def *midi-handle* (mk-midi-reader "launchpad-seq192"))
(start-midi-reader *midi-handle* 'midi-map)
(start-midi-writer *midi-handle*)

(light-pad *record-pad* *record-btn-color*)
