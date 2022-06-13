(load "~/src/cl-music/lib.cl")
(in-package :sc-user)
(named-readtables:in-readtable :sc)
(init)   ;; start new server
(bpm 60)
(ql:quickload :cl-alsaseq)

;; TODO
;; - Need to track individual notes length in the looper and store the notes using an internal representation
;;   (note velocity duration)

;;; LOOPER ;;;

(defstruct mr-midi-event
  type
  value
  velocity
  start
  duration)

(defstruct (midi-recorder (:conc-name mr-))
  (start-beat 0)
  (data nil)
  (updatebeat nil)
  (beats 1)
  (quants 4)
  (running-notes (hshm))
  (recordingp nil))

(defun start-midi-recording (self beats quants)
  (setf (mr-data self) (make-array (* beats quants) :initial-element nil))
  (setf (mr-updatebeat self) (make-array (* beats quants) :initial-element nil))
  (setf (mr-beats self) beats)
  (setf (mr-quants self) quants)
  (setf (mr-running-notes self) (hshm))
  (setf (mr-start-beat self) (-> (clock-beats) ceiling))
  (setf (mr-recordingp self) t))
;
(defun clear-midi-prev-beat (self i beat)
  (when-let* ((lastupd (-> self mr-updatebeat (elt i)))
              (is-old (< lastupd (floor beat))))
    (-> self mr-data (elt i) (setf nil))))
;
(defun stop-midi-recording (self)
  (setf (mr-recordingp self) nil))
;
(defun continue-midi-recording (self)
  (setf (mr-recordingp self) t))

(defun register-midi-event (self event)
  (when (mr-recordingp self)
    (let* ((beat (mod (- (clock-beats) (mr-start-beat self))
                      (mr-beats self)))
           (i (mod (round (* (mr-quants self) beat))
                   (* (mr-quants self) (mr-beats self)))))
      ;(clear-midi-prev-beat self i (floor (clock-beats)))
      ;(-> self mr-updatebeat (elt i) (setf (floor (clock-beats))))
      (case (mr-midi-event-type event)
        (:note_on
         (setf (mr-midi-event-start event) (clock-beats))
         (-> self mr-running-notes (hset (mr-midi-event-value event) event)))
        (:note_off
         (when-let* ((note (mr-midi-event-value event))
                     (start-event (-> self mr-running-notes (href note)))
                     (start-beat (mr-midi-event-start start-event))
                     (start-i (floor (* (mr-quants self)
                                        (mod (- start-beat (mr-start-beat self))
                                             (mr-beats self))))))
           (setf (mr-midi-event-duration start-event)
                 (- (clock-beats) start-beat))
           (-<> self mr-data (elt start-i) (push start-event <>))))
        (:control
         (-<> self mr-data (elt i) (push event <>)))))))

(defun recall-midi-events (self beat q)
  (let ((i (+ q (* (mr-quants self) (mod (- beat (mr-start-beat self)) (mr-beats self))))))
    (-> self mr-data (elt i))))

;;; IN A RUNNING MIDI ;;;

(def mr (make-midi-recorder))

(def *running-synths* (hshm))
(def *scales-conv* (hshm  112 :bd-dur  114 :bd-freq  75 :synth-lpf  72 :synth-res  93 :synth-release  73 :synth-mod  79 :synth-mod-fq))
(def *drumpads* (hshm  36 ['bd]  37 ['snare]  38 ['hh]))

(defsynth synth ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.001) (d 0.2) (s 0.7) (r 0.5))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (saw.ar fq)
         ;(+ (* 1/3 (saw.ar (+ fq 3))))
         (rlpf.ar (+ 40 (* 2000 (in.kr (cbus :synth-lpf)))) (in.kr (cbus :synth-res)))
         (* (range (sin-osc.kr (* 40 (+ 0 (in.kr (cbus :synth-mod-fq))))) (in.kr (cbus :synth-mod)) 1))
         (* amp (env-gen.kr (adsr a d s (* 0.5 (in.kr (cbus :synth-release)))) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(defsynth ssin ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (sin-osc.ar fq)
         (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(defun convert-alsa-to-my-midi (message)
  (let* ((event-type (getf message :event-type))
         (event-data (getf message :event-data)))
    (case event-type
      (:SND_SEQ_EVENT_CONTROLLER
       (let ((param (getf event-data 'cl-alsaseq:param))
             (value (getf event-data 'calispel:value)))
         (make-mr-midi-event :type :control
                             :value param
                             :velocity value)))
      (:SND_SEQ_EVENT_NOTEON
       (let ((note (getf event-data 'cl-alsaseq:note))
             (velo (getf event-data 'cl-alsaseq:velocity)))
         (make-mr-midi-event :type :note_on
                             :value note
                             :velocity velo)))
      (:SND_SEQ_EVENT_NOTEOFF
       (let ((note (getf event-data 'cl-alsaseq:note)))
         (make-mr-midi-event :type :note_off
                             :value note))))))

(defun midi-handler (message)
  (handler-case 
      (case (mr-midi-event-type message)
        (:note_on
         (hset *running-synths* (mr-midi-event-value message)
               (if-let ((dur (mr-midi-event-duration message))
                        (beat (mr-midi-event-start message)))
                 (dur beat dur ['ssin :freq (midicps (mr-midi-event-value message)) :amp (/ (mr-midi-event-velocity message) 127)])
                 (synth 'ssin :freq (midicps (mr-midi-event-value message)) :amp (/ (mr-midi-event-velocity message) 127)))))
        (:note_off
         (when-let ((snt (href *running-synths* (mr-midi-event-value message))))
                   (release snt))))
    (error (c) (writeln "ERROR: " c))))

(defun midi-map (messages)
  (dolist (message messages)
    (let ((msg (convert-alsa-to-my-midi message)))
      (midi-handler msg)
      (register-midi-event mr msg))))

(midihelper:start-midihelper :master 96 'midi-map)

(midihelper:stop-midihelper)

;;; TESTS ;;;

(defun midi-player (beat i)
  (let ((quantization 24)) (when (not (mr-recordingp mr))
    (start-midi-recording mr 4 quantization))
  (loop :for j :from 0 :to (1- quantization) :do
        (dolist (m (recall-midi-events mr i j))
          (setf (mr-midi-event-start m) (+ beat (/ j quantization)))
          (midi-handler m)
          (writeln "MIDI: " m " beat: " beat)
          )))
  (clock-add (1+ beat) #'midi-player (1+ beat) (1+ i)))

(recall-midi-events mr 0 0)

(def mr (make-midi-recorder))
(midi-player (clock-quant 4) 0)
(drums :start 4)

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil))
      (sim (per-beat i
             (seq 'hh))))))

(drums :stop)

(mr-data mr)

(stop)
