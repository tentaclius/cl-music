(load "~/src/cl-music/lib.cl")
(in-package :sc-user)
(named-readtables:in-readtable :sc)
(init)   ;; start new server
(bpm 60)
(ql:quickload :cl-alsaseq)

;;;; Synth
(defsynth wah ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> ;(sin-osc.ar)
         (dyn-klang.ar [[fq (* fq 2) (* fq 3) (* fq 4) (* fq 5) (* fq 6)]
                        [(in.kr (cbus :osc1)) (in.kr (cbus :osc2)) (in.kr (cbus :osc3)) (in.kr (cbus :osc4)) (in.kr (cbus :osc5)) (in.kr (cbus :osc6))]])
         (* (line.kr 0 1 4) amp (env-gen.kr (adsr 2 1/2 1 3) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(def *running-synths* (hshm))
(def *pressed-keys*   0)
(def *scales-conv*    (hshm  112 :osc1  74 :osc2  71 :osc3  76 :osc4  77 :osc5  93 :osc6))

(defun midi-map (messages)
  (handler-case
    (dolist (message messages)
      (let* ((event-type (getf message :event-type))
             (event-data (getf message :event-data))
             (source (car (getf message :source)))
             (destination (car (getf message :dest))))
        (declare (ignorable source destination))
        (format t "~a: ~s~%" event-type event-data)
        (case event-type
          ;; Control event
          (:snd_seq_event_controller
           (when-let* ((param (getf event-data 'cl-alsaseq:param))
                       (value (getf event-data 'calispel:value))
                       (scl-n (href *scales-conv* param)))
             (control-set (cbus scl-n) (float (/ value 127)))
             (writeln param " " scl-n " " (float (/ value 127)))))
          ;; Note On
          (:snd_seq_event_noteon
           (let ((note (getf event-data 'cl-alsaseq:note))
                 (velo (getf event-data 'cl-alsaseq:velocity)))
             (when (<= *pressed-keys* 0)
               (loop for key being the hash-keys of *running-synths* do
                     (release (href *running-synths* key)))
               (setf *running-synths* (hshm)))
             (hset *running-synths* note (synth 'wah :freq (midicps note) :amp (/ velo 127)))
             (incf *pressed-keys*)))
          ;; Note Off
          (:snd_seq_event_noteoff
           (let ((note (getf event-data 'cl-alsaseq:note)))
             (decf *pressed-keys*)))
          (:SND_SEQ_EVENT_PITCHBEND
           (let ((bend (getf event-data 'calispel:value)))
             (control-set (cbus :pitch-bend) (float (/ bend 8192))))))))
    (error (c) (writeln "ERROR: " c))))

(release s)
(def s (synth 'wah))

(midihelper:start-midihelper :master 96 'midi-map)

(stop)
