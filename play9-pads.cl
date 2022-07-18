(load "~/src/cl-music/lib.cl")
(in-package :sc-user)
(named-readtables:in-readtable :sc)
(init)   ;; start new server
(bpm 60)
(ql:quickload :cl-alsaseq)

(def *running-synths* (hshm))
(def *trigger-map* (make-array 8 :initial-element nil))
(def *scale-map* (make-array 8 :initial-element 0))
(def *scales-conv* (hshm 114 0  18 1  19 2  16 3  17 4  91 5  79 6  72 7 
                         112 :attack  74 :decay  71 :sustain  76 :release
                         77 :lpf  93 :resonance))

(cbus-set :attack 1)
(cbus-set :decay 40)
(cbus-set :sustain 70)
(cbus-set :release 40)
(cbus-set :lpf 127)
(cbus-set :resonance 127)

(defsynth ssw ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (saw.ar fq)
         (rlpf.ar (+ 50 (* 3000/127 (in.kr (cbus :lpf)))) (* 1/127 (in.kr (cbus :resonance))))
         (* amp (env-gen.kr (adsr (/ (in.kr (cbus :attack)) 127) 
                                  (/ (in.kr (cbus :decay)) 127)
                                  (/ (in.kr (cbus :sustain)) 127)
                                  (/ (in.kr (cbus :release)) 127)) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(synth 'ssw)

(defpattern ssw
  (play-note 'ssw
             :release t
             :attr []
             :note-fn (λ(n) [:freq (midicps (+ 57 -12 -12 (sc *pentatonic* n)))]))
  (λ(n)
    ;(writeln *scale-map*)
    (per-beat n
              (seql (loop :for i :from 0 :to 3 :collect (if (elt *trigger-map* i) (-> (elt *scale-map* i) (/ 6) floor) nil)))
              (seql (loop :for i :from 4 :to 7 :collect (if (elt *trigger-map* i) (-> (elt *scale-map* i) (/ 6) floor) nil))))))

(ssw :start)
(ssw :stop)

(defsynth synth ((freq 440) (amp 0.3) (out 0) (gate 1)
              (a 0.008) (d 0.2) (s 0.7) (r 0.5))
  (let ((fq (* 1/4 (+ freq (* 20 (in.kr (cbus :pitch-bend)))))))
    (-<> (dyn-klang.ar [[fq (* fq 3) (* fq 5)] [1/2 1/4 1/7]])
         (* 1/3 amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(defun midi-map (messages)
  (handler-case
    (dolist (message messages)
      (let* ((event-type (getf message :event-type))
             (event-data (getf message :event-data))
             (source (car (getf message :source)))
             (destination (car (getf message :dest))))
        (declare (ignorable source destination))
        (case event-type
          ;; Control event
          (:snd_seq_event_controller
           (let* ((param (getf event-data 'cl-alsaseq:param))
                  (value (getf event-data 'calispel:value))
                  (scl-n (href *scales-conv* param)))
             (writeln "CTRL " scl-n " " value)
             (typecase scl-n
               (keyword  (control-set (cbus scl-n) value))
               (number (setf (elt *scale-map* scl-n) value)))))
          ;; Note On
          (:snd_seq_event_noteon
           (let ((note (getf event-data 'cl-alsaseq:note))
                 (velo (getf event-data 'cl-alsaseq:velocity)))
             (if (and (>= note 36) (<= note 43))
                 (setf (elt *trigger-map* (- note 36)) t)
                 (progn (when-let ((s (href *running-synths* note))) (release s))
                        (hset *running-synths* note (synth 'synth :freq (midicps note) :amp (/ velo 127)))))))
          ;; Note Off
          (:snd_seq_event_noteoff
           (let ((note (getf event-data 'cl-alsaseq:note)))
             (if (and (>= note 36) (<= note 43))
                 (setf (elt *trigger-map* (- note 36)) nil)
                 (when-let ((s (href *running-synths* note)))
                           (release s)
                           (hset *running-synths* note nil)))))
          (:SND_SEQ_EVENT_PITCHBEND
           (let ((bend (getf event-data 'calispel:value)))
             (control-set (cbus :pitch-bend) (float (/ bend 8192))))))))
    (error (c) (writeln "ERROR: " c))))


(midihelper:stop-midihelper)
(midihelper:start-midihelper :master 96 'midi-map)

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil) (d ['bd :dur 0.03]))
      (sim (per-beat i
             (seq d d))))))

(drums :start)
(drums :stop)

(stop)
