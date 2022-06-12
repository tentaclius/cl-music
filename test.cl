(load "~/src/cl-music/lib.cl")
(in-package :sc-user)
(named-readtables:in-readtable :sc)
(init)   ;; start new server
;(connect)
(bpm 60)
(ql:quickload :cl-alsaseq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO
;; - compressor
;; - granular synthesis
;; - CL FFI, try using libjack midi

(defsynth drone-gain ((hi 0.3) (lo 0.03))
  (-<> (env-gen.kr (perc 0.01 0.8) :act :free)
       (* hi) (+ lo)
       (out.kr (cbus :drone-gain) <>)))

(proxy :drone
       (-<> (saw.ar 70)
            (lpf.ar (range (var-saw.kr 0.1) 70 400))
            (+ (* 0.8 (sin-osc.ar (* 3 70))))
            (* (in.kr (cbus :drone-gain)))
            ;(* 0.3)
            pan2.ar))

(synth 'drone-gain)

(release :drone)

(defpattern drums
  (play-drum)
  (λ(j)
    (let ((o nil)
          (i 'drone-gain))
      (sim (per-beat j
             (seq i o i o o i o o)
             (seq o o o o o o o o)
             (seq i o o o o i o o)
             (seq o o o o o o o o))))))

(drums :start)
(drums :stop)

(defpattern drumss
  (play-drum)
  (λ(i)
    (let ((o nil)
          (b ['bd :dur 0.07 :freq 200]))
      (sim (per-beat i
             (seq b)
             (sim b 'snare))))))

(drumss :start)
(drumss :stop)


(defsynth ssin ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (sin-osc.ar fq)
         (+ (* 1/2 (sin-osc.ar (* fq 1.5))))
         (+ (* 1/15 (sin-osc.ar (* fq 3))))
         (* amp (env-gen.kr (perc 0.001 0.4) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(defpattern ssin
  (play-note 'ssin :note-fn (λ(n) [:freq (midicps (+ 54 (sc *minor* n)))]))
  (λ(i)
    (let ((o nil))
      (sim
        (per-beat i
                  (seq 0 2 3 (rand-el 0 3 7))
                  (seq 0 2 3 (rand-el 0 3 7 5) 3 2))
        (per-beat i
                  (seq  0 o o 0)
                  (seq  o o 2 o )
                  )))))

(ssin :start)
(ssin :stop)

(test :stop)


;;; Arpegios
(def arp (gen-list [0 2 4 6 4  7 9 11 13 11  14 16 18 20 18  21 23 25 27]))

(def *root-note* 42)
(def *root-step* 0)
(def *arp-delay* 1/8)

(def *arp-running* nil)

(def *arp-running* t)
(spawn (loop :while *arp-running* :do
             (pl 'ssin [:freq (-<> (funcall arp) (+ *root-step*) (sc *minor* <>) (+ *root-note*) midicps)] *arp-delay*) (sleep *arp-delay*)))

(defun pl (snt attrs dur)
  (spawn (let ((s (apply #'synth (cons snt attrs))))
           (sleep dur)
           (release s))))


;;; Continuous synths

(defsynth wah ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (dyn-klang.ar [[fq (* fq 2) (* fq 3) (* fq 4) (* fq 5) (* fq 6)]
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

(midihelper:start-midihelper :master 96 'midi-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System

(server-query-all-nodes)
(stop)
