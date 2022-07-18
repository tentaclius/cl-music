(load "~/src/cl-music/lib.cl")
(in-package :sc-user)
(named-readtables:in-readtable :sc)
(init)   ;; start new server
(bpm 60)
(ql:quickload :cl-alsaseq)


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
;
(defsynth bd ((bass 40) (d 0.3) (out 0) (amp 0.5))
  (let ((freq (+ 40 (* 1000 (in.kr (cbus :bd-freq)))))
        (dur  (* 0.5 (in.kr (cbus :bd-dur)))))
    (-<> (x-line.ar freq bass (in.kr (cbus :bd-dur)))
         (sin-osc.ar)
         (* amp (env-gen.kr (env [0 1 1 0] [0.0001 dur d]) :act :free))
         pan2.ar (out.ar out <>))))
;
(defsynth hh ((out 0) (amp 0.3) (dur 0.1))
  (-<> (white-noise.ar)
       (hpf.ar 8000)
       (* amp (env-gen.kr (perc 0.0 dur) :act :free))
       pan2.ar (out.ar out <>)
    ))
;
(defsynth snare ((freq 1100) (amp 0.3)
              (out 0) (gate 1)
              (a 0.001) (d 0.2))
  (-<> (white-noise.ar)
       (lpf.ar freq)
       (* amp (env-gen.kr (perc a d) :gate gate :act :free))
       pan2.ar (out.ar out <>)))

(def *running-synths* (hshm))
(def *scales-conv* (hshm  112 :bd-dur  114 :bd-freq  75 :synth-lpf  72 :synth-res  93 :synth-release  73 :synth-mod  79 :synth-mod-fq))
(def *drumpads* (hshm  36 ['bd]  37 ['snare]  38 ['hh]))

;(def *scales-conv* (hshm  112 nil  74 nil  71 nil  76 nil  77 nil  93 nil  73 nil  75 nil
;                          114 nil  18 nil  19 nil  16 nil  17 nil  91 nil  79 nil  72 nil))

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
          (:SND_SEQ_EVENT_CONTROLLER
           (let* ((param (getf event-data 'cl-alsaseq:param))
                  (value (getf event-data 'calispel:value))
                  (scl-n (href *scales-conv* param)))
             (control-set (cbus scl-n) (float (/ value 127)))
             (writeln param " " scl-n " " (float (/ value 127)))
             ))
          ;; Note On
          (:SND_SEQ_EVENT_NOTEON
           (let ((note (getf event-data 'cl-alsaseq:note))
                 (velo (getf event-data 'cl-alsaseq:velocity)))
             (writeln "NOTE_ON " note " " velo)
             (if (and (>= note 36) (<= note 43))
                 (when-let ((snt (href *drumpads* note)))
                           (apply #'synth snt))
                 (progn (when-let ((s (href *running-synths* note))) (release s))
                        (hset *running-synths* note (synth 'synth :freq (midicps note) :amp (/ velo 127)))))))
          ;; Note Off
          (:SND_SEQ_EVENT_NOTEOFF
           (let ((note (getf event-data 'cl-alsaseq:note)))
             (if (and (>= note 36) (<= note 43))
                 nil
                 (when-let ((s (href *running-synths* note)))
                           (release s)
                           (hset *running-synths* note nil)))))
          (:SND_SEQ_EVENT_PITCHBEND
           (let ((bend (getf event-data 'calispel:value)))
             (control-set (cbus :pitch-bend) (float (/ bend 8192))))))))
    (error (c) (writeln "ERROR: " c))))

(midihelper:start-midihelper :master 96 'midi-map)

(stop)
