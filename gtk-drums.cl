(load "init.cl")
(defpackage :play (:use :cl :sc :mylisp))
(in-package :play)
(use-gtk)
(sc-init)
(clock-bpm 60)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def sample-bank
  (->> (list
          "~/Mus/samples/selection1/000_bas2.wav"
          "~/Mus/samples/selection1/000_Tom Clap.wav"
          "~/Mus/samples/selection1/001_011112-melody.wav"
          "~/Mus/samples/selection1/001_Tom Crash.wav"
          "~/Mus/samples/selection1/003_abt.wav"
          "~/Mus/samples/selection1/003_cymrev.wav"
          "~/Mus/samples/selection1/004_2.wav"
          "~/Mus/samples/selection1/004_fan.wav"
          "~/Mus/samples/selection1/004_Tom Openhat.wav"
          "~/Mus/samples/selection1/005_tankeng.wav"
          "~/Mus/samples/selection1/007_microsound.wav"
          "~/Mus/samples/selection1/011_9.wav"
          "~/Mus/samples/selection1/01.wav"
          "~/Mus/samples/selection1/1.wav"
          "~/Mus/samples/selection1/23645_loofa_A_001.wav"
          "~/Mus/samples/selection1/2.wav"
          "~/Mus/samples/selection1/3.wav"
          "~/Mus/samples/selection1/bd.wav"
          "~/Mus/samples/selection1/crash.wav"
          "~/Mus/samples/selection1/CSHD0.wav"
          "~/Mus/samples/selection1/hh.wav"
          "~/Mus/samples/selection1/kick.wav"
          "~/Mus/samples/selection1/rave_choir01.ogg"
          "~/Mus/samples/selection1/rytm-00-hard.wav"
          "~/Mus/samples/selection1/rytm-01-classic.wav"
          "~/Mus/samples/selection1/s-bd.wav"
          "~/Mus/samples/selection1/s-ding.WAV"
          "~/Mus/samples/selection1/snare.wav"
          "~/Mus/samples/selection1/VEC1 BD Distortion 06.wav"
          "~/Mus/samples/selection1/VEC1 BD Distortion 39.wav")
       (mapcar #'buffer-read)))

(def sample-bank
  (vector
    (buffer-read "/home/aerdman/Mus/samples/sequential/007_Tom Tom2.wav")
    (buffer-read "/home/aerdman/Mus/samples/sequential/002_Tom Hat Closed.wav")
    (buffer-read "/home/aerdman/Mus/samples/sequential/006_Tom Tom1.wav")
    (buffer-read "/home/aerdman/Mus/samples/sequential/003_Tom Kick.wav")
    (buffer-read "/home/aerdman/Mus/samples/sequential/000_Tom Clap.wav")
    (buffer-read "/home/aerdman/Mus/samples/sequential/005_Tom Snare.wav")
    (buffer-read "/home/aerdman/Mus/samples/sequential/004_Tom Openhat.wav")
    (buffer-read "/home/aerdman/Mus/samples/sequential/001_Tom Crash.wav")
    ))

(def sample-bank
  (->> (list
         "/home/aerdman/Mus/samples/sequential/007_Tom Tom2.wav"
         "/home/aerdman/Mus/samples/sequential/002_Tom Hat Closed.wav"
         "/home/aerdman/Mus/samples/sequential/006_Tom Tom1.wav"
         "/home/aerdman/Mus/samples/sequential/003_Tom Kick.wav"
         "/home/aerdman/Mus/samples/sequential/000_Tom Clap.wav"
         "/home/aerdman/Mus/samples/sequential/005_Tom Snare.wav"
         "/home/aerdman/Mus/samples/sequential/004_Tom Openhat.wav"
         "/home/aerdman/Mus/samples/sequential/001_Tom Crash.wav")
       (mapcar #'buffer-read)))

;;;

(defsynth snare ((freq 1100) (amp 0.3)
              (out 0) (gate 1)
              (a 0.001) (d 0.2))
  (-<> (white-noise.ar)
       (lpf.ar freq)
       (* amp (env-gen.kr (perc a d) :gate gate :act :free))
       pan2.ar (out.ar out <>)))
;
(defsynth ssin ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (sin-osc.ar fq)
         (+ (* 1/2 (sin-osc.ar (* fq 1.5))))
         (+ (* 1/10 (sin-osc.ar (* fq 3))))
         (* amp (env-gen.kr (perc 0.001 0.4) :gate gate :act :free))
         pan2.ar (out.ar out <>))))


(def *root-note* 60)
(def *running-synths* (hshm))
(def *key-map* (hshm 44  (λ() (synth 'bd))
                     108 (λ() (synth 'bd))
                     46  (λ() (synth 'snare))
                     59  (λ() (synth 'snare))
                     47  (λ() (synth 'hh))
                     39  (λ() (synth 'hh))
                     122 (λ() (synth 'ssin :freq (midicps 60)))
                     97  (λ() (synth 'ssin :freq (midicps 63)))
                     113 (λ() (synth 'ssin :freq (midicps 65)))))

(def *key-map* (hshm))
(loop :for key :in (list 122 120 99 118 98 110 109 44 46 47 97 115 100 102 103 104 106 107 108 59 39 92 113 119 101 114
                         116 121 117 105 111 112 91 93 49 50 51 52 53 54 55 56 57 48 45 61)
      :for sample :in sample-bank
      :do (hset *key-map* key (let ((s sample) (k key)) (λ() (csnt k (synth 'sample-1 :buffer s))))))

(defun on-key-press (self event)
  (declare (ignorable self))
  (handler-case
      (let ((key (-> event gdk-event-key-keyval (gethash *key-map*))))
        (typecase key
          (function (funcall key))
          (number (when (not (gethash key *running-synths*))
                    (hset *running-synths* key (synth 'key-pad-synth :freq (midicps (+ *root-note* key)))))))
        (writeln "KEY-PRESS " (gdk-event-key-keyval event) " " (gdk-event-key-string event)))
    (error (c) (writeln c))))
;
(defun on-key-release (self event)
  (declare (ignorable self))
  (handler-case
      (-<> event
           gdk-event-key-string
           (gethash *key-map*)
           (gethash *running-synths*)
           (progn (when <> (release <>))
                  (setf <> nil)))
    (error (c) (writeln c))))
;
(defstruct scale-descr
  (name "")
  (range-low 0)
  (range-high 100)
  (value 0)
  (switch nil)
  (setter nil))
;
(def *scales*
  (labels ((sc (title low high default switch setter)
             (make-scale-descr :name title :range-low low :range-high high :value default :switch switch :setter setter)))
    (list (sc "BD hi" 100 1000 400 nil (λ(x) (cbus-set :bd-hi x)))
          (sc "BD lo" 20 400 40 nil (λ(x) (cbus-set :bd-lo x)))
          (sc "BD dur 1/1000" 1 500 20 nil (λ(x) (cbus-set :bd-dur (/ x 1000))))
          (sc "Snare fq" 100 10000 2000 nil (λ(x) (cbus-set :snare-fq x)))
          (sc "Snare dur 1/1000" 1 1000 200 nil (λ(x) (cbus-set :snare-dur (/ x 1000)))))))

(defun hello-world ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window :type :toplevel
                                 :title "hello world"
                                 :default-width 250 :border-width 12))
          (grid (gtk-grid-new)))
      ;;
      ;; Init
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gobject:g-signal-connect window "key-press-event" #'on-key-press)
      (gobject:g-signal-connect window "key-release-event" #'on-key-release)
      ;;
      ;; Elements
      (loop :for descr :in *scales* :for row :from 0
            :do (let ((label (gtk-label-new (scale-descr-name descr)))
                      (check (when (scale-descr-switch descr) (gtk-check-button-new)))
                      (scale (gtk-scale-new-with-range :horizontal
                                                       (scale-descr-range-low descr)
                                                       (scale-descr-range-high descr)
                                                       0.1)))
                  (gtk-range-set-value scale (scale-descr-value descr))
                  (gtk-grid-attach grid label 0 row 1 1)
                  (when check (gtk-grid-attach grid check 1 row 1 1))
                  (gtk-grid-attach grid scale 2 row 1 1)
                  (setf (gtk-widget-size-request scale) '(300 25))
                  (let ((setter (scale-descr-setter descr)))
                    (when setter (g-signal-connect scale "value-changed"
                                                   (lambda (self)
                                                     (funcall setter (gtk-range-get-value self))))))))
      ;;
      ;; Pack and go
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))

(hello-world)

(stop)
