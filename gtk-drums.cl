(load "~/src/cl-music/lib.cl")
(ql:quickload :cl-cffi-gtk)
(in-package :sc-user)
(use-package :gtk)
(use-package :gdk)
(use-package :gdk-pixbuf)
(use-package :gobject)
(use-package :glib)
(use-package :gio)
(use-package :pango)
(use-package :cairo)
(use-package :common-lisp)
(use-package :sc)
(use-package :sc-user)
(use-package :sc-extensions)
(use-package :bdef)
(named-readtables:in-readtable :sc)
(init)   ;; start new server
(bpm 60)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cbus-set :bd-hi 400)
(cbus-set :bd-lo 40)
(cbus-set :bd-dur 0.2)
(cbus-set :snare-fq 2000)
(cbus-set :snare-dur 0.2)

(defsynth bd ((d 0.3) (out 0) (amp 0.5))
  (-<> (x-line.ar (cbus-in :bd-hi) (cbus-in :bd-lo) (cbus-in :bd-dur))
       (sin-osc.ar)
       (* amp (env-gen.kr (env [0 1 1 0] [0.0001 (cbus-in :bd-dur) d]) :act :free))
       pan2.ar (out.ar out <>)))
;
(defsynth hh ((out 0) (amp 0.3) (dur 0.1))
  (-<> (white-noise.ar)
       (hpf.ar 8000)
       (* amp (env-gen.kr (perc 0.0 dur) :act :free))
       pan2.ar (out.ar out <>)))
;
(defsynth snare ((amp 0.3) (out 0) (gate 1))
  (-<> (white-noise.ar)
       (lpf.ar (cbus-in :snare-fq))
       (* amp (env-gen.kr (perc 0.005 (cbus-in :snare-dur)) :gate gate :act :free))
       pan2.ar (out.ar out <>)))
;
(defsynth ssin ((freq 440) (freq0 440) (slide 0) (amp 0.3) (out 0))
  (-<> (dyn-klang.ar [[freq (* 1.5 freq)] [0.7 0.4]]) 
       (* amp (env-gen.kr (perc 0.007 1) :act :free))
       pan2.ar (out.ar out <>)))

(synth 'ssin :freq (midicps 60))

;; piano keyboard
;(def *key-map* (let ((table (make-hash-table :test 'equal)))
;                 (loop :for c :across "zsxdcvgbhnjm,l.;/" :for i :from 0
;                       :do (setf (gethash (string c) table) i))
;                 (loop :for c :across "q2w3er5t6y7ui9o0p[=" :for i :from 12
;                       :do (setf (gethash (string c) table) i))
;                 (-> "=" (gethash table)
;                     (setf (λ(&optional key)
;                             (when (< *root-note* 60) (setf *root-note* (+ *root-note* 12))))))
;                 (-> "-" (gethash table)
;                     (setf (λ(&optional key)
;                             (when (> *root-note* 20) (setf *root-note* (- *root-note* 12))))))
;                 table))

(def *root-note* 60)
(def *running-synths* (hshm))
(def *key-map* (hshm 44 (λ() (synth 'bd))
                     108 (λ() (synth 'bd))
                     46 (λ() (synth 'snare))
                     59 (λ() (synth 'snare))
                     47 (λ() (synth 'hh))
                     39 (λ() (synth 'hh))
                     122 (λ() (synth 'ssin :freq (midicps 60)))
                     97 (λ() (synth 'ssin :freq (midicps 63)))
                     113 (λ() (synth 'ssin :freq (midicps 65)))))

(synth 'ssin :freq (midicps 60))

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
