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

(def key-pad-synth-bus (bus-audio :chanls 2))

(proxy :test
       (-<> (sin-osc.ar 60)
            (clip (- 0 (in.kr (cbus :clip))) (in.kr (cbus :clip)))
            (/ (in.kr (cbus :clip)))
            (fold (- 0 (in.kr (cbus :fold))) (in.kr (cbus :fold)))
            (/ (in.kr (cbus :fold)))
            (wrap (- 0 (in.kr (cbus :wrap))) (in.kr (cbus :wrap)))
            (/ (in.kr (cbus :wrap)))
            (* (in.kr (cbus :amp)) (env-gen.kr (perc 0.007 0.7) :gate (sin-osc.kr 1)))
            pan2.ar
            ))

(release :test)

(proxy :key-pad-synth-fx
       (-<> (in.ar key-pad-synth-bus 2)
            ;(* (range (sin-osc.kr 10) 0.6 1))  ;; wobble
            ;(freeverb.ar)
            ) :pos :tail)

(def lpf-coeff (bus-control)
     lpf-res (bus-control)
     keypad-attack (bus-control)
     keypad-decay  (bus-control)
     keypad-sustain (bus-control)
     keypad-release (bus-control))
(control-set (busnum lpf-coeff) 20)
(control-set (busnum lpf-res) 0.5)
(control-set (busnum keypad-attack) 90)
(control-set (busnum keypad-decay) 200)
(control-set (busnum keypad-sustain) 600)
(control-set (busnum keypad-release) 200)

(defsynth key-pad-synth ((gate 1) (freq 440) (amp 0.5) (out key-pad-synth-bus))
  (-<> (saw.ar freq)
       (rlpf.ar (* freq (in.kr lpf-coeff)) (in.kr lpf-res))
       (* amp (env-gen.kr (adsr (/ (in.kr keypad-attack) 1000)
                                (/ (in.kr keypad-decay) 1000)
                                (/ (in.kr keypad-sustain) 1000)
                                (/ (in.kr keypad-release) 1000))
                          :gate gate :act :free))
       pan2.ar (out.ar out <>)
       ))

(proxy :key-pad-fx
       (-<> (in.ar (abus :keypad) 2)
            (greyhole.ar)
            (* 2)
            ))
(defsynth key-pad-synth ((gate 1) (freq 440) (amp 0.9) (out (abus :keypad)))
  (-<> (saw.ar freq)
       (+ (* 1/20 (pulse.ar (* 1/2 freq))))
       (+ (* 1/6 (bpf.ar (brown-noise.ar) freq 0.1)))
       (rlpf.ar (* freq 3) (line.ar 0.01 0.3 0.2))
       (* amp 1/2 (env-gen.kr (adsr 0.002 0.1 0.8 0.2) :act :free :gate gate))
       pan2.ar (out.ar out <>)))

(def *running-synths* {})
(def *root-note* 50)

;; piano keyboard
(def *key-map* (let ((table (make-hash-table :test 'equal)))
                 (loop :for c :across "zsxdcvgbhnjm,l.;/" :for i :from 0
                       :do (setf (gethash (string c) table) i))
                 (loop :for c :across "q2w3er5t6y7ui9o0p[=" :for i :from 12
                       :do (setf (gethash (string c) table) i))
                 (-> "=" (gethash table)
                     (setf (λ(&optional key)
                             (when (< *root-note* 60) (setf *root-note* (+ *root-note* 12))))))
                 (-> "-" (gethash table)
                     (setf (λ(&optional key)
                             (when (> *root-note* 20) (setf *root-note* (- *root-note* 12))))))
                 table))

;; harmony keyboard
(def *key-map* (let ((table (make-hash-table :test 'equal)))
                 (loop :for c :across "/'].;[,lpmkonjibhuvgycftxdrzse<aw" :for i :from 0
                       :do (setf (gethash (string c) table) i))
                 (loop :for c :across "=-0987654321" :for i :from 6 :by 3
                       :do (setf (gethash (string c) table) i))
                 (setf (gethash "q" table) 35)
                 table))

(defun on-key-press (self event)
  (handler-case
      (let ((key (-> event gdk-event-key-string (gethash *key-map*))))
        (if (functionp key)
            (funcall key (gdk-event-key-string event))
            (when (not (gethash key *running-synths*))
              (setf (gethash key *running-synths*)
                    (synth 'key-pad-synth :freq (midicps (+ *root-note* key))))
              (writeln (+ *root-note* key)))))
    (error (c) (writeln c))))
;
(defun on-key-release (self event)
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
;;
(def *scales*
  (mapcar (λ(descr) (apply #'make-scale-descr
                           (apply (curry #'concatenate 'list) (loop :for key :in [:name :range-low :range-high :value :switch :setter]
                                          :for val :in descr
                                          :collect [key val]))))
          [["LPF" 1/8 20 20 t (λ(x) (control-set (busnum lpf-coeff) x))]
           ["Res" 0.1 3.0 1 nil (λ(x) (control-set (busnum lpf-res) x))]
           ["HPF" 30 12000 30 t (λ(x) (ctrl :saw :hpf x))]
           ["Attack (1/1000)" 1 1000 90 nil (λ(x) (control-set (busnum keypad-attack) x))]
           ["Decay (1/1000)" 1 1000 200 nil (λ(x) (control-set (busnum keypad-decay) x))]
           ["Sustain (1/1000)" 1 1000 600 nil (λ(x) (control-set (busnum keypad-sustain) x))]
           ["Release (1/1000)" 1 1000 200 nil (λ(x) (control-set (busnum keypad-release) x))]
           ["Clip (1/100)" 1 100 100 nil (λ(x) (control-set (cbus :clip) (/ x 100)))]
           ["Wrap (1/100)" 1 100 100 nil (λ(x) (control-set (cbus :wrap) (/ x 100)))]
           ["Fold (1/100)" 1 100 100 nil (λ(x) (control-set (cbus :fold) (/ x 100)))]
           ["Amp (1/100)" 1 100 30 nil (λ(x) (control-set (cbus :amp) (/ x 100)))]]))

(def *scales*
  (labels ((v (name low high value switch setter)
             (make-scale-descr :name name :range-low low :range-high high :value value :switch switch :setter setter)))
    ))
  (mapcar (λ(descr) (apply #'make-scale-descr
                           (apply (curry #'concatenate 'list) (loop :for key :in [:name :range-low :range-high :value :switch :setter]
                                          :for val :in descr
                                          :collect [key val]))))
          [["LPF" 1/8 20 20 t (λ(x) (control-set (busnum lpf-coeff) x))]
           ["Res" 0.1 3.0 1 nil (λ(x) (control-set (busnum lpf-res) x))]
           ["HPF" 30 12000 30 t (λ(x) (ctrl :saw :hpf x))]
           ["Attack (1/1000)" 1 1000 90 nil (λ(x) (control-set (busnum keypad-attack) x))]
           ["Decay (1/1000)" 1 1000 200 nil (λ(x) (control-set (busnum keypad-decay) x))]
           ["Sustain (1/1000)" 1 1000 600 nil (λ(x) (control-set (busnum keypad-sustain) x))]
           ["Release (1/1000)" 1 1000 200 nil (λ(x) (control-set (busnum keypad-release) x))]
           ["Clip (1/100)" 1 100 100 nil (λ(x) (control-set (cbus :clip) (/ x 100)))]
           ["Wrap (1/100)" 1 100 100 nil (λ(x) (control-set (cbus :wrap) (/ x 100)))]
           ["Fold (1/100)" 1 100 100 nil (λ(x) (control-set (cbus :fold) (/ x 100)))]
           ["Amp (1/100)" 1 100 30 nil (λ(x) (control-set (cbus :amp) (/ x 100)))]]))

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
