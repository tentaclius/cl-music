(load "~/src/cl-music/lib.cl")
(ql:quickload :cl-cffi-gtk)
(ql:quickload :cl-alsaseq)
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

(def *running-synths* {})
(def *midi-channel* 0)
(def *root-note* 50)

(defun midi-map (messages)
  (dolist (message messages)
    (let* ((event-type (getf message :event-type))
           (event-data (getf message :event-data))
           (source (car (getf message :source)))
           (destination (car (getf message :dest))))
      (declare (ignorable source destination))
      (format t "~a: ~s~%"
              (case event-type
                (:snd_seq_event_noteon "Note on")
                (:snd_seq_event_noteoff "Note off")
                (:snd_seq_event_controller "CC")
                (t event-type))
              event-data))))
;
(midihelper:start-midihelper :master 96 'midi-map)

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
              (setf (gethash key *running-synths*) t)
              (midihelper:send-event (midihelper:ev-noteon *midi-channel* (+ *root-note* key) 127)))))
    (error (c) (writeln c))))
;
(defun on-key-release (self event)
  (handler-case
      (-<> event
           gdk-event-key-string
           (gethash *key-map*)
           (progn
             (when (gethash <> *running-synths*)
               (midihelper:send-event (midihelper:ev-noteoff *midi-channel* (+ *root-note* <>) 127)))
             (setf (gethash <> *running-synths*) nil)))
    (error (c) (writeln c))))


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
      ;; Pack and go
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))

(hello-world)
