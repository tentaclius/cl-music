(ql:quickload :cl-collider)
(ql:quickload :sc-extensions)
(ql:quickload '(bdef/cl-collider))
(ql:quickload :alexandria)
(ql:quickload :arrow-macros)
(ql:quickload :bordeaux-threads)
(ql:quickload :trivial-arguments)
(ql:quickload :trivia)
(ql:quickload :cl-cffi-gtk)
(ql:quickload :cl-alsaseq)

(shadowing-import '(
                    alexandria:flatten alexandria:curry alexandria:compose alexandria:if-let alexandria:when-let alexandria:when-let*
                    alexandria:random-elt alexandria:shuffle alexandria:rotate
                    bdef:bdef bdef:bdef-metadata
                    trivia:match
                    calispel:? calispel:! calispel:channel
                    ))

(named-readtables:in-readtable :sc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITY FUNCTIONS

(defmacro λ (&body body) `(lambda ,@body))

(defmacro def (&rest pairs)
  `(progn ,@(loop :for (k v) :on pairs :by #'cddr :collect `(if (boundp ',k) (setf ,k ,v) (defparameter ,k ,v)))))

(defmacro nlet (tag var-vals &body body)
  `(labels ((,tag ,(mapcar #'car var-vals) ,@body))
     (,tag ,@(mapcar #'cadr var-vals))))

(defmacro spawn (&body body)
  `(bordeaux-threads:make-thread (lambda() ,@body)))

(defun mergeplist (a b)
  (let ((n ()))
    (loop :for (k v) :on a :by #'cddr :do (setf (getf n k) v))
    (loop :for (k v) :on b :by #'cddr :do (setf (getf n k) v))
    n))

(defun ++. (lst &rest els)
  (append lst els))

;; easier hash-map syntax
(defun hshm (&rest args)
  (let ((h (make-hash-table :test #'equal)))
    (loop :for (k v) :on args :by #'cddr :do (setf (gethash k h) v))
    h))

(defun href (map key &optional default)
  (multiple-value-bind (val found?) (gethash key map)
    (values (if found? val default) found?)))

(defun hset (map key value)
  (setf (gethash key map) value))

(let ((thread-map (make-hash-table :test #'equal)))
  (defun spawn-named-fn (name fn)
    (let ((tid (gethash name thread-map)))
      (when (and (bordeaux-threads:threadp tid) (bordeaux-threads:thread-alive-p tid))
        (bordeaux-threads:destroy-thread tid)))
    (setf (gethash name thread-map)
          (bordeaux-threads:make-thread fn))))
(defmacro spawn-named (name &body body)
  `(spawn-named-fn ,name (lambda() ,@body)))

(defun writeln (&rest args)
  (loop :for el :in args :do (princ el))
  (terpri)
  (car args))

(defun cat (&rest vals)
  (let ((str (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s str)
      (loop :for vl :in vals :do (format s "~a" vl)))
    (string str)))

(defun 1/ (&rest vals)
  (apply #'/ (cons 1 vals)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MIDI

;; arturia minilab mkII knob ctl
(def
  knob01 112
  knob02 74 
  knob03 71 
  knob04 76 
  knob05 77 
  knob06 93 
  knob07 73 
  knob08 75 
  ;
  knob09 114
  knob10 18 
  knob11 19 
  knob12 16 
  knob13 17 
  knob14 91 
  knob15 79 
  knob16 72 )

(defstruct mr-midi-event
  type
  note
  velocity
  start
  duration
  channel)

(defun alsa-to-my-midi (message)
  (let* ((event-type (getf message :event-type))
         (event-data (getf message :event-data)))
    (case event-type
      (:SND_SEQ_EVENT_CONTROLLER
       (let ((param (getf event-data 'cl-alsaseq:param))
             (value (getf event-data 'calispel:value))
             (chan (getf event-data 'calispel:channel)))
         (make-mr-midi-event :type :control
                             :note param
                             :velocity value
                             :channel chan)))
      (:SND_SEQ_EVENT_NOTEON
       (let ((note (getf event-data 'cl-alsaseq:note))
             (velo (getf event-data 'cl-alsaseq:velocity))
             (chan (getf event-data 'calispel:channel)))
         (make-mr-midi-event :type :note_on
                             :note note
                             :velocity velo
                             :channel chan)))
      (:SND_SEQ_EVENT_NOTEOFF
       (let ((note (getf event-data 'cl-alsaseq:note))
             (velo (getf event-data 'cl-alsaseq:velocity))
             (chan (getf event-data 'calispel:channel)))
         (make-mr-midi-event :type :note_off
                             :note note
                             :velocity velo
                             :channel chan)))
      (:SND_SEQ_EVENT_PITCHBEND
       (let ((value (getf event-data 'calispel:value))
             (chan  (getf event-data 'calispel:channel)))
         (make-mr-midi-event :type :pitchbend
                             :velocity value
                             :channel chan))))))

(defstruct midi-reader
  name
  seq*
  seq
  in-port
  out-port)

(defun mk-midi-reader (name)
  (make-midi-reader :name name))

;(in-package :mylisp)
;(in-package :play)

(defun start-midi-reader (handle handler)
  (let* ((seq*     (cl-alsaseq:open-seq (midi-reader-name handle)))
         (midi-seq (cffi:mem-ref seq* :pointer))
         (in-port  (cl-alsaseq:open-port (midi-reader-name handle) midi-seq :input))
         )
    (setf (midi-reader-seq* handle) seq*)
    (setf (midi-reader-seq handle) midi-seq)
    (setf (midi-reader-in-port handle) in-port)
    (spawn-named
      (midi-reader-name handle)
      (loop
        (handler-case
            (when-let
              ((msg (alsa-to-my-midi (cl-alsaseq:recv (midi-reader-seq handle)))))
              (funcall handler msg))
          (error (c) (format t "ERROR: ~a~%" c)))))
    handle))

(defun start-midi-writer (handle)
  (let* ((seq*     (or (midi-reader-seq* handle) (cl-alsaseq:open-seq (midi-reader-name handle))))
         (midi-seq (or (midi-reader-seq handle) (cffi:mem-ref seq* :pointer)))
         (out-port (cl-alsaseq:open-port (format nil (midi-reader-name handle)) midi-seq :output)))
    (setf (midi-reader-seq* handle) seq*)
    (setf (midi-reader-seq handle) midi-seq)
    (setf (midi-reader-out-port handle) out-port)
    handle))

(defun stop-midi-reader (handle)
  (spawn-named (midi-reader-name handle) nil)
  (when (midi-reader-in-port handle)
    (cl-alsaseq:close-port (midi-reader-seq handle) (midi-reader-in-port handle)))
  (when (midi-reader-out-port handle)
    (cl-alsaseq:close-port (midi-reader-seq handle) (midi-reader-out-port handle)))
  (when (midi-reader-seq* handle)
    (cl-alsaseq:close-seq (midi-reader-seq* handle)))
  (setf (midi-reader-seq handle) nil)
  (setf (midi-reader-seq* handle) nil)
  (setf (midi-reader-in-port handle) nil)
  (setf (midi-reader-out-port handle) nil))

(defun midi-note-on (handle note &optional (velo 127) (chan 0))
  (when (midi-reader-out-port handle)
    (cl-alsaseq:send-note velo note chan :SND_SEQ_EVENT_NOTEON
                          (midi-reader-seq handle) (midi-reader-out-port handle))))

(defun midi-note-off (handle note &optional (velo 0) (chan 0))
  (when (midi-reader-out-port handle)
    (cl-alsaseq:send-note velo note chan :SND_SEQ_EVENT_NOTEOFF
                          (midi-reader-seq handle) (midi-reader-out-port handle))))


;;; PATTERNS

(defclass %seq () (data))
(defun seql (&rest data)
  (let ((ss (make-instance '%seq)))
    (setf (slot-value ss 'data) (apply #'append data))
    ss))
(defun seq (&rest data)
  (seql data))

(defclass %sim () (data))
(defun siml (&rest data)
  (let ((ss (make-instance '%sim)))
    (setf (slot-value ss 'data) (apply #'append data))
    ss))
(defun sim (&rest data)
  (siml data))

(defun seq-data (ss)
  (slot-value ss 'data))

(defun seq-map (fn sq)
  (let ((ss (make-instance (type-of sq))))
    (setf (slot-value ss 'data)
          (mapcar (lambda(n) (if (or (equal (type-of n) '%sim)
                                     (equal (type-of n) '%seq))
                                 (seq-map fn n)
                                 (funcall fn n)))
            (seq-data sq)))
    ss))

(defclass %snt () (synth data release))
(defun snt (synth &rest data)
  (let ((snt (make-instance '%snt)))
    (setf (slot-value snt 'synth) synth)
    (setf (slot-value snt 'data) data)
    (setf (slot-value snt 'release) t)
    snt))
(defun snx (synth &rest data)
  (let ((snt (make-instance '%snt)))
    (setf (slot-value snt 'synth) synth)
    (setf (slot-value snt 'data) data)
    (setf (slot-value snt 'release) nil)
    snt))

(defgeneric seq-schedule (ss snth-fn note-fn start duration))

(defmethod seq-schedule ((ss %seq) (snth-fn function) (note-fn function) (start number) (duration number))
  (when-let* ((seq (seq-data ss))
              (delta-t (and (not (null seq)) (/ duration (length seq)))))
             (loop :for el :in seq :for j :from 0 :do
                   (let ((tm (+ start (* j delta-t))))
                     (seq-schedule el snth-fn note-fn tm delta-t)))))

(defmethod seq-schedule ((ss %sim) (snth-fn function) (note-fn function) (start number) (duration number))
  (loop :for el :in (seq-data ss) :do
        (seq-schedule el snth-fn note-fn start duration)))

(defmethod seq-schedule ((ss number) (synth-fn function) (note-fn function) (start number) (duration number))
  (at-beat start (funcall synth-fn start duration (funcall note-fn ss))))

(defmethod seq-schedule ((ss list) (synth-fn function) (note-fn function) (start number) (duration number))
  (at-beat start (apply #'synth (cons (funcall note-fn (first ss)) (rest ss)))))


(defclass %pattern () 
  ((i :initform 0)
   (is-running :initform nil)
   (repeats :initarg :repeats
            :initform :infinity)
   (stop-quant :initarg :stop-quant
               :initform nil)
   (beat-incr :initarg :beat-incr
              :initform 1)
   (note-fn  :initarg :note-fn
             :initform (lambda (n) [:freq (midicps n)]))
   (synth-fn :initarg :synth-fn
             :initform (lambda(b d s e) (declare (ignorable b d)) (when (and s e) (at-beat b (apply #'synth (cons s e))))))
   (pattern-fn :initarg :pattern-fn)))

(let ((pattern-registry (hshm)))
  (defun pstart (name &key (quant 1) (repeats :infinity))
    (when-let ((pattern (href pattern-registry name)))
      (setf (slot-value pattern 'is-running) t)
      (setf (slot-value pattern 'repeats) repeats)
      (setf (slot-value pattern 'i) 0)
      (setf (slot-value pattern 'stop-quant) nil)
      (funcall (slot-value pattern 'pattern-fn) pattern (clock-quant quant) 0)))
  ;
  (defun pstop (name &optional (quant nil))
    (when-let ((pattern (href pattern-registry name)))
      (if quant (setf (slot-value pattern 'stop-quant) quant)
          (setf (slot-value pattern 'is-running) nil))))
  ;
  (defun regpattern (name synth-fn pattern-fn
                          &key (note-fn (lambda (n) [:freq (midicps n)])) (beat-incr 1))
    (multiple-value-bind (pattern found?) (href pattern-registry name)
      (labels ((pattern-wrap (ptn beat i)
                 (when (slot-value ptn 'is-running)
                   (writeln 'pattern-wrap)
                   (writeln (funcall pattern-fn i))
                   (seq-schedule (funcall pattern-fn i) synth-fn note-fn beat 1)
                   (let ((next-beat (+ beat beat-incr)))
                     (clock-add next-beat #'pattern-wrap ptn next-beat (1+ i))))))
        (if found?
            (progn
              (setf (slot-value pattern 'pattern-fn) #'pattern-wrap)
              (setf (slot-value pattern 'synth-fn) synth-fn)
              (when note-fn (setf (slot-value pattern 'note-fn) note-fn)))
            (hset pattern-registry name
                  (make-instance '%pattern
                                 :synth-fn synth-fn
                                 :pattern-fn #'pattern-wrap
                                 :note-fn note-fn
                                 :beat-incr beat-incr)))))))

(regpattern :ssin
  (λ(b d e) (when e (at-beat b (synth e))))
  (λ(i)
    (seq 'bd nil 'bd nil)))

(pstart :ssin)

(pstop :ssin)



