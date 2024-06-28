(require "clseqs-util")
(provide "clseqs-sequences")
(in-package #:clseqs)
(ql:quickload :cl-collider)


(defclass <events> ()
  ((events :initarg :events
           :initform (list)
           :accessor events)
   (attrib :initarg :attrib
           :initform (list)
           :accessor attrib)))
(defclass <seq> (<events>) ())
(defclass <sim> (<events>) ())

(defgeneric ev-schedule (el attr))

(defmethod ev-schedule ((el <seq>) attr)
    (let ((start (getf attr :start 0))
          (dur   (getf attr :dur 1))
          (len   (length (events el))))
      (loop :for i :from 0 :for evx :in (events el)
            :do (ev-schedule evx
                             (mergeplist attr
                                         (list :start (+ start (* dur (/ i len)))
                                               :dur (/ dur len))
                                         (attrib el))))))

(defmethod ev-schedule ((el <events>) attr)
   (loop :for x :in (events el)
         :do (ev-schedule x (mergeplist attr (attrib el)))))

(defmethod ev-schedule (el attr)
   (let ((fun (getf attr :fn)))
     (when fun (funcall fun el attr))))

(defun S  (&rest events)  (make-instance '<seq> :events events))
(defun SL (events)        (make-instance '<seq> :events events))
(defun U  (&rest events)  (make-instance '<sim> :events events))
(defun UL (events)        (make-instance '<sim> :events events))

(defun SA (attrs &rest events)
  (make-instance '<seq> :events events :attrib attrs))
(defun SAL (attrs events)
  (make-instance '<seq> :events events :attrib attrs))
(defun UA (attrs &rest events)
  (make-instance '<sim> :events events :attrib attrs))
(defun UAL (attrs events)
  (make-instance '<sim> :events events :attrib attrs))
(defun A (attrs &rest events)
  (make-instance '<sim> :events events :attrib attrs))

(defun ev-map (fn sq)
  (let ((ss (make-instance (type-of sq))))
    (setf (attrib ss) (attrib sq))
    (setf (events ss)
          (mapcar (lambda(n)
                      (if (subtypep (type-of n) '<events>)
                          (ev-map fn n)
                          (funcall fn n)))
            (events sq)))
    ss))

;;; Metro

(defparameter *metro-seqs* (make-hash-table))
(defparameter *metro-running* nil)

(defun metro-add (name &optional seq)
  (setf (gethash name *metro-seqs*) seq))

(defun metro-start ()
  (unless *metro-running*
    (setf *metro-running* t)
    (let ((b (ceiling (clock-beats))))
      (sc:clock-add b 'metro-play b))))

(defun metro-stop ()
  (setf *metro-running* nil))

(defun metro-play (bt)
  (when *metro-running*
    (loop for value being each hash-values of *metro-seqs*
          :do (handler-case
                  (if (functionp value)
                      (let ((ret (funcall value bt)))
                        (when (subtypep (type-of ret) '<events>)
                          (ev-schedule ret (list :start bt))))
                      (ev-schedule value (list :start bt)))
                (error (c) (format t "ERROR ~a~%" c))))
    (let ((next-beat (1+ bt)))
      (sc:clock-add next-beat 'metro-play next-beat))))

;;; Helper Functions

(defmacro per-beat (i &rest seqs)
  (let ((len (length seqs)))
    `(case (mod ,i ,len)
       ,@(loop :for el :in seqs :for n :from 0 :collect `((,n) ,el)))))

(defun m-play-drum ()
  (lambda (e a)
    (sc:at-beat (getf a :start 0)
             (cond
               ((not e) nil)
               ((listp e) (apply #'synth e))
               (t (synth e))))))

(defun m-play-synth (snt &key (start 0) (dur 1) (note-len 1) (note-fn (λ(f) (midicps f))) (attr (list)))
  (lambda (e a)
    (let* ((start    (getf a :start start))
           (dur      (getf a :dur dur))
           (note-len (getf a :note-len note-len))
           (note-fn  (getf a :note-fn note-fn))
           (appl     (getf a :attr attr))
           (s        (when e (sc:at-beat start (apply #'synth (append (list snt :freq (funcall note-fn e)) appl))))))
      (when s (sc:at-beat (+ start (* dur note-len)) (release s))))))

(defun m-tracker (attributes synthesizers &rest events)
  (let* ((pattern-len  (getf attributes :pattern-len :inf))
         (step-len     (length synthesizers))
         (steps-count  (ceiling (/ (length events) step-len)))
         (quant        (getf attributes :quant 1))
         (events-rest  (list)))
    (lambda (beat)
      (when (or (equal pattern-len :inf) (> pattern-len 0))
        (when (numberp pattern-len)
          (decf pattern-len))
        (when (and (= 0 (mod beat quant)) (null events-rest))
          (setq events-rest (subseq events (* step-len (mod beat steps-count)))))
        (loop :for synth :in synthesizers :while (not (null events-rest)) :do
              (ev-schedule (first events-rest) (list :fn synth :start beat))
              (setq events-rest (rest events-rest)))))))

;;;

(export '(S SL U UL SA SAL UA UAL A
          ev-map metro-add metro-start metro-stop
          m-play-drum m-play-synth m-tracker))
