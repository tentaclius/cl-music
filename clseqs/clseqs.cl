(provide "clseqs")
(ql:quickload :cl-collider)
(defpackage #:clseqs (:use :cl :sc))
(in-package #:clseqs)


(defun mergeplist (&rest args)
  (let ((n ()))
    (loop :for a :in args
          :do (loop :for (k v) :on a :by #'cddr :do (setf (getf n k) v)))
    n))

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
      (clock-add b 'metro-play b 0))))

(defun metro-stop ()
  (setf *metro-running* nil))

(defun metro-play (bt i)
  (when *metro-running*
    (loop for value being each hash-values of *metro-seqs*
          :do (handler-case
                  (if (functionp value)
                      (let ((ret (funcall value i)))
                        (when (subtypep (type-of ret) '<events>)
                          (ev-schedule ret (list :start bt))))
                      (ev-schedule value (list :start bt)))
                (error (c) (format t "ERROR ~a~%" c))))
    (let ((next-beat (1+ bt)))
      (clock-add next-beat 'metro-play next-beat (1+ i)))))

(defun m-play-drum ()
  (lambda (e a)
    (at-beat (getf a :start 0)
             (cond
               ((not e) nil)
               ((listp e) (apply #'synth e))
               (t (synth e))))))

(defun m-play-synth (snt &key (start 0) (dur 1) (note-len 1) (note-fn (λ(f) f)) (attr (list)))
  (lambda (e a)
    (let* ((start    (getf a :start start))
           (dur      (getf a :dur dur))
           (note-len (getf a :note-len note-len))
           (note-fn  (getf a :note-fn note-fn))
           (appl     (getf a :attr attr))
           (s        (when e (at-beat start (apply #'synth (append (list snt :freq (funcall note-fn e)) appl))))))
      (when s (at-beat (+ start (* dur note-len)) (release s))))))

(export '(
          S SL U UL SA SAL UA UAL A
          ev-map metro-add metro-start metro-stop
          m-play-drum m-play-synth
          ))
