(provide "clseqs-util")
(ql:quickload :cl-collider)
(defpackage :clseqs (:use :cl :sc))
(in-package :clseqs)

(ql:quickload :bordeaux-threads)

(defmacro λ (&body body) `(lambda ,@body))

(defmacro def (&rest pairs)
  `(progn ,@(loop :for (k v) :on pairs :by #'cddr :collect `(if (boundp ',k) (setf ,k ,v) (defparameter ,k ,v)))))

(defmacro nlet (tag var-vals &body body)
  `(labels ((,tag ,(mapcar #'car var-vals) ,@body))
     (,tag ,@(mapcar #'cadr var-vals))))

(defmacro spawn (&body body)
  `(bordeaux-threads:make-thread (lambda() ,@body)))

(defun mergeplist (&rest args)
  (let ((n ()))
    (loop :for a :in args
          :do (loop :for (k v) :on a :by #'cddr :do (setf (getf n k) v)))
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

(export '(λ def nlet spawn mergeplist ++. hshm href hset spawn-named writeln cat 1/))
