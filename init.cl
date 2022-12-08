(provide "mylisp")

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

(in-package #:sc)
(defugen (loop-buf "LoopBuf")
    (chanls bufnum &key (rate 1.0) (gate 1) (start-pos 0.0) (start-loop 0.0) (end-loop 0.0) (interpolation 2))
  ((:ar (multinew new 'multiout-ugen chanls bufnum rate gate start-pos start-loop end-loop interpolation))
   (:kr (multinew new 'multiout-ugen chanls bufnum rate gate start-pos start-loop end-loop interpolation))))

(defpackage :mylisp (:use :cl :sc :arrow-macros))
(in-package :mylisp)

(shadowing-import '(
                    alexandria:flatten alexandria:curry alexandria:compose alexandria:if-let alexandria:when-let alexandria:when-let*
                    alexandria:random-elt alexandria:shuffle alexandria:rotate
                    bdef:bdef bdef:bdef-metadata
                    trivia:match
                    calispel:? calispel:! calispel:channel
                    ))

(named-readtables:in-readtable :sc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SUPERCOLLIDER INIT

(defvar *sc-started* nil)

(defun sc-init (&optional (port 57110))
  (when (not *sc-started*)
    (setf *sc-started* t)
    (setf *sc-plugin-paths* nil)
    (setf *s* (make-external-server "localhost" :port port
                                    :server-options (make-server-options :realtime-mem-size (* 65536 4))))
    (when (null (all-running-servers))
      (server-boot *s*)
      (jack-connect)
      (init-synths))))

(defun sc-connect (&optional (port 57110))
  (setf *sc-plugin-paths* nil)
  (setf *s* (make-external-server "localhost" :port port :just-connect-p t))
  (server-boot *s*)
  (init-synths))

(defun use-gtk ()
  (use-package :gtk)
  (use-package :gdk)
  (use-package :gdk-pixbuf)
  (use-package :gobject)
  (use-package :glib)
  (use-package :gio)
  (use-package :pango)
  (use-package :cairo))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SC HELPERS

(defun dur (bt tm &rest spec)
  (let ((running-synths (mapcar (lambda (s) (at-beat bt (apply #'synth s))) spec)))
    (at-beat (+ bt tm) (loop :for synt :in running-synths :do (release synt)))))

(defun sc (scale step)
  (if (listp step)
      (mapcar (curry #'sc scale) step)
      (+ (aref scale (mod step (length scale)))
         (* 12 (floor (/ step (length scale)))) )))

(let ((running-synths (hshm)))
  (defun nsynth (name &rest specs)
    (let* ((s (and (href running-synths name)))
           (p (and s (is-playing-p s))))
      (cond
        ((null specs)
         (when p (release s))
         (hset running-synths name nil))
        ((equal (car specs) :free)
         (when p (free s))
         (hset running-synths name nil))
        ((equal (car specs) :release)
         (when p (release s))
         (hset running-synths name nil))
        (t
         (when p (release s))
         (hset running-synths name (apply #'synth specs)))))))

(defun chord (&optional (mod nil) (shift 0) (root 0) (scale *chromatic*))
  (mapcar
    (curry (λ(n) (+ root (sc scale (+ n shift)))))
    (case mod
      ((nil :3) (list 0 2 4))
      ((:7)     (list 0 2 4 6))
      ((:5)     (list 0 4 7))
      ((:sus4)  (list 0 2 3))
      ((:sus2)  (list 0 1 4))
      ((:sus9)  (list 0 4 8))
      ((:7sus4) (list 0 2 3 6)))))

;; my sequences

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
          (mapcar (lambda(n)
                      (typecase n
                        (%sim (seq-map fn n))
                        (%seq (seq-map fn n))
                        (list (cons (funcall fn (first n)) (rest n)))
                        (t (funcall fn n))))
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

(defmacro per-beat (i &rest seqs)
  (let ((len (length seqs)))
    `(case (mod ,i ,len)
       ,@(loop :for el :in seqs :for n :from 0 :collect `((,n) ,el)))))

(defun once-every (i n r sq)
  (if (= (mod i n) r) sq nil))

(defgeneric seq-schedule (ss snth-fn start duration attrs))

(defmethod seq-schedule ((ss %seq) (snth-fn function) (start number) (duration number) (attrs list))
  (when-let* ((seq (seq-data ss))
              (delta-t (and (not (null seq)) (/ duration (length seq)))))
             (loop :for el :in seq :for j :from 0 :do
                   (let ((tm (+ start (* j delta-t))))
                     (seq-schedule el snth-fn tm delta-t attrs)))))

(defmethod seq-schedule ((ss %sim) (snth-fn function) (start number) (duration number) (attrs list))
  (loop :for el :in (seq-data ss) :do
        (seq-schedule el snth-fn start duration attrs)))

(defmethod seq-schedule ((ss number) (synth-fn function) (start number) (duration number) (attrs list))
  (at-beat start (funcall synth-fn start duration (cons ss attrs))))

(defmethod seq-schedule ((ss symbol) (synth-fn function) (start number) (duration number) (attrs list))
  (when ss (at-beat start (funcall synth-fn start duration (cons ss attrs)))))

(defmethod seq-schedule ((ss list) (synth-fn function) (start number) (duration number) (attrs list))
  (seq-schedule (car ss) synth-fn start duration (append attrs (cdr ss))))

;;;;; PATTERNS

(defclass %pattern () 
  ((is-running :initform nil)
   (repeats :initarg :repeats
            :initform nil)
   (stop-quant :initarg :stop-quant
               :initform nil)
   (beat-incr :initarg :beat-incr
              :initform 1)
   (synth-fn :initarg :synth-fn
             :initform (lambda(b d s e) (declare (ignorable b d))
                         (when (and s e)
                           (at-beat b (apply #'synth (cons s e))))))
   (pattern-fn :initarg :pattern-fn)
   (runner-fn :initarg :runner-fn)))

(let ((pattern-registry (hshm)))
  (defun pstart (name &optional (quant 1) repeats)
    (when-let ((pattern (href pattern-registry name)))
      (when (not (slot-value pattern 'is-running))
        (setf (slot-value pattern 'is-running) t)
        (setf (slot-value pattern 'repeats) repeats)
        (setf (slot-value pattern 'stop-quant) nil)
        (funcall (slot-value pattern 'runner-fn) pattern (clock-quant quant) 0))))
  ;
  (defun pstop (name &optional (reps 0))
    (when-let ((pattern (href pattern-registry name)))
      (setf (slot-value pattern 'repeats) reps)))
  ;
  (defun regpattern (name synth-fn pattern-fn &optional (beat-incr 1))
    (multiple-value-bind (pattern found?) (href pattern-registry name)
      (flet ((runner (ptn beat i)
               (when-let ((reps (slot-value ptn 'repeats)))
                 (writeln reps)
                 (if (> reps 0)
                     (decf (slot-value ptn 'repeats))
                     (setf (slot-value ptn 'is-running) nil)))
               (when (slot-value ptn 'is-running)
                 (handler-case
                     (seq-schedule (funcall (slot-value ptn 'pattern-fn) i) (slot-value ptn 'synth-fn) beat (slot-value ptn 'beat-incr) (list))
                   (error (c) (writeln "ERROR: " c)))
                 (let ((next-beat (+ beat (slot-value ptn 'beat-incr))))
                   (clock-add next-beat (slot-value ptn 'runner-fn) ptn next-beat (1+ i))))))
        (if found?
            (progn
              (setf (slot-value pattern 'runner-fn) #'runner)
              (setf (slot-value pattern 'pattern-fn) pattern-fn)
              (setf (slot-value pattern 'synth-fn) synth-fn)
              (setf (slot-value pattern 'beat-incr) beat-incr))
            (hset pattern-registry name
                  (make-instance '%pattern
                                 :synth-fn synth-fn
                                 :runner-fn #'runner
                                 :pattern-fn pattern-fn
                                 :beat-incr beat-incr)))))))

(defmacro defpattern (pattern-name a-synth-fn a-pattern-fn &optional (incr 1))
  (let ((pattern-fn (read-from-string (cat pattern-name "-pattern")))
        (synth-fn   (read-from-string (cat pattern-name "-synth"))))
    `(progn
       (def ,pattern-fn ,a-pattern-fn)
       (def ,synth-fn ,a-synth-fn)
       (let ((i 0)
             (repeats +inf+)
             (stop-quant nil))
         (defun ,pattern-name (&rest args) ;; (beat &optional ,j ,a-repeats)
           (trivia:match args
             ((list :reset)
                (setf i 0)
                (setf repeats +inf+)
                (setf stop-quant nil))
             ((list :stop)        (setf repeats 0))
             ((list :stop quant)  (setf stop-quant (clock-quant quant)))
             ((list :start)       (,pattern-name :start 1 +inf+))
             ((list :start quant) (,pattern-name :start quant +inf+))
             ((list :start-now)
                (,pattern-name :reset)
                (,pattern-name (clock-beats) 0))
             ((list :start quant reps)
                (,pattern-name :reset)
                (setf repeats reps)
                (,pattern-name (clock-quant quant) 0))
             ((list beat j)
                (when (and (< i repeats) (or (not stop-quant) (< beat stop-quant)))
                  (labels ((dispatch (beat frame seq)
                             (typecase seq
                               (%sim (chord beat frame (seq-data seq)) t)
                               (%seq (sched beat frame (seq-data seq)) t)))
                           (chord (beat frame seq)
                             (loop :for el :in seq :do
                                   (or (dispatch beat frame el)
                                       (at-beat beat (funcall ,synth-fn beat frame el)))))
                           (sched (beat frame seq)
                             (when-let ((delta-t (and (not (null seq)) (/ frame (length seq)))))
                               (loop :for el :in seq :for j :from 0 :do
                                     (let ((tm (+ beat (* j delta-t))))
                                       (or (dispatch tm delta-t el)
                                           (at-beat tm (funcall ,synth-fn tm delta-t el))))))))
                    (handler-case
                        (dispatch beat ,incr (funcall ,pattern-fn j))
                      (error (c) (writeln "ERROR: " c))))
                  (let ((next-beat (+ beat ,incr)))
                    (setf i (1+ i))
                    (clock-add next-beat ',pattern-name next-beat (1+ j)))))))))))

(defun play-note (snth &key
                       (synth-fn (lambda(b d s e) (declare (ignorable b d)) (when (and s e) (at-beat b (apply #'synth (cons s e))))))
                       (note-fn (lambda(e) [:freq (midicps e)]))
                       (release t)
                       attr)
  (lambda (b d e)
    (declare (ignorable b d))
    (when e
      (let* ((start (+ b (or (and (listp e) (getf (cdr e) :start))
                             (getf attr :start 0))))
             (prb (or (and (listp e) (getf (cdr e) :prob))
                      (getf attr :prob nil)))
             (s (when (or (not prb) (< (random 1000000) (* prb 1000000)))
                  (typecase e
                    (%snt [(apply #'synth (cons (slot-value e 'synth) (slot-value e 'data)))])
                    (function (funcall e b d))
                    (list (funcall synth-fn start d snth (append (funcall note-fn (car e)) (mergeplist attr (cdr e)))))
                    (t (funcall synth-fn start d snth (append (funcall note-fn e) attr)))))))
        (when (and s release)
          (let ((dur (or (and (listp e) (getf (cdr e) :dur))
                         (getf attr :dur d))))
            (if (listp s)
                (at-beat (+ start dur) (mapcar (λ(s) (when (and s (equal 'cl-collider::node (type-of s))) (release s))) s))
                (when (equal 'cl-collider::node (type-of s))
                  (at-beat (+ start dur) (release s))))))))))

(defun play-note-attr (snth &key
                       (synth-fn (lambda(b d s) (declare (ignorable b d))
                                   (when s (at-beat b (apply #'synth s)))))
                       (note-fn (lambda(e) [:freq (midicps e)]))
                       (release t))
  (lambda (b d e)
    (declare (ignorable b d))
    (when e
      (let* ((start (+ b (or (and (listp e) (getf (cdr e) :start))
                             (getf (cdr snth) :start 0))))
             (s (typecase e
                  (%snt [(apply #'synth (cons (slot-value e 'synth) (slot-value e 'data)))])
                  (function (funcall e b d))
                  (list (funcall synth-fn start d (append snth (funcall note-fn (car e)))))
                  (t (funcall synth-fn start d (append snth (funcall note-fn e)))))))
        (when release
          (let ((dur (or (and (listp e) (getf (cdr e) :dur))
                         (getf (cdr snth) :dur d))))
            (if (listp s)
                (at-beat (+ start dur) (mapcar (λ(s) (when (and s (equal 'cl-collider::node (type-of s))) (release s))) s))
                (when (equal 'cl-collider::node (type-of s))
                  (at-beat (+ start dur) (release s))))))))))

(defun play-drum (&rest attrs)
  (λ(b d e)
    (declare (ignorable b d))
    (when (and e (not (equal e '-)))
      (if (listp e)
          (let ((prob (getf (cdr e) :prob nil)))
            (when (or (not prob) (< (random 1000000) (* prob 1000000)))
              (apply #'synth (cons (car e) (mergeplist attrs (cdr e))))))
          (apply #'synth (cons e attrs))))))

(defun play-midi (mh &key (note-fn (λ(n) n)))
  (λ(b d e)
    (when e
      (spawn
        (sleep (- b (clock-beats))) 
        (midi-note-on mh (funcall note-fn e))
        (sleep (- (* d (/ (clock-bpm) 60)) 0.03))
        (midi-note-off mh (funcall note-fn e))
        ))))

(defun play-seq (synth-fn ptn j)
  (labels
      ((dispatch (beat frame seq)
         (typecase seq
           (%sim (chord beat frame (seq-data seq)) t)
           (%seq (sched beat frame (seq-data seq)) t)))
       (chord (beat frame seq)
         (loop :for el :in seq :do
               (or (dispatch beat frame el)
                   (at-beat beat (funcall synth-fn beat frame el)))))
       (sched (beat frame seq)
         (when-let ((delta-t (and (not (null seq)) (/ frame (length seq)))))
                   (loop :for el :in seq :for j :from 0 :do
                         (let ((tm (+ beat (* j delta-t))))
                           (or (dispatch tm delta-t el)
                               (at-beat tm (funcall synth-fn tm delta-t el))))))))
    (handler-case
        (dispatch (clock-beats) 1 (funcall ptn j))
      (error (c) (writeln "ERROR: " c)))))

(defvar *chromatic* #(0 1 2 3 4 5 6 7 8 9 10 11))
(defvar *pentatonic* #(0 3 5 7 10))
(defvar *major* #(0 2 4 5 7 9 11))
(defvar *minor* #(0 2 3 5 7 8 10))

(defun ext-synth (name &rest args)
  (let ((id (sc::get-next-id *s*)))
    (apply (curry #'send-message *s* "/s_new" name id 0 0) args)
    id))

(defun rand-el (&rest args)
  (random-elt args))


(defun euclidian (n m snt &optional (nl nil))
  (let ((ii 0))
    (loop :for i :from 0 :to (1- m)
          :collect (if (= (floor (* ii (/ m n))) i)
                       (prog1 (if (functionp snt) (funcall snt) snt) (incf ii))
                       nl))))

(defun gen-list (lst)
  (let ((l0 lst) (p lst))
    (lambda (&optional n)
      (if n
          (setf p (nthcdr (mod n (length l0)) l0))
          (progn
            (when (null p) (setf p l0))
            (prog1 (car p) (setf p (cdr p))))))))

(defun gen-xrand (lst)
  (let ((l0 lst) (p lst))
    (lambda (&optional a)
      (when (null p) (setf p l0))
      (when a (setf p l0))
      (let* ((i (random (length p)))
             (el (nth i p)))
        (setf p (append (subseq p 0 i) (subseq p (1+ i) (length p))))
        el))))

(defun gen-rand (lst)
  (lambda ()
    (random-elt lst)))

(let ((buses (make-hash-table)))
  (defun cbus (name)
    (busnum (if-let ((val (gethash name buses)))
                    val (setf (gethash name buses) (bus-control)))))
  (defun cbus-set (name value)
    (control-set (cbus name) value))
  (defun cbus-get (name)
    (control-get (cbus name)))
  (defun cbus-in (name)
    (in.kr (cbus name)))
  (defun cbus-in-scale (name lo hi)
    (-> (in.kr (cbus name))
        (* (- hi lo) 1/127)
        (+ lo)
        float))
  (defun cbus-out (name val)
    (out.kr (cbus name) val)))

(let ((buses (make-hash-table)))
  (defun abus (name)
    (busnum (if-let ((val (gethash name buses)))
                    val (setf (gethash name buses) (bus-audio :chanls 2)))))
  (defun abus-in (name)
    (in.ar (abus name) 2))
  (defun abus-out (name val)
    (out.ar (abus name) val)))

(let ((synths (make-hash-table)))
  (defmacro csnt (name value)
    (if value 
        `(progn
           (when-let* ((s (href ,synths ,name))
                       (p (is-playing-p s)))
             (release s))
           (hset ,synths ,name ,value))
        `(href ,synths ,name))))

(let ((bufs (make-hash-table :test #'equal)))
  (defun cbuf (path)
    (bufnum (if-let ((val (gethash path bufs)))
                    val (setf (gethash path bufs) (buffer-read path))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTRUMENTS

(defun init-synths ()

;; General synth for playing a buffer
(defsynth sample ((buffer 0) (rate 1) (start 0) (amp 0.5) (out 0) (loop 0) (gate 1) (attack 0.1) (release 0.1))
  (let* ((sig (play-buf.ar 2 buffer (* rate (buf-rate-scale.ir buffer))
                           :start-pos (* start (/ (buf-frames.ir buffer) (buf-channels.ir buffer)))
                           :loop loop
                           :act :free))
         (sig (* sig (env-gen.kr (adsr attack 0.1 1 release) :gate gate :act :free))))
    (out.ar out (* amp sig)) ))

(defsynth sample-1 ((buffer 0) (rate 1) (start 0) (amp 0.5) (out 0) (loop 0) (gate 1) (attack 0.1) (release 0.1))
  (let* ((sig (play-buf.ar 1 buffer (* rate (buf-rate-scale.ir buffer))
                           :start-pos (* start (/ (buf-frames.ir buffer) (buf-channels.ir buffer)))
                           :loop loop
                           :act :free))
         (sig (* sig (env-gen.kr (adsr attack 0.1 1 release) :gate gate :act :free))))
    (out.ar out (* amp (pan2.ar sig))) ))

(defsynth sample-dur ((buffer 0) (rate 1) (start 0) (amp 0.5) (out 0) (loop 0) (gate 1) (dur 60) (attack 0.001) (release 0.1))
  (let* ((sig (play-buf.ar 2 buffer (* rate (buf-rate-scale.ir buffer))
                           :start-pos (* start (/ (buf-frames.ir buffer) (buf-channels.ir buffer)))
                           :loop loop
                           :act :free))
         (sig (* sig (env-gen.kr (linen attack (- dur attack release) release) :act :free)
                     (env-gen.kr (adsr 0.0001 0.0001 1 release) :gate gate :act :free))))
    (out.ar out (* amp sig)) ))

(defsynth sample-dur-1 ((buffer 0) (rate 1) (start 0) (amp 0.5) (out 0) (loop 0) (gate 1) (dur 60) (attack 0.001) (release 0.1))
  (let* ((sig (play-buf.ar 1 buffer (* rate (buf-rate-scale.ir buffer))
                           :start-pos (* start (/ (buf-frames.ir buffer) (buf-channels.ir buffer)))
                           :loop loop
                           :act :free))
         (sig (* sig (env-gen.kr (linen attack (- dur attack release) release) :act :free)
                 (env-gen.kr (adsr 0.0001 0.0001 1 release) :gate gate :act :free))))
    (out.ar out (* amp (pan2.ar sig))) ))

(defsynth sample-loop ((buffer 0) (rate 1) (amp 0.5) (out 0)
                       (start-pos 0) (start-loop 0) (end-loop 0) (interpolation 2)
                       (attack 0.01) (release 0.1) (gate 1))
  (let* ((sig (loop-buf.ar 2 buffer
                           :rate (* rate (buf-rate-scale.ir buffer))
                           :gate gate
                           :start-pos start-pos
                           :start-loop start-loop
                           :end-loop end-loop
                           :interpolation interpolation
                           ))
         (sig (* sig amp (env-gen.kr (adsr attack 0.1 1 release) :gate gate :act :free))))
    (out.ar out sig)))

;; Instruments

(defsynth fm-bass ((freq 220) (freq0 220) (slide 0) (out 0) (amp 0.5) (gate 1)
                              (q 3) (depth 400)
                              (a 0.01) (d 0.2) (s 0.6) (r 0.4))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (sin-osc.ar (* q fq))
         (* depth) (+ fq)
         (sin-osc.ar)
         (* amp (env-gen.kr (adsr a d s r) :act :free :gate gate))
         pan2.ar (out.ar out <>))))

(defsynth saw-bass ((freq 440) (freq0 440) (slide 0) (out 0) (amp 0.5) (gate 1)
                           (lpf 7) (res 1)
                           (a 0.01) (d 0.2) (s 0.4) (r 0.4))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> 
      (saw.ar fq)
      (rlpf.ar (* fq lpf) res)
      (+ (* 1/2 (sin-osc.ar fq)))
      (* amp (env-gen.kr (adsr a d s r) :act :free :gate gate))
      pan2.ar (out.ar out <>))))

(defsynth ssin ((freq 440) (freq0 440) (slide 0) (out 0) (amp 0.5) (gate 1)
                        (a 0.01) (d 0.2) (s 0.4) (r 0.4))
  (-<> (x-line.kr freq0 freq slide)
       (sin-osc.ar)
       (* amp (env-gen.kr (adsr a d s r) :act :free :gate gate))
       pan2.ar (out.ar out <>)))

(defsynth ssaw ((freq 440) (freq0 440) (slide 0) (out 0) (amp 0.5) (gate 1)
                           (lpf 7) (res 1)
                           (a 0.01) (d 0.2) (s 0.4) (r 0.4))
  (-<> (x-line.kr freq0 freq slide)
       (saw.ar)
       (rlpf.ar (* freq lpf) res)
       (* amp (env-gen.kr (adsr a d s r) :act :free :gate gate))
       pan2.ar (out.ar out <>)))

;; drums
(defsynth bd ((freq 330) (bass 10) (dur 0.05) (d 0.1) (out 0) (amp 0.5))
  (-<> (x-line.ar freq bass dur)
       (sin-osc.ar)
       (+ (* 0.7 (sin-osc.ar 60)))
       (* 0.9 amp (env-gen.kr (env [0 1 1 0] [0.0001 dur d]) :act :free))
       pan2.ar
       (out.ar out <>)))

(defsynth hh ((out 0) (amp 0.3) (dur 0.1) (freq 8000))
  (-<> (white-noise.ar)
       (hpf.ar freq)
       (* amp (env-gen.kr (perc 0.0 dur) :act :free))
       pan2.ar (out.ar out <>)))

(defsynth snare ((freq 1100) (amp 0.3)
              (out 0) (gate 1)
              (a 0.001) (d 0.2))
  (-<> (white-noise.ar)
       (lpf.ar freq)
       (* amp (env-gen.kr (perc a d) :gate gate :act :free))
       pan2.ar (out.ar out <>)))

(defsynth clap ((out 0) (amp 0.5) (f 1000))
  (-<> (white-noise.ar)
       (* amp (env-gen.kr (perc 0 0.16) :act :free))
       (bpf.ar f)
       pan2.ar
       (out.ar out <>)))

(defsynth metronome ((freq 440)(amp 0.3) (out 0))
  (-<> (sin-osc.ar freq)
       (+ (* 1/20 (white-noise.ar)))
       (* amp (env-gen.kr (perc 0.001 0.1) :act :free))
       pan2.ar (out.ar out <>)))

(defsynth recorder ((bus 1) (buffer 0) (dur 15))
  (record-buf.ar (delay-n.ar (in.ar bus 2) 1 0.003)
                 buffer
                 :run (env-gen.kr (linen 0 dur 0)
                                  :gate (changed.ar (in.ar bus 2))
                                  :act :free)))

) ;; init-synths 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPORTS

(export '(
          *s* sc-init sc-connect use-gtk λ mergelist def spawn spawn-named writeln cat dur sc chord ++. 1/
          seql seq siml sim seq-data seq-map play-seq snt snx
          ext-synth nsynth rand-el euclidian gen-list gen-rand gen-xrand
          loop-buf.kr loop-buf.ar
          make-mr-midi-event mr-midi-event-type mr-midi-event-note mr-midi-event-velocity mr-midi-event-start mr-midi-event-duration mr-midi-event-channel
          alsa-to-my-midi mk-midi-reader midi-reader-name midi-reader-seq midi-reader-in-port midi-reader-out-port
          start-midi-reader start-midi-writer stop-midi-reader midi-note-on midi-note-off
          per-beat once-every regpattern defpattern pstart pstop seq-schedule play-note play-note-attr play-drum play-midi
          csnt cbus cbuf cbus-set cbus-get cbus-in cbus-in-scale cbus-out abus abus-set abus-get abus-in abus-out
          ;; imported
          flatten curry compose if-let if-let* when-let when-let* random-elt shuffle rotate
          -> ->> -<> -<>> <>
          bdef bdef-metadata bdef-ensure-key match
          *chromatic* *pentatonic* *major* *minor*
          hshm href hset ? ! channel
          ))
