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
      (init-synths)
      )))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MIDI

;; midi notes
(def
  C-0 12    C0  12
  C#0 13    Db0 13
  D-0 14    D0  14
  D#0 15    Eb0 15
  E-0 16    E0  16
  F-0 17    F0  17
  F#0 18    Gb0 18
  G-0 19    G0  19
  G#0 20    Ab0 20
  A-0 21    A0  21
  A#0 22    Bb0 22
  B-0 23    B0  23
  ;
  C-1 24    C1  24
  C#1 25    Db1 25
  D-1 26    D1  26
  D#1 27    Eb1 27
  E-1 28    E1  28
  F-1 29    F1  29
  F#1 30    Gb1 30
  G-1 31    G1  31
  G#1 32    Ab1 32
  A-1 33    A1  33
  A#1 34    Bb1 34
  B-1 35    B1  35
  ;
  C-2 36    C2  36
  C#2 37    Db2 37
  D-2 38    D2  38
  D#2 39    Eb2 39
  E-2 40    E2  40
  F-2 41    F2  41
  F#2 42    Gb2 42
  G-2 43    G2  43
  G#2 44    Ab2 44
  A-2 45    A2  45
  A#2 46    Bb2 46
  B-2 47    B2  47
  ;
  C-3 48    C3  48
  C#3 49    Db3 49
  D-3 50    D3  50
  D#3 51    Eb3 51
  E-3 52    E3  52
  F-3 53    F3  53
  F#3 54    Gb3 54
  G-3 55    G3  55
  G#3 56    Ab3 56
  A-3 57    A3  57
  A#3 58    Bb3 58
  B-3 59    B3  59
  ;
  C-4 60    C4  60
  C#4 61    Db4 61
  D-4 62    D4  62
  D#4 63    Eb4 63
  E-4 64    E4  64
  F-4 65    F4  65
  F#4 66    Gb4 66
  G-4 67    G4  67
  G#4 68    Ab4 68
  A-4 69    A4  69
  A#4 70    Bb4 70
  B-4 71    B4  71
  ;
  C-5 72    C5  72
  C#5 73    Db5 73
  D-5 74    D5  74
  D#5 75    Eb5 75
  E-5 76    E5  76
  F-5 77    F5  77
  F#5 78    Gb5 78
  G-5 79    G5  79
  G#5 80    Ab5 80
  A-5 81    A5  81
  A#5 82    Bb5 82
  B-5 83    B5  83
  ;
  C-6 84    C6  84
  C#6 85    Db6 85
  D-6 86    D6  86
  D#6 87    Eb6 87
  E-6 88    E6  88
  F-6 89    F6  89
  F#6 90    Gb6 90
  G-6 91    G6  91
  G#6 92    Ab6 92
  A-6 93    A6  93
  A#6 94    Bb6 94
  B-6 95    B6  95
  ;
  C-7 96    C7  96
  C#7 97    Db7 97
  D-7 98    D7  98
  D#7 99    Eb7 99
  E-7 100   E7  100
  F-7 101   F7  101
  F#7 102   Gb7 102
  G-7 103   G7  103
  G#7 104   Ab7 104
  A-7 105   A7  105
  A#7 106   Bb7 106
  B-7 107   B7  107
  ;
  C-8 108   C8  108
  C#8 109   Db8 109
  D-8 110   D8  110
  D#8 111   Eb8 111
  E-8 112   E8  112
  F-8 113   F8  113
  F#8 114   Gb8 114
  G-8 115   G8  115
  G#8 116   Ab8 116
  A-8 117   A8  117
  A#8 118   Bb8 118
  B-8 119   B8  119
  ;
  C-9 120   C9  120
  C#9 121   Db9 121
  D-9 122   D9  122
  D#9 123   Eb9 123
  E-9 124   E9  124
  F-9 125   F9  125
  F#9 126   Gb9 126
  G-9 127   G9  127)

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

(let ((midi-send-lock (bt:make-lock)))
  (defun midi-note-on (handle note &optional (velo 127) (chan 0))
    (when (midi-reader-out-port handle)
      (bt:with-lock-held (midi-send-lock)
        (cl-alsaseq:send-note velo note chan :SND_SEQ_EVENT_NOTEON
                              (midi-reader-seq handle) (midi-reader-out-port handle)))))
  ;
  (defun midi-note-off (handle note &optional (velo 0) (chan 0))
    (when (midi-reader-out-port handle)
      (bt:with-lock-held (midi-send-lock)
        (cl-alsaseq:send-note velo note chan :SND_SEQ_EVENT_NOTEOFF
                              (midi-reader-seq handle) (midi-reader-out-port handle))))) )

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

(defclass %atr () (synth data))
(defun A (snt &rest data)
  (let ((atr (make-instance '%atr)))
    (setf (slot-value atr 'synth) snt)
    (setf (slot-value atr 'data) data)
    atr))

(defclass %seq () (data))
(defun seql (&rest data)
  (let ((ss (make-instance '%seq)))
    (setf (slot-value ss 'data) (apply #'append data))
    ss))
(defun seq (&rest data)
  (seql data))
;(defmacro S (&body body) `(seq ,@body))

(defclass %sim () (data))
(defun siml (&rest data)
  (let ((ss (make-instance '%sim)))
    (setf (slot-value ss 'data) (apply #'append data))
    ss))
(defun sim (&rest data)
  (siml data))
;(defmacro U (&body body) `(sim ,@body))

(defun seq-data (ss)
  (slot-value ss 'data))

(defun seq-map (fn sq)
  (let ((ss (make-instance (type-of sq))))
    (setf (slot-value ss 'data)
          (mapcar (lambda(n)
                      (typecase n
                        (%sim (seq-map fn n))
                        (%seq (seq-map fn n))
                        (%atr (apply 'A (cons (funcall fn (slot-value n 'synth)) (slot-value n 'data))))
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

(defun per-beat-n (n i &rest data)
  (let ((offset (mod (* i n) (length data))))
    (subseq data offset (+ offset n))))

(defun once-every (i n r sq &optional alt)
  (if (= (mod i n) r) sq alt))

(defgeneric seq-schedule (ss snth-fn start duration attrs))

(defmethod seq-schedule ((ss %seq) (synth-fn function) (start number) (duration number) (attrs list))
  (when-let* ((seq (seq-data ss))
              (delta-t (and (not (null seq)) (/ duration (length seq)))))
             (loop :for el :in seq :for j :from 0 :do
                   (let ((tm (+ start (* j delta-t))))
                     (seq-schedule el synth-fn tm delta-t attrs)))))

(defmethod seq-schedule ((ss %sim) (snth-fn function) (start number) (duration number) (attrs list))
  (loop :for el :in (seq-data ss) :do
        (seq-schedule el snth-fn start duration attrs)))

(defmethod seq-schedule ((ss %atr) (synth-fn function) (start number) (duration number) (attrs list))
  (seq-schedule (slot-value ss 'synth) synth-fn start duration (append (slot-value ss 'data) attrs)))

(defmethod seq-schedule (ss (synth-fn function) (start number) (duration number) (attrs list))
  (when ss (at-beat start (funcall synth-fn start duration ss attrs))))

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
  (defun pkill (name)
    (when-let ((pattern (href pattern-registry name)))
              (setf (slot-value pattern 'is-running) nil)))
  ;
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
                 (if (> reps 0)
                     (decf (slot-value ptn 'repeats))
                     (setf (slot-value ptn 'is-running) nil)))
               (when (slot-value ptn 'is-running)
                 (handler-case
                     (seq-schedule (funcall (slot-value ptn 'pattern-fn) i)
                                   (slot-value ptn 'synth-fn)
                                   beat
                                   (slot-value ptn 'beat-incr)
                                   (list))
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

(defmacro regpattern-ch (name chnls data &optional (spd 1))
  (let ((i-sym (gensym)))
   `(let ((- nil)) 
      (regpattern ,name
                  (play-channels ,@chnls)
                  (lambda (,i-sym) (cons :step (per-beat-n (length (list ,@chnls)) ,i-sym ,@data)))
                  ,spd))))

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

(defun play-channels (&rest ch)
  (lambda (b d e &optional a)
    (loop :for f :in ch :for inst :in (cdr e)
          :do (seq-schedule inst f b d (list)))))

(defun play-note (snth &key
                       (synth-fn (lambda(b d s e) (declare (ignorable b d)) (when (and s e) (at-beat b (apply #'synth (cons s e))))))
                       (note-fn (lambda(e) (list :freq (midicps e))))
                       (release t)
                       attr)
  (lambda (b d e &optional a)
    (declare (ignorable b d))
    (when e
      (let* ((start (+ b (getf a :start (getf attr :start 0))))
             (prb (getf a :prob (getf attr :prob)))
             (s (when (or (not prb) (< (random 1000000) (* prb 1000000)))
                  (typecase e
                    (%snt [(apply #'synth (cons (slot-value e 'synth) (slot-value e 'data)))])
                    (function (funcall e b d))
                    (list (funcall synth-fn start d snth (append (funcall note-fn (car e)) (mergeplist attr (cdr e)))))
                    (t (funcall synth-fn start d snth (append (funcall note-fn e) attr)))))))
        (when (and s release)
          (let ((dur (* d (getf a :dur (getf attr :dur 1)))))
            (if (listp s)
                (at-beat (+ start dur) (mapcar (λ(s) (when (and s (equal 'cl-collider::node (type-of s))) (release s))) s))
                (when (equal 'cl-collider::node (type-of s))
                  (at-beat (+ start dur) (release s))))))))))

(defun play-note-attr (snth &key
                       (synth-fn (lambda(b d s) (declare (ignorable b d))
                                   (when s (at-beat b (apply #'synth s)))))
                       (note-fn (lambda(e) [:freq (midicps e)]))
                       (release t))
  (lambda (b d e &optional a)
    (declare (ignorable b d))
    (when e
      (let* ((start (+ b (getf a :start (getf (cdr snth) :start 0))))
             (prb   (getf a :prob (getf (cdr snth) :prob)))
             (s (when (or (not prb) (< (random 1000000) (* prb 1000000)))
                  (typecase e
                    (%snt [(apply #'synth (cons (slot-value e 'synth) (slot-value e 'data)))])
                    (function (funcall e b d))
                    (list (funcall synth-fn start d (append snth (funcall note-fn (car e)))))
                    (t (funcall synth-fn start d (append snth (funcall note-fn e))))))))
        (when release
          (let ((dur (* d (getf a :dur (getf (cdr snth) :dur 1)))))
            (if (listp s)
                (at-beat (+ start dur) (mapcar (λ(s) (when (and s (equal 'cl-collider::node (type-of s))) (release s))) s))
                (when (equal 'cl-collider::node (type-of s))
                  (at-beat (+ start dur) (release s))))))))))

(defun play-note-x (snth &key (release t)
                         (synth-fn (lambda(b d s) (declare (ignorable b d))
                                     (when s (at-beat b (apply #'synth s))))))
  (lambda (e &optional a)
    (when e
      (let* ((start    (getf a :start (getf (cdr snth) :start 0)))
             (dur      (getf a :dur (getf (cdr snth) :dur 1)))
             (prb      (getf a :prob (getf (cdr snth) :prob)))
             (s (when (or (not prb) (< (random 1000000) (* prb 1000000)))
                  (typecase e
                    (%snt [(apply #'synth (cons (slot-value e 'synth) (slot-value e 'data)))])
                    (function (funcall e start dur))
                    (list (funcall synth-fn start dur (append snth (car e))))
                    (t (funcall synth-fn start dur (append snth (list e))))))))
        (when release
          (if (listp s)
              (at-beat (+ start dur) (mapcar (λ(s) (when (and s (equal 'cl-collider::node (type-of s))) (release s))) s))
              (when (equal 'cl-collider::node (type-of s))
                (at-beat (+ start dur) (release s)))))))))

(defun play-drum (&rest attrs)
  (λ(b d e &optional a)
    (declare (ignorable b d))
    (when (and e (not (equal e '-)))
      (if (listp e)
          (let ((prob (getf a :prob (getf attrs :prob))))
            (when (or (not prob) (< (random 1000000) (* prob 1000000)))
              (apply #'synth (cons (car e) (mergeplist attrs (cdr e))))))
          (apply #'synth (cons e attrs))))))

(defun m-play-drum ()
  (lambda (e a)
    (at-beat (getf a :start 0)
             (if (listp e) (apply #'synth e) (synth e)))))

(defun play-midi (mh &key (note-fn (λ(n) n)))
  (λ(b d e &optional a)
    (let ((dur (* d (getf a :dur 1))))
      (when e
        (spawn
          (sleep (- b (clock-beats))) 
          (midi-note-on mh (funcall note-fn e) (getf a :velo 127) (getf a :chan 0))
          ;(sleep (* dur (/ (clock-bpm) 60)))
          (sleep 0.1)
          (midi-note-off mh (funcall note-fn e) (getf a :velo 0) (getf a :chan 0))
          )))))

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

;;; Helper funcs for metro
(defun m-play-synth (snt)
  (lambda (e a)
    (let* ((start    (getf a :start 0))
           (dur      (getf a :dur 1))
           (note-len (getf a :note-len 1))
           (appl     (getf a :attr (list)))
           (s        (at-beat start (apply #'synth (append (cons snt e) appl)))))
      (at-beat (+ start (* dur note-len))
               (release s)))))

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
                       (if (functionp nl) (funcall nl) nl)))))

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

(defstruct knob%
  key
  (min 0)
  (max 127)
  (step 1)
  (value 0)
  (expt nil))

(let ((knob-registry (hshm)))
  (defun knob (name value &optional (step 0.1) (min 0) (max 1) expt)
    (let ((kn (href knob-registry name)))
      (when (not kn)
        (setf kn (hset knob-registry name
                       (make-knob% :key name
                                   :min min
                                   :max max
                                   :step step
                                   :value value
                                   :expt expt))))
      (cbus-set name value)
      (cbus-in name)))
  ;
  (defun knob-set (name midi-value)
    (when-let* ((kn (href knob-registry name))
                (mn (knob%-min kn))
                (mx (knob%-max kn))
                (vl (if (knob%-expt kn)
                        (* (knob%-value kn) (expt (knob%-step kn) (- midi-value 64)))
                        (+ (knob%-value kn) (* (knob%-step kn) (- midi-value 64)))))
                (nv (cond ((> vl mx) mx) ((< vl mn) mn) (t vl))))
      (setf (knob%-value kn) nv)
      (cbus-set name nv)
      (writeln "knob " name " = " (knob-value name))))
  ;
  (defun knob-value (name)
    (when-let ((kn (href knob-registry name)))
       (knob%-value kn)))
  ;
  (defun knob-clear ()
    (setf knob-registry (hshm))))

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

(defsynth clap ((out 0) (amp 0.5) (freq 1000))
  (-<> (white-noise.ar)
       (* amp (env-gen.kr (perc 0 0.16) :act :free))
       (bpf.ar freq)
       pan2.ar
       (out.ar out <>)))

(defsynth metronome ((freq 440) (amp 0.3) (out 0))
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
;;; updated seqs and metro

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

;; Metro

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
                (error (c) (writeln "ERROR " c))))
    (let ((next-beat (1+ bt)))
      (clock-add next-beat 'metro-play next-beat (1+ i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXPORTS

(export '(
          *s* sc-init sc-connect use-gtk λ mergelist def spawn spawn-named writeln cat dur sc chord ++. 1/
          seql seq siml sim seq-data seq-map play-seq snt snx
          ext-synth nsynth rand-el euclidian gen-list gen-rand gen-xrand
          loop-buf.kr loop-buf.ar
          make-mr-midi-event mr-midi-event-type mr-midi-event-note mr-midi-event-velocity mr-midi-event-start mr-midi-event-duration mr-midi-event-channel
          alsa-to-my-midi mk-midi-reader midi-reader-name midi-reader-seq midi-reader-in-port midi-reader-out-port
          start-midi-reader start-midi-writer stop-midi-reader midi-note-on midi-note-off
          per-beat per-beat-n once-every regpattern regpattern-ch defpattern pstart pstop pkill seq-schedule play-channels play-note play-note-attr play-drum play-midi
          m-play-synth m-play-drum
          csnt cbus cbuf cbus-set cbus-get cbus-in cbus-in-scale cbus-out abus abus-set abus-get abus-in abus-out
          knob knob-set knob-value knob-clear
          knob01 knob02 knob03 knob04 knob05 knob06 knob07 knob08 knob09 knob10 knob11 knob12 knob13 knob14 knob15 knob16
          ;; imported
          flatten curry compose if-let if-let* when-let when-let* random-elt shuffle rotate
          -> ->> -<> -<>> <>
          bdef bdef-metadata bdef-ensure-key match
          *chromatic* *pentatonic* *major* *minor*
          hshm href hset ? ! channel
          <events> <seq> <sim> ev-schedule S SA SL SAL U UA UL UAL A play-note-x metro-add metro-start metro-stop ev-map events attrib
          C-0 C0 C#0 Db0 D-0 D0 D#0 Eb0 E-0 E0 F-0 F0 F#0 Gb0 G-0 G0 G#0 Ab0 A-0 A0 A#0 Bb0 B-0 B0
          C-1 C1 C#1 Db1 D-1 D1 D#1 Eb1 E-1 E1 F-1 F1 F#1 Gb1 G-1 G1 G#1 Ab1 A-1 A1 A#1 Bb1 B-1 B1
          C-2 C2 C#2 Db2 D-2 D2 D#2 Eb2 E-2 E2 F-2 F2 F#2 Gb2 G-2 G2 G#2 Ab2 A-2 A2 A#2 Bb2 B-2 B2
          C-3 C3 C#3 Db3 D-3 D3 D#3 Eb3 E-3 E3 F-3 F3 F#3 Gb3 G-3 G3 G#3 Ab3 A-3 A3 A#3 Bb3 B-3 B3
          C-4 C4 C#4 Db4 D-4 D4 D#4 Eb4 E-4 E4 F-4 F4 F#4 Gb4 G-4 G4 G#4 Ab4 A-4 A4 A#4 Bb4 B-4 B4
          C-5 C5 C#5 Db5 D-5 D5 D#5 Eb5 E-5 E5 F-5 F5 F#5 Gb5 G-5 G5 G#5 Ab5 A-5 A5 A#5 Bb5 B-5 B5
          C-6 C6 C#6 Db6 D-6 D6 D#6 Eb6 E-6 E6 F-6 F6 F#6 Gb6 G-6 G6 G#6 Ab6 A-6 A6 A#6 Bb6 B-6 B6
          C-7 C7 C#7 Db7 D-7 D7 D#7 Eb7 E-7 E7 F-7 F7 F#7 Gb7 G-7 G7 G#7 Ab7 A-7 A7 A#7 Bb7 B-7 B7
          C-8 C8 C#8 Db8 D-8 D8 D#8 Eb8 E-8 E8 F-8 F8 F#8 Gb8 G-8 G8 G#8 Ab8 A-8 A8 A#8 Bb8 B-8 B8
          C-9 C9 C#9 Db9 D-9 D9 D#9 Eb9 E-9 E9 F-9 F9 F#9 Gb9 G-9 G9
          ))
