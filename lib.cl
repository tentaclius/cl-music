;; -----------------------------------------------------------------------------------------------------
;; Boot the server
;; {{{

(ql:quickload :cl-collider)
(ql:quickload :sc-extensions)
(ql:quickload '(bdef/cl-collider))
(ql:quickload :alexandria)
;(ql:quickload :rutils)
(ql:quickload :arrow-macros)
(ql:quickload :generators)
(ql:quickload :bordeaux-threads)
(ql:quickload :trivial-arguments)
(ql:quickload :f-underscore)
(ql:quickload :trivia)
(in-package :sc-user)
(use-package :sc-extensions)
(use-package :arrow-macros)
(use-package :f-underscore)
;
(shadowing-import '(alexandria:flatten alexandria:curry alexandria:compose
                                       alexandria:if-let alexandria:when-let alexandria:random-elt alexandria:shuffle alexandria:rotate
                                       generators:make-generator generators:next generators:yield
                                       bdef:bdef bdef:bdef-metadata
                                       trivia:match
                                       ))
;
(named-readtables:in-readtable :sc)

(in-package #:sc)
(defugen (loop-buf "LoopBuf")
    (chanls bufnum &key (rate 1.0) (gate 1) (start-pos 0.0) (start-loop 0.0) (end-loop 0.0) (interpolation 2))
  ((:ar (multinew new 'multiout-ugen chanls bufnum rate gate start-pos start-loop end-loop interpolation))
   (:kr (multinew new 'multiout-ugen chanls bufnum rate gate start-pos start-loop end-loop interpolation))))
(in-package #:sc-user)

(defun init (&optional (port 57110))
  (setf *sc-plugin-paths* nil)
  (setf *s* (make-external-server "localhost" :port port :server-options (make-server-options :realtime-mem-size 16384)))
  (server-boot *s*)
  (jack-connect)
  (init-synths))

(defun connect (&optional (port 57110))
  (setf *sc-plugin-paths* nil)
  (setf *s* (make-external-server "localhost" :port port :just-connect-p t))
  (server-boot *s*)
  (init-synths))

;; }}}

;; -----------------------------------------------------------------------------------------------------
;; My function library
;; {{{

;; general utility

(defun mergeplist (a b)
  (let ((n ()))
    (loop :for (k v) :on a :by #'cddr :do (setf (getf n k) v))
    (loop :for (k v) :on b :by #'cddr :do (setf (getf n k) v))
    n))

(defmacro λ (&rest body) `(lambda ,@body))

(defmacro def (&rest pairs)
  `(progn ,@(loop :for (k v) :on pairs :by #'cddr :collect `(defparameter ,k ,v))))

(defmacro spawn (&body body)
  `(bordeaux-threads:make-thread (lambda() ,@body)))

(defun writeln (&rest args)
  (loop :for el :in args :do (princ el))
  (terpri)
  (car args))

(defun cat (&rest vals)
  (let ((str (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s str)
      (loop :for vl :in vals :do (format s "~a" vl)))
    (string str)))

(defmacro %> (&rest forms)
  `(let* (,@(map 'list (lambda(n) `(% ,n)) forms)) %))

;; sc

(defun dur (bt tm &rest spec)
  (let ((running-synths (mapcar (lambda (s) (at-beat bt (apply #'synth s))) spec)))
    (at-beat (+ bt tm) (loop :for synt :in running-synths :do (release synt)))))

(defun sc (scale step)
  (+ (aref scale (mod step (length scale)))
     (* 12 (floor (/ step (length scale)))) ))

;; my sequences

(defclass %seq () (data))
(defun seql (data)
  (let ((ss (make-instance '%seq)))
    (setf (slot-value ss 'data) data)
    ss))
(defun seq (&rest data)
  (seql data))

(defclass %sim () (data))
(defun siml (data)
  (let ((ss (make-instance '%sim)))
    (setf (slot-value ss 'data) data)
    ss))
(defun sim (&rest data)
  (siml data))

(defun seq-data (ss)
  (slot-value ss 'data))

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
       ,@(loop :for el :in seqs :for n :from 0 :collect `((,n) ,el))
       )))

(defun once-every (i n r sq)
  (if (= (mod i n) r) sq nil))

;; TODO
;; - rewrite with pattern matching
;; - get rid of unnecessary gensym's
;(defmacro defpattern (name fun pattern-fn)
;  (let ((beat (gensym))
;        (i (gensym))
;        (j (gensym))
;        (repeats (gensym))
;        (a-repeats (gensym))
;        (exe (read-from-string (concatenate 'string (string name) "-exe"))))
;    `(progn
;       (defparameter ,name ,pattern-fn)
;       (defparameter ,exe ,fun)
;       (let ((,i 0)
;             (,repeats +inf+))
;         (defun ,name (,beat &optional ,j ,a-repeats)
;           (when (eq ,beat :stop)
;             (setf ,repeats (+ ,i (if ,j ,j 0))))
;           (when (eq ,beat :start)
;             (setf ,repeats (if ,a-repeats ,a-repeats +inf+))
;             (setf running-p t)
;             (setf ,beat (clock-quant (or ,j 1)))
;             (setf ,j 0)
;             (setf ,i 0))
;           (when (< ,i ,repeats)
;             (labels ((dispatch (beat frame seq)
;                        (typecase seq
;                          (%sim (chord beat frame (seq-data seq)) t)
;                          (%seq (sched beat frame (seq-data seq)) t)))
;                      (chord (beat frame seq)
;                        (loop :for el :in seq :do
;                              (or (dispatch beat frame el)
;                                  (at-beat beat (funcall ,exe beat frame el)))))
;                      (sched (beat frame seq)
;                        (let ((delta-t (/ frame (length seq))))
;                          (loop :for el :in seq :for j :from 0 :do
;                                (let ((tm (+ beat (* j delta-t))))
;                                  (or (dispatch tm delta-t el)
;                                      (at-beat tm (funcall ,exe tm delta-t el))))))))
;               (handler-case
;                   (dispatch ,beat 1 (funcall ,name ,j))
;                 (error (c) (writeln "ERROR: " c))))
;             (let ((next-beat (+ ,beat 1)))
;               (setf ,i (1+ ,i))
;               (clock-add next-beat #',name next-beat (1+ ,j)))))))))
(defmacro defpattern (pattern-name a-synth-fn a-pattern-fn)
  (let ((pattern-fn (gensym)) (synth-fn (gensym)))
    `(progn
       (defparameter ,pattern-fn ,a-pattern-fn)
       (defparameter ,synth-fn ,a-synth-fn)
       (let ((i 0)
             (repeats +inf+)
             (stop-quant nil))
         (defun ,pattern-name (&rest args) ;; (beat &optional ,j ,a-repeats)
           (trivia:match args
             ((list :stop)        (setf repeats 0))
             ((list :stop quant)  (setf stop-quant (clock-quant quant)))
             ((list :start)       (,pattern-name :start 1 +inf+))
             ((list :start quant) (,pattern-name :start quant +inf+))
             ((list :start quant reps)
                (setf i 0) (setf repeats reps) (setf stop-quant nil)
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
                        (dispatch beat 1 (funcall ,pattern-fn j))
                      (error (c) (writeln "ERROR: " c))))
                  (let ((next-beat (+ beat 1)))
                    (setf i (1+ i))
                    (clock-add next-beat #',pattern-name next-beat (1+ j)))))))))))

(defun play-note (snth &key 
                       (synth-fn (lambda(s e) (when (and s e) [(apply #'synth (cons s e))])))
                       (note-fn (lambda(e) [:freq (midicps e)])) (release t) attr)
  (lambda (b d e)
    (declare (ignorable b d))
    (when e
      (let ((s (typecase e
                 (%snt [(apply #'synth (cons (slot-value e 'synth) (slot-value e 'data)))])
                 (function (funcall e b d))
                 (list (funcall synth-fn snth (append (funcall note-fn (car e)) (mergeplist attr (cdr e)))))
                 (t (funcall synth-fn snth (append (funcall note-fn e) attr))))))
        (when release 
          (let ((dur (or (and (listp e) (getf (cdr e) :dur))
                         (getf attr :dur d))))
            (if (listp s)
                (at-beat (+ b dur) (mapcar (λ(s) (when (and s (equal 'cl-collider::node (type-of s))) (release s))) s))
                (when (equal 'cl-collider::node (type-of s))
                  (at-beat (+ b dur) (release s))))))))))

(defun play-ctrl (prx &optional (default-ctl :freq) (release-p t))
  (λ(b d e)
    (declare (ignorable b d))
    (when e
      (if (listp e)
          (let ((note (getf e :note)))
            (apply #'ctrl (append [:gate 1] (if note [:freq (midicps note)] nil) e)))
          (ctrl default-ctl e))
      (when release-p (at-beat (+ b d) (release prx))))))

(defun play-drum (&rest attrs)
  (λ(b d e)
    (declare (ignorable b d))
    (when (and e (not (equal e '-)))
      (if (listp e)
          (apply #'synth (cons (car e) (mergeplist attrs (cdr e))))
          (apply #'synth (cons e attrs))))))

(defun play-function ()
  (λ(b d e)
    (declare (ignorable b d))
    (when e (funcall e b d))))

;; Additional synthesis with sin-osc
(defmacro sin-n (num &key (freq-fn (λ(i) (* 440 i))) (amp-fn (λ(i) (* 0.3 (/ 1 i)))))
  `(sc::+~ ,@(loop :for i :from 1 :to num :collect 
              `(sc::*~ (sin-osc.ar (funcall ,freq-fn ,i)) (funcall ,amp-fn ,i)))))

(defvar *chromatic* #(0 1 2 3 4 5 6 7 8 9 10 11))
(defvar *pentatonic* #(0 3 5 7 10))
(defvar *major* #(0 2 4 5 7 9 11))
(defvar *minor* #(0 2 3 5 7 8 10))
(defvar *minor3* #(0 3 7))
(defvar *major3* #(0 4 7))

(defun ext-synth (name &rest args)
  (let ((id (sc::get-next-id *s*)))
    (apply (curry #'send-message *s* "/s_new" name id 0 0) args)
    id))

(defun rand-el (&rest args)
  (random-elt args))

;(defun synth-cs (name &rest args)
;  "Start a synth by name."
;  (let* ((name-string (symbol-name name))
;         (next-id (or (getf args :id) (get-next-id *s*)))
;         (to (or (getf args :to) 1))
;         (pos (or (getf args :pos) :head))
;         (new-synth (make-instance 'node :server *s* :id next-id :name name-string :pos pos :to to))
;         (parameter-names (mapcar (lambda (param) (string-downcase (car param))) (synthdef-metadata name :controls)))
;         (args (loop :for (arg val) :on args :by #'cddr
;           :for pos = (position (string-downcase arg) parameter-names :test #'string-equal)
;           :unless (null pos)
;             :append (list (string-downcase (nth pos parameter-names)) (floatfy val)))))
;    (message-distribute new-synth
;         (apply #'make-synth-msg *s* name-string next-id to pos args)
;         *s*)))

(defun euclidian (n m snt &optional (nl nil))
  (let ((ii 0))
    (loop :for i :from 0 :to (1- m)
          :collect (if (= (floor (* ii (/ m n))) i)
                       (prog1 snt (incf ii))
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
        el
        ))))

(defun gen-rand (lst)
  (lambda ()
    (random-elt lst)))

;; }}}

;; hash-table literal syntax using braces
(set-macro-character #\{
                     (lambda (str char)
                       (declare (ignore char))
                       (let ((*readtable* (copy-readtable *readtable* nil))
                             (keep-going t))
                         (set-macro-character #\} (lambda (stream char)
                                                    (declare (ignore char) (ignore stream))
                                                    (setf keep-going nil)))
                         (let ((pairs (loop for key = (read str nil nil t)
                                            while keep-going
                                            for value = (read str nil nil t)
                                            collect (list key value)))
                               (retn (gensym)))
                           `(let ((,retn (make-hash-table :test #'equal :synchronized t)))
                              ,@(mapcar
                                  (lambda (pair)
                                    `(setf (gethash ,(car pair) ,retn) ,(cadr pair)))
                                  pairs)
                              ,retn)))))

;; -----------------------------------------------------------------------------------------------------
;; Instruments
;; {{{

(defun init-synths ()

;; Helper synths

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

(defsynth sample-dur ((buffer 0) (rate 1) (start 0) (amp 0.5) (out 0) (loop 0) (dur 60) (attack 0.1) (release 0.1))
  (let* ((sig (play-buf.ar 2 buffer (* rate (buf-rate-scale.ir buffer))
                           :start-pos (* start (/ (buf-frames.ir buffer) (buf-channels.ir buffer)))
                           :loop loop
                           :act :free))
         (sig (* sig (env-gen.kr (linen attack (- dur attack release) release) :act :free))))
    (out.ar out (* amp sig)) ))

(defsynth sample-dur-1 ((buffer 0) (rate 1) (start 0) (amp 0.5) (out 0) (loop 0) (dur 60) (attack 0.1) (release 0.1))
  (let* ((sig (play-buf.ar 1 buffer (* rate (buf-rate-scale.ir buffer))
                           :start-pos (* start (/ (buf-frames.ir buffer) (buf-channels.ir buffer)))
                           :loop loop
                           :act :free))
         (sig (* sig (env-gen.kr (linen attack (- dur attack release) release) :act :free))))
    (out.ar out (* amp (pan2.ar sig))) ))

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

(defsynth ssin ((freq 440) (freq0 440) (slide 0) (out 0) (amp 0.5) (gate 1)
                        (a 0.01) (d 0.2) (s 0.4) (r 0.4))
  (-<> (x-line.kr freq0 freq slide)
       (sin-osc.ar)
       (* amp (env-gen.kr (adsr a d s r) :act :free :gate gate))
       pan2.ar (out.ar out <>)))

(defsynth ssaw ((freq 440) (freq0 440) (slide 0) (out 0) (amp 0.5) (gate 1)
                        (a 0.01) (d 0.2) (s 0.4) (r 0.4))
  (-<> (x-line.kr freq0 freq slide)
       (saw.ar)
       (* amp (env-gen.kr (adsr a d s r) :act :free :gate gate))
       pan2.ar (out.ar out <>)))

;; drums
(defsynth bd ((freq 330) (bass 40) (dur 0.09) (d 0.3) (out 0) (amp 0.5))
  (-<> (x-line.ar freq bass dur)
       (sin-osc.ar)
       (* amp (env-gen.kr (env [0 1 1 0] [0.001 dur d]) :act :free))
       pan2.ar (out.ar out <>)))

(defsynth hh ((out 0) (amp 0.3) (dur 0.1))
  (-<> (white-noise.ar)
       (hpf.ar 8000)
       (* amp (env-gen.kr (perc 0.0 dur) :act :free))
       pan2.ar (out.ar out <>)
    ))

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
       (out.ar out <>)
       ))

)
;; }}}
