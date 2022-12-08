(require "mylisp" "init.cl")
(defpackage :arpegiator (:use :cl :sc :mylisp))
(in-package :arpegiator)
(named-readtables:in-readtable :sc)
(sc-init)
(def sample-rate 48000)

(def *pressed-keys* (list))
(def *playing-seq* (list))
(def *num-pressed* 0)
;
(defun event-handler (msg)
  (case (mr-midi-event-type msg)
    ((:note_on)
     (incf *num-pressed*)
     (push (mr-midi-event-note msg) *pressed-keys*))
    ((:note_off)
     (decf *num-pressed*)
     (when (= *num-pressed* 0)
       (setf *playing-seq* (reverse *pressed-keys*))
       (setf *pressed-keys* nil)))))

(def mh (mk-midi-reader "CLarp"))
(start-midi-reader mh 'event-handler)
(start-midi-writer mh)

(def *synth* ['ssaw :amp 0 :dur 1/10 :a 0.1 :r 0.7])
(defpattern arpegiator
  (let ((pn (play-note-attr *synth*))
        (pm (play-midi mh)))
    (λ(b d e)
      (funcall pm b d e)
      (funcall fn b d e)))
  (λ(i) (seql (loop :for n :in *playing-seq* :collect n))))

(arpegiator :start)
(arpegiator :stop)


(defpattern bass
  (play-note-attr ['saw-bass :amp 0.4 :lpf 3 :res 0.2]
             :release t
             :note-fn (λ(n) [:freq (midicps (+ 57 -24 (sc *minor* n)))]))
  (λ(i)
    (per-beat i
      (seq 0)
      (seq 3)
      (seq 2)
      (seq 5)
      ))
  2)

(bass :start 4)
(bass :stop 4)


(defpattern drums
  (play-drum :amp 2/3)
  (λ(i)
    (let ((o nil)
          (d 'bd)
          (s (list 'snare :freq 2000 :d 0.15))
          (h 'hh))
      (sim 
        (seq o s)
        (seq o h o h)
        ;(per-beat i
        ;  (seq d d)
        ;  (seq d d)
        ;  (seq d d)
        ;  (seq d (seq d d o o))
        ;  )
        ))))

(drums :stop 4)
(drums :start 4)


(defun play-midi (mh &key (note-fn (λ(n) n)))
  (λ(b d e)
    (spawn
      (sleep (- b (clock-beats))) 
      (midi-note-on mh (funcall note-fn e))
      (sleep (- (* d (/ (clock-bpm) 60)) 0.03))
      (midi-note-off mh (funcall note-fn e))
      )))

(defpattern test
  (play-midi mh :note-fn (λ(n) (+ 54 -12 (sc *pentatonic* n))))
  (λ(i)
    (per-beat
      i
      (seql (euclidian 3 8 0 3))
      (seql (euclidian 3 8 0 1))
      (seql (euclidian 3 8 0 3))
      (seql (euclidian 3 8 0 2))
      ))
  2)

(test :start 4)
(test :stop)
