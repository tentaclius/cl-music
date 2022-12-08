(require "mylisp" "init.cl")
(defpackage :mpg (:use :cl :sc :mylisp))
(in-package :mpg)
(named-readtables:in-readtable :sc)
(sc-init)
(def sample-rate 48000)

(def midi-chan (make-instance 'channel))
(spawn-named 
  :midi-sender
  (loop :do (let ((n (? midi-chan)))
              (match n
                ((list* handle :on param)
                 (apply #'midi-note-on (cons handle param)))
                ((list* handle :off param)
                 (apply #'midi-note-off (cons handle param)))))))

(defun event-handler (msg)
  (case (mr-midi-event-type msg)
    ((:note_on)
     )))

(def mh (mk-midi-reader "CLarp"))
(start-midi-reader mh 'event-handler)
(start-midi-writer mh)

(defun sndnote (note &optional (velo 127) (chan 0))
  (spawn
    (midi-note-on mh note velo chan)
    (sleep 0.1)
    (midi-note-off mh note 0 chan)))

(def ptrn1 (gen-list (mapcar (λ(n) (+ 42 (sc *pentatonic* n)))
                             [0 1 2 3 5])))

(defun play-midi (handle &key (note-fn (λ(n) n)))
  (λ(b d e)
    (when e
      (spawn
        (sleep (- b (clock-beats) 0.01)) 
        (! midi-chan [handle :on (funcall note-fn e)])
        (sleep (- (* d (/ (clock-bpm) 60)) 0.02))
        (! midi-chan [handle :off (funcall note-fn e)])
        ))))

(def ptt1 (gen-list [0 1 2 7 4 8 9 5]))

(defpattern eucl
  (play-midi mh :note-fn (λ(n) (+ 42 (sc *pentatonic* n))))
  (λ(i)
    (seql (loop :repeat 6 :collect (funcall ptt1)))
    ;(seq 0 1 2 3)
    ))

(eucl :start)
(eucl :stop)


(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil)
          (b 'bd)
          (s 'snare)
          (h 'hh))
      (sim (per-beat i
             (seq b h b h))))))

(drums :start)
(drums :stop)


(def drums-mh (mk-midi-reader "CLdrums"))
(start-midi-writer drums-mh)

(defpattern drums
  (λ(b d e)
    (when e 
      (spawn
        (sleep (- b (clock-beats)))
        (! midi-chan (list drums-mh :on e)))))
  (λ(i)
    (let ((o nil)
          (d 36)
          (c 42)
          (s 38)
          (m 37))
      (sim (seq d d)
           ))))

(drums :start)
(drums :stop)
