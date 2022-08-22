(require "mylisp" "init.cl")
(defpackage :play (:use :cl :sc :mylisp))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
;
(def sample-rate 48000)
(def mh (mk-midi-reader "CLseq124audio"))
;
(def *record-bus* 8)
(def *synth-map* (make-hash-table))
(def *buffer-map* (make-hash-table))
;
(defsynth recorder ((bus 1) (buffer 0) (dur 15) (gate 1))
  (record-buf.ar (* (in.ar bus 2) (env-gen.kr (adsr 0.001 0.001 1 0.001) :gate gate :act :free)) buffer))

(defun event-handler (msg)
  (let ((note (mr-midi-event-note msg)))
    (when (and msg (equal (mr-midi-event-type msg) :note_on))
      (if-let ((buf (href *buffer-map* note))) ;; if buffer exists
        (progn ;; then: release previous synth, start playing
          (when-let ((snt (href *synth-map* note))) (release snt))
          (hset *synth-map* note (synth 'sample :buffer (href *buffer-map* note) :amp 1))
          (writeln "playing " note))
        (progn ;; else: start recording
          (let ((buf (buffer-alloc (* 70 sample-rate) :chanls 2)))
            (hset *synth-map* note (synth 'recorder :bus *record-bus* :buffer buf))
            (hset *buffer-map* note buf)
            (writeln "recording " note)))))
    (when (and msg (equal (mr-midi-event-type msg) :note_off))
      (when-let ((snt (href *synth-map* note)))
        (release snt)
        (hset *synth-map* note nil)
        (writeln "stop synth " note)))))

(start-midi-reader mh 'event-handler)
