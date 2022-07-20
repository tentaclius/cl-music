(require "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :midi-loop-player (:use :cl :sc :mylisp :midi-looper))
(in-package :midi-loop-player)
(named-readtables:in-readtable :sc)
(sc-init)
(def mr (make-midi-track))
(def *running-synths* (hshm))
;
(defsynth synth ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.001) (d 0.2) (s 0.7) (r 0.5))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (saw.ar fq)
         (+ (* 1/3 (saw.ar (+ fq 3))))
         (rlpf.ar (* fq 4) 0.5)
         (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
         pan2.ar (out.ar out <>))))

(defun midi-map (messages)
  (dolist (message messages)
    (writeln message)
    (let ((msg (alsa-to-my-midi message)))
      (case (mr-midi-event-type msg)
        (:note_on
         (hset *running-synths* (mr-midi-event-note msg)
               (synth 'synth :freq (midicps (mr-midi-event-note msg)) :amp (/ (mr-midi-event-velocity msg) 127))))
        (:note_off
         (when-let ((snt (href *running-synths* (mr-midi-event-note msg))))
                   (release snt))))
      (register-midi-event mr msg))))

(midihelper:stop-midihelper)
(midihelper:start-midihelper :master 96 'midi-map)

;;; TESTS ;;;

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil))
      (sim (per-beat i
             (seq 'hh))))))

(defpattern metronome
  (play-note 'metronome
             :release nil
             :attr []
             :note-fn (λ(n) [:freq (midicps (+ 54 12 (sc *pentatonic* n)))]))
  (λ(i)
    (per-beat i
    (seq 5 0)
    (seq 0 0)
    (seq (seq 5 5) 0)
    (seq 0 0)
    )))
(defun midi-player (beat)
  (dolist (m (recall-midi-events mr (ceiling (clock-beats))))
    (let ((dur (quantize-beat (mr-midi-event-duration m) 8))
          (start (quantize-beat (mr-midi-event-start m) 4)))
      (when (and start dur)
        (dur (+ beat start) dur ['synth :freq (midicps (mr-midi-event-note m)) :amp (/ (mr-midi-event-velocity m) 127)]))))
  (clock-add (1+ beat) #'midi-player (1+ beat)))

(def mr (make-midi-track))
(start-midi-recording mr 8)
(midi-player (clock-quant 8))
(metronome :start 8)

(drums :stop)

(print-midi-events mr 8)

(ssn :start)
(ssn :stop)

(stop-midi-recording mr)
(stop)
