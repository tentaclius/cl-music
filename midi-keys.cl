(require "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :play (:use :cl :sc :mylisp :midi-looper))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)
;
(def *running-synths* (hshm))
(def *ctrl-map* (hshm 114 :keys-feedback 112 :keys-delay  18 :keys-drop))
(def main-synth 'keys)
;(def main-synth 'saw-bass)

;; Synths

(defsynth saw-bass ((freq 440) (freq0 440) (slide 0) (out 0) (amp 0.5) (gate 1)
                           (lpf 7) (res 1)
                           (a 0.01) (d 0.2) (s 0.4) (r 0.04))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> 
      (saw.ar fq)
      (rlpf.ar (* fq (cbus-in :q) 40/127) res)
      ;(+ (sin-osc.ar fq))
      (* amp (env-gen.kr (adsr a d s r) :act :free :gate gate))
      pan2.ar (out.ar out <>))))

(cbus-set :keys-feedback 40)
(cbus-set :keys-delay 0.3)
(cbus-set :keys-drop 10)
(proxy :keys
       (-<> (in.ar (abus :keys) 2)
            (* (-> 10 sin-osc.ar (range 0.7 1)))
            (+ <> (* (/ 1 (+ 2 (cbus-in :keys-drop))) (comb-c.ar <> 2 (* (cbus-in :keys-delay) 1/127) (cbus-in :keys-feedback))))
            ) :pos :tail)
(defsynth keys ((gate 1) (freq 440) (amp 0.5) (out (abus :keys)))
  (-<> (dyn-klang.ar [[freq (* freq 2) (* freq 3)] [1/2 1/4 1/8]])
       ;(+ (-> (white-noise.ar) (bpf.ar freq) (* 1/10)))
       (* amp (env-gen.kr (adsr 0.008 0.1 0.8 0.2) :act :free :gate gate))
       pan2.ar (out.ar out <>)))

;; Midi

(defun midi-map (messages)
  (dolist (message messages)
    (when-let ((msg (alsa-to-my-midi message)))
      ;(writeln msg)
      (case (mr-midi-event-type msg)
        (:note_on
         ;(writeln msg)
         (hset *running-synths* (mr-midi-event-note msg)
               (synth main-synth :freq (midicps (mr-midi-event-note msg)) :amp (/ (mr-midi-event-velocity msg) 127))))
        (:note_off
         (when-let ((snt (href *running-synths* (mr-midi-event-note msg))))
                   (release snt)))
        (:control
         (when-let* ((ctl-num (mr-midi-event-note msg))
                     (ctl-val (mr-midi-event-velocity msg))
                     (ctl-bus (href *ctrl-map* ctl-num)))
                    (writeln "CTRL_SET " ctl-bus " = " ctl-val)
                    (cbus-set ctl-bus ctl-val))))
      ;(writeln msg)
      ;(register-midi-event mr msg)
      )))

(midihelper:stop-midihelper)
(midihelper:start-midihelper :master 96 'midi-map)

;; Patterns

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil)
          (d ['bd :dur 0.05]))
      (sim nil
           ;(seq nil ['snare :d 0.1])
           ;(seql (euclidian 3 8 'hh ['hh :amp 0.1]))
           (per-beat i
             (seq d d)
             (seq d d)
             (seq d d)
             (seq d o o o  d d o o)
             )))))

(drums :start 8)
(drums :stop)

(defun midi-player (beat)
  (dolist (m (recall-midi-events mr (ceiling (clock-beats))))
    (let ((dur (quantize-beat (mr-midi-event-duration m) 128))
          (start (quantize-beat (mr-midi-event-start m) 128)))
      (when (and start dur)
        (dur (+ beat start) dur
             [main-synth :freq (midicps (mr-midi-event-note m)) :amp (/ (mr-midi-event-velocity m) 127)]))))
  (clock-add (1+ beat) #'midi-player (1+ beat)))

(midi-player (clock-quant 8))

(def mr (make-midi-track))
(start-midi-recording mr 8)

(stop-midi-recording mr)

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

(metronome :start 8)
(metronome :stop)


(stop)
