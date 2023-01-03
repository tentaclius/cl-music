(require "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :play (:use :cl :sc :mylisp :midi-looper))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)

(def *midi-channel* 0)
;
(defun light-pad (note color)
  (midi-note-on mh note color))
;
(defstruct padspec
  synth
  toggle
  off-color
  on-color
  (amp-sensitive nil)
  instance)
;
(proxy :mixer
       (-<> (abus-in :master)
            ;(if~ (cbus-in :ctl-reverb) (freeverb.ar <> :mix 0.2 :room 0.6) <>)
            ;(if~ (cbus-in :ctl-hpf) (hpf.ar <> 2000) <>)
            ;(if~ (cbus-in :ctl-lpf) (lpf.ar <> 200) <>)
            ;(if~ (cbus-in :ctl-bpf) (bpf.ar <> (range (sin-osc.kr 0.4) 100 7000)) <>)
            ) :pos :tail)

;(labels ((s (synth &optional (toggle :gate) (off-color 0) (on-color *active-color*) (amp-sensitive nil))
;           (make-padspec :synth synth :toggle toggle :off-color off-color :on-color on-color :amp-sensitive amp-sensitive))
;         (dl (buff &optional (toggle :gate) (off-color *drumloops-color*) (on-color *active-color*))
;           (s ['sample-loop :out (abus :master) :buffer (cbuf buff) :attack 0.001 :release 0.01 :end-loop 84662]
;              toggle off-color on-color))
;         (fx (buff &optional (rate 1) (toggle :once) (off-color *fx-color*) (on-color *active-color*))
;           (s ['sample-dur :out (abus :master) :buffer (cbuf buff) :attack 0.1 :release 3 :amp 0.9 :rate rate]
;              toggle off-color on-color))
;         (aa (buff &optional (rate 1) (toggle :once) (off-color *fx-color*) (on-color *active-color*))
;           (s ['sample-dur :out (abus :master) :buffer (cbuf buff) :dur 5 :attack 0.001 :release 3 :amp 0.9 :rate rate]
;              toggle off-color on-color t))
;         (os (buff &optional (rate 1) (toggle :gate) (off-color *oneshot-color*) (on-color *active-color*))
;           (s ['sample :out (abus :master) :buffer (cbuf buff) :rate rate :attack 0.001 :release 1]
;              toggle off-color on-color t))
;         (fn (func &optional (off-color 0) (on-color *active-color*))
;           (s func :function off-color on-color)))

; 64 65 66 67  96 97 98 99
; 60 61 62 63  92 93 94 95
; 56 57 58 59  88 89 90 91
; 52 53 54 55  84 85 86 87
; 48 49 50 51  80 81 82 83
; 44 45 46 47  76 77 78 79
; 40 41 42 43  72 73 74 75
; 36 37 38 39  68 69 70 71
(def
  *active-color*     3
  *drumloops-color*  5
  *fx-color*         18
  *noise-color*      14
  *oneshot-color*    40
  *fn-on-color*      18
  *fn-off-color*     5)
;
(def *pad-map* (hshm))
(loop :for pad-id
      :in (list
            36 37 38 39 68 69 70 71
            40 41 42 43 72 73 74 75
            44 45 46 47 76 77 78 79
            48 49 50 51 80 81 82 83
            52 53 54 55 84 85 86 87
            56 57 58 59 88 89 90 91
            60 61 62 63 92 93 94 95
            64 65 66 67 96 97 98 99
            )
      :for file
      :in (list [:file "~/Mus/samples/hang-drum/hangA3.28.wav"]
                [:file "~/Mus/samples/hang-drum/hangD4.24.wav"]
                [:file "~/Mus/samples/hang-drum/hangD#4.24.wav"]
                [:file "~/Mus/samples/hang-drum/hangF#4.26.wav"]
                [:file "~/Mus/samples/hang-drum/hangG4.27.wav"]
                [:file "~/Mus/samples/hang-drum/hangA4.34.wav"]
                [:file "~/Mus/samples/hang-drum/hangA#4.35.wav"]
                [:file "~/Mus/samples/hang-drum/hangC#5.28.wav"]
                [:file "~/Mus/samples/hang-drum/hangD5.31.wav"]
                ;
                [:file "~/Mus/samples/selection-drums/bass.wav" :color 5 :toggle :mono]
                [:file "~/Mus/samples/selection-drums/bottle.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/clap.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/cowbell.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/crash.wav"]
                [:file "~/Mus/samples/selection-drums/hh-closed.wav"]
                [:file "~/Mus/samples/selection-drums/hh-open.wav"]
                [:file "~/Mus/samples/selection-drums/hh-pedal.wav"]
                [:file "~/Mus/samples/selection-drums/kick-electro-2.wav"]
                [:file "~/Mus/samples/selection-drums/kick-electro.wav"]
                [:file "~/Mus/samples/selection-drums/knock.wav"]
                [:file "~/Mus/samples/selection-drums/maracas.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/pop.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/shaker.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/sleigh.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/snap.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/snare_dull.wav"]
                [:file "~/Mus/samples/selection-drums/snare-hit.wav"]
                [:file "~/Mus/samples/selection-drums/snare-rim.wav"]
                [:file "~/Mus/samples/selection-drums/snare.wav"]
                [:file "~/Mus/samples/selection-drums/tom-low.wav"]
                [:file "~/Mus/samples/selection-drums/tom-middle.wav"]
                [:file "~/Mus/samples/selection-drums/tom.wav"]
                )
      :do (when (and pad-id file)
            (hset *pad-map* pad-id
                  (make-padspec :synth (list 'sample-dur :buffer (cbuf (getf file :file)) :attack 0.001 :release 0.01 :amp 0.9 :rate 1)
                                :toggle (getf file :toggle :once)
                                :on-color 3
                                :off-color (getf file :color 40)
                                :amp-sensitive (getf file :sensitive t)))))

(defun play-pad (note velo chan)
  (when-let ((padspec (href *pad-map* note)))
    (if (> velo 0)
        ;; (velo > 0) start a synth
        (progn
          (case (padspec-toggle padspec)
            (:once
             (apply #'synth (if (padspec-amp-sensitive padspec)
                                (append (padspec-synth padspec) [:amp (/ velo 127)])
                                (padspec-synth padspec))))
            (:gate
             (setf (padspec-instance padspec)
                   (apply #'synth (if (padspec-amp-sensitive padspec)
                                      (append (padspec-synth padspec) [:amp (/ velo 127)])
                                      (padspec-synth padspec)))))
            (:mono
             (csnt note (apply #'synth
                               (if (padspec-amp-sensitive padspec)
                                   (append (padspec-synth padspec) [:amp (/ velo 127)])
                                   (padspec-synth padspec)))))
            (:function
             (funcall (padspec-synth padspec) padspec note (/ velo 127))))
          (when (not (functionp (padspec-synth padspec)))
            (light-pad note (padspec-on-color padspec))))
        ;; (velo = 0) stop the synth
        (if (not (functionp (padspec-synth padspec)))
            ;; synth, stop it
            (progn
              (when-let ((s (padspec-instance padspec)))
                        (when (is-playing-p s) (release s))
                        (setf (padspec-instance padspec) nil))
              (light-pad note (padspec-off-color padspec)))
            ;; function, call it again
            (funcall (padspec-synth padspec) padspec note 0)
          ))))

(defun event-handler (msg)
  (when msg
    (case (mr-midi-event-type msg)
      ((:note_on :note_off)
       (play-pad (mr-midi-event-note msg) (mr-midi-event-velocity msg) (mr-midi-event-channel msg))))))

(def mh (mk-midi-reader "CLLaunchpad"))
(start-midi-reader mh 'event-handler)
(start-midi-writer mh)


(proxy :drums-px
  (-<> (abus-in :drums-chan)
       ) :pos :tail)
(regpattern :drums
  (play-drum :out (abus :drums-chan))
  (λ(i)
    (let ((o nil)
          (b ['bd])
          (h ['hh])
          (s ['snare]))
      (per-beat i
        (seq )))))

(pstart :drums)
(pstop :drums)
