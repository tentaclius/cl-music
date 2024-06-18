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
      :in (list [:file "~/Mus/samples/hang-drum/hangA3.28.wav" :rate 6/5]
                [:file "~/Mus/samples/hang-drum/hangD4.24.wav" :rate 6/5]
                [:file "~/Mus/samples/hang-drum/hangD#4.24.wav" :rate 6/5]
                [:file "~/Mus/samples/hang-drum/hangF#4.26.wav" :rate 6/5]
                [:file "~/Mus/samples/hang-drum/hangG4.27.wav" :rate 6/5]
                [:file "~/Mus/samples/hang-drum/hangA4.34.wav" :rate 6/5]
                [:file "~/Mus/samples/hang-drum/hangA#4.35.wav" :rate 6/5]
                [:file "~/Mus/samples/hang-drum/hangC#5.28.wav" :rate 6/5]
                [:file "~/Mus/samples/hang-drum/hangD5.31.wav" :rate 6/5]
                ;
                [:file "~/Mus/samples/selection-drums/bass.wav" :color 5 :toggle :mono]
                [:file "~/Mus/samples/selection-drums/bottle.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/clap.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/cowbell.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/crash.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/hh-closed.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/hh-open.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/hh-pedal.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/kick-electro-2.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/kick-electro.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/knock.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/maracas.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/pop.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/shaker.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/sleigh.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/snap.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/snare_dull.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/snare-hit.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/snare-rim.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/snare.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/tom-low.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/tom-middle.wav" :color 14]
                [:file "~/Mus/samples/selection-drums/tom.wav" :color 14]
                ;
                [:file "~/Mus/samples/selection-misc/28a-prc01-125.wav" :color 88 :toggle :mono]
                [:file "~/Mus/samples/selection-misc/28a-prc05-125.wav" :color 88 :toggle :mono]
                [:file "~/Mus/samples/selection-misc/28a-prc06-125.wav" :color 88 :toggle :mono]
                [:file "~/Mus/samples/selection-misc/28a-prc07-125.wav" :color 88 :toggle :mono]
                [:file "~/Mus/samples/selection-misc/28b-prc05-125.wav" :color 88 :toggle :mono]
                [:file "~/Mus/samples/selection-misc/28b-prc12-125.wav" :color 88 :toggle :mono]
                [:file "~/Mus/samples/selection-misc/28c-prc01-125.wav" :color 88 :toggle :mono]
                [:file "~/Mus/samples/selection-misc/28c-prc05-125.wav" :color 88 :toggle :mono]
                [:file "~/Mus/samples/selection-misc/28c-prc09-125.wav" :color 88 :toggle :mono]
                [:file "~/Mus/samples/selection-misc/28c-prc14-125.wav" :color 88 :toggle :mono]
                ;
                [:file "~/Mus/samples/selection-misc/Ambiance_FX.wav" :color 56 :toggle :gate :rate 5/6 :release 1]
                [:file "~/Mus/samples/selection-misc/Astrl_Drms.wav" :color 56 :toggle :gate :rate 5/6 :release 1]
                [:file "~/Mus/samples/selection-misc/Csmc_Frnc_C.wav" :color 56 :toggle :gate :rate 5/6 :release 1]
                [:file "~/Mus/samples/selection-misc/Csmc_Wvs.wav" :color 56 :toggle :gate :rate 5/6 :release 1]
                [:file "~/Mus/samples/selection-misc/Digi_Thr_FX.wav" :color 56 :toggle :gate :rate 5/6 :release 1]
                [:file "~/Mus/samples/selection-misc/Dl_Fltr_C.wav" :color 56 :toggle :gate :rate 5/6 :release 1]
                [:file "~/Mus/samples/selection-misc/Dry_Wind.wav" :color 56 :toggle :gate :rate 5/6 :release 1]
                ;
                [:file "~/Mus/samples/selection-misc/LS_PTH_FXLOOP_002_125_G#m.wav" :color 120 :toggle :mono :amp 1/3]
                [:file "~/Mus/samples/selection-misc/LS_PTH_KICKLOOP_001_125.wav" :color 120 :toggle :mono]
                [:file "~/Mus/samples/selection-misc/LS_PTH_PERCUSSIONLOOP_013_125.wav" :color 120 :toggle :mono]
                [:file "~/Mus/samples/selection-misc/LS_PTH_TOPLOOP_025_125.wav" :color 120 :toggle :mono]
                [:file "~/Mus/samples/selection-misc/LS_TM_DRUMLOOP_017_125.wav" :color 120 :toggle :mono]
                [:file "~/Mus/samples/selection-misc/LS_TM_DRUMLOOP_042_125.wav" :color 120 :toggle :mono]
                [:file "~/Mus/samples/selection-misc/LS_TM_FXLOOP_011_125_F.wav" :color 120 :toggle :mono]
                [:file "~/Mus/samples/selection-misc/LS_TM_TOPLOOP_042_125.wav" :color 120 :toggle :mono]
                [:file "~/Mus/samples/selection-misc/Moon_Cycl.wav" :color 120 :toggle :mono]
                )
      :do (when (and pad-id file)
            (hset *pad-map* pad-id
                  (make-padspec :synth (list 'sample-dur
                                             :buffer (cbuf (getf file :file))
                                             :attack 0.007
                                             :release (getf file :release 0.1)
                                             :amp (getf file :amp 1/2)
                                             :rate (getf file :rate 1))
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
