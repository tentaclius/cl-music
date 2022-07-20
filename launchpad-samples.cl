(equire "mylisp" "init.cl")
(require "midi-looper" "midi-looper.cl")
(defpackage :play (:use :cl :sc :mylisp :midi-looper))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)
(clock-bpm 60)
;
(def *midi-channel* 0)
;
(defun light-pad (note color)
  (midihelper:send-event (midihelper:ev-noteon *midi-channel* note color)))
;
(defstruct padspec
  synth
  toggle
  off-color
  on-color
  (amp-sensitive nil)
  instance)
;
(defun toggle-ctl (n ctl)
  (if (> (cbus-get ctl) 0)
      (progn (writeln 't) (cbus-set ctl 0) (light-pad n *fn-off-color*))
      (progn (writeln 'f) (cbus-set ctl 1) (light-pad n *fn-on-color*))))

(defsynth ssin ((freq 440) (freq0 440) (slide 0) (out 0) (amp 0.5) (gate 1)
                        (a 0.01) (r 0.4))
  (-<> (x-line.kr freq0 freq slide)
       (sin-osc.ar)
       (* amp (env-gen.kr (perc a r) :act :free :gate gate))
       pan2.ar (out.ar out <>)))
(def ssin-gen (gen-list [0 1 2 3 4 5 6 7 8 9 10 9 8 7 6 5 4 3 2 1]))

(proxy :mixer
       (-<> (abus-in :master)
            (if~ (cbus-in :ctl-reverb) (freeverb.ar <> :mix 0.2 :room 0.6) <>)
            (if~ (cbus-in :ctl-hpf) (hpf.ar <> 2000) <>)
            (if~ (cbus-in :ctl-lpf) (lpf.ar <> 200) <>)
            (if~ (cbus-in :ctl-bpf) (bpf.ar <> (range (sin-osc.kr 0.4) 100 7000)) <>)
            ) :pos :tail)

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
  *fn-off-color*     5
  )
(labels ((s (synth &optional (toggle :gate) (off-color 0) (on-color *active-color*) (amp-sensitive nil))
           (make-padspec :synth synth :toggle toggle :off-color off-color :on-color on-color :amp-sensitive amp-sensitive))
         (dl (buff &optional (toggle :gate) (off-color *drumloops-color*) (on-color *active-color*))
           (s ['sample-loop :out (abus :master) :buffer (cbuf buff) :attack 0.001 :release 0.01 :end-loop 84662]
              toggle off-color on-color))
         (fx (buff &optional (rate 1) (toggle :once) (off-color *fx-color*) (on-color *active-color*))
           (s ['sample-dur :out (abus :master) :buffer (cbuf buff) :attack 0.1 :release 3 :amp 0.9 :rate rate]
              toggle off-color on-color))
         (aa (buff &optional (rate 1) (toggle :once) (off-color *fx-color*) (on-color *active-color*))
           (s ['sample-dur :out (abus :master) :buffer (cbuf buff) :dur 5 :attack 0.001 :release 3 :amp 0.9 :rate rate]
              toggle off-color on-color t))
         (os (buff &optional (rate 1) (toggle :gate) (off-color *oneshot-color*) (on-color *active-color*))
           (s ['sample :out (abus :master) :buffer (cbuf buff) :rate rate :attack 0.001 :release 1]
              toggle off-color on-color t))
         (fn (func &optional (off-color 0) (on-color *active-color*))
           (s func :function off-color on-color)))
  (def *pad-map*
    (hshm
      64 (dl "/home/aerdman/Mus/samples/selection2/drums/28b-prc03-125.wav")
      60 (dl "/home/aerdman/Mus/samples/selection2/drums/28b-prc07-125.wav")
      56 (dl "/home/aerdman/Mus/samples/selection2/drums/28c-prc33-125.wav")
      52 (dl "/home/aerdman/Mus/samples/selection2/drums/28k-drm04-125.wav")
      48 (dl "/home/aerdman/Mus/samples/selection2/drums/28r-drm01-125.wav")
      44 (dl "/home/aerdman/Mus/samples/selection2/drums/28r-drm02-125.wav")
      40 (dl "/home/aerdman/Mus/samples/selection2/drums/28r-drm06-125.wav")
      36 (dl "/home/aerdman/Mus/samples/selection2/drums/28r-drm08-125.wav")
      ;
      65 (fx "/home/aerdman/Mus/samples/selection2/Crmson_5th_C.wav")
      61 (fx "/home/aerdman/Mus/samples/selection2/Cy_X_Fadez.wav")
      57 (fx "/home/aerdman/Mus/samples/selection2/Moon_Cycl.wav")
      53 (fx "/home/aerdman/Mus/samples/selection2/Str_Wv_Swp_C.wav")
      49 (fx "/home/aerdman/Mus/samples/selection2/Transformation2.wav")
      45 (fx "/home/aerdman/Mus/samples/selection2/Vctr_Trance.wav")
      41 (s ['sample :buffer (cbuf "/home/aerdman/Mus/samples/selection2/WavePoint-VHSTapeHiss-Loop.wav") :attack 3 :release 3] :gate *noise-color*)
      37 (s ['sample :buffer (cbuf "/home/aerdman/Mus/samples/selection2/WavePoint-VinylCrackle-Loop.wav") :attack 3 :release 3] :gate *noise-color*)
      ;
      66 (fx "/home/aerdman/Mus/samples/selection2/Crmson_5th_C.wav" 4/3)
      62 (fx "/home/aerdman/Mus/samples/selection2/Cy_X_Fadez.wav" 4/3)
      58 (fx "/home/aerdman/Mus/samples/selection2/Moon_Cycl.wav" 4/3)
      54 (fx "/home/aerdman/Mus/samples/selection2/Str_Wv_Swp_C.wav" 4/3)
      50 (fx "/home/aerdman/Mus/samples/selection2/Transformation2.wav" 4/3)
      46 (fx "/home/aerdman/Mus/samples/selection2/Vctr_Trance.wav" 4/3)
      42 (s ['sample :rate 4/3 :buffer (cbuf "/home/aerdman/Mus/samples/selection2/WavePoint-VHSTapeHiss-Loop.wav") :attack 3 :release 3] :gate *noise-color*)
      38 (s ['sample :rate 4/3 :buffer (cbuf "/home/aerdman/Mus/samples/selection2/WavePoint-VinylCrackle-Loop.wav") :attack 3 :release 3] :gate *noise-color*)
      ;
      67 (fx "/home/aerdman/Mus/samples/selection2/Crmson_5th_C.wav" 5/3)
      63 (fx "/home/aerdman/Mus/samples/selection2/Cy_X_Fadez.wav" 5/3)
      59 (fx "/home/aerdman/Mus/samples/selection2/Moon_Cycl.wav" 5/3)
      55 (fx "/home/aerdman/Mus/samples/selection2/Str_Wv_Swp_C.wav" 5/3)
      51 (fx "/home/aerdman/Mus/samples/selection2/Transformation2.wav" 5/3)
      47 (fx "/home/aerdman/Mus/samples/selection2/Vctr_Trance.wav" 5/3)
      43 (s ['sample :rate 5/3 :buffer (cbuf "/home/aerdman/Mus/samples/selection2/WavePoint-VHSTapeHiss-Loop.wav") :attack 3 :release 3] :gate *noise-color*)
      39 (s ['sample :rate 5/3 :buffer (cbuf "/home/aerdman/Mus/samples/selection2/WavePoint-VinylCrackle-Loop.wav") :attack 3 :release 3] :gate *noise-color*)
      ;
      96 (os "/home/aerdman/Mus/samples/selection2/SweepingEP.wav" (* 1/2 9/8))
      92 (os "/home/aerdman/Mus/samples/selection2/SweepingEP.wav" (* 1/2 4/3))
      88 (os "/home/aerdman/Mus/samples/selection2/SweepingEP.wav" (* 1/2 3/2))
      84 (os "/home/aerdman/Mus/samples/selection2/SweepingEP.wav" (* 1/2 5/3))
      80 (os "/home/aerdman/Mus/samples/selection2/SweepingEP.wav" 1)
      76 (os "/home/aerdman/Mus/samples/selection2/SweepingEP.wav" 9/8)
      72 (os "/home/aerdman/Mus/samples/selection2/SweepingEP.wav" 4/3)
      68 (os "/home/aerdman/Mus/samples/selection2/SweepingEP.wav" 3/2)
      ;;
      99 (fn (λ(pad note velo) (when (> velo 0) (toggle-ctl 99 :ctl-reverb))))
      95 (fn (λ(pad note velo) (when (> velo 0) (toggle-ctl 95 :ctl-hpf))))
      91 (fn (λ(pad note velo) (when (> velo 0) (toggle-ctl 91 :ctl-lpf))))
      87 (fn (λ(pad note velo) (when (> velo 0) (toggle-ctl 87 :ctl-bpf))))
      ;
      97 (aa "/home/aerdman/Mus/samples/hang-drum/hangA3.28.wav" 1)
      93 (aa "/home/aerdman/Mus/samples/hang-drum/hangA3.28.wav" 1.189)
      89 (aa "/home/aerdman/Mus/samples/hang-drum/hangA3.28.wav" 1.335)
      85 (aa "/home/aerdman/Mus/samples/hang-drum/hangA3.28.wav" 1.498)
      81 (aa "/home/aerdman/Mus/samples/hang-drum/hangA3.28.wav" 1.782)
      77 (aa "/home/aerdman/Mus/samples/hang-drum/hangA3.28.wav" 2)
      73 (aa "/home/aerdman/Mus/samples/hang-drum/hangA3.28.wav" 2.378)
      69 (aa "/home/aerdman/Mus/samples/hang-drum/hangA3.28.wav" 2.670)
      ;
      98 (aa "/home/aerdman/Mus/samples/hang-drum/hangA3.28.wav" 1)
      94 (aa "/home/aerdman/Mus/samples/hang-drum/hangA3.28.wav" 1.189)
      90 (aa "/home/aerdman/Mus/samples/hang-drum/hangA3.28.wav" 1.335)
      86 (aa "/home/aerdman/Mus/samples/hang-drum/hangA3.28.wav" 1.498)
      82 (aa "/home/aerdman/Mus/samples/hang-drum/hangA3.28.wav" 1.782)
      78 (aa "/home/aerdman/Mus/samples/hang-drum/hangA3.28.wav" 2)
      74 (aa "/home/aerdman/Mus/samples/hang-drum/hangA3.28.wav" 2.378)
      70 (aa "/home/aerdman/Mus/samples/hang-drum/hangA3.28.wav" 2.670)
      ;
      83 (fn (λ(pad note velo)
               (if (> velo 0)
                   (spawn-named 83
                     (funcall ssin-gen 0)
                     (light-pad note *active-color*)
                     (loop :do
                           (synth 'ssin :out (abus :master) :freq (midicps (+ 50 (sc *pentatonic* (funcall ssin-gen)))))
                           (sleep 0.1)))
                   (spawn-named 83
                     (light-pad note 20))))))))

;; MIDI mappings

(defun midi-map (messages)
  (dolist (message messages)
    (when-let ((msg (alsa-to-my-midi message)))
      (let ((note (mr-midi-event-note msg))
            (velo (mr-midi-event-velocity msg))
            (chan (mr-midi-event-channel msg)))
        (case (mr-midi-event-type msg)
          (:note_on
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
                      (setf (padspec-instance padspec)
                            (csnt note (apply #'synth
                                              (if (padspec-amp-sensitive padspec)
                                                  (append (padspec-synth padspec) [:amp (/ velo 127)])
                                                  (padspec-synth padspec))))))
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
          (:note_off)
          (:control))
        (writeln msg)
        ))))

(midihelper:stop-midihelper)
(midihelper:start-midihelper :master 96 'midi-map)

(midihelper:send-event (midihelper:ev-noteoff 0 36 0))

(stop)
