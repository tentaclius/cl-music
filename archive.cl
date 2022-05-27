;; TODO
;; defun or defmacro to initiate a synthesizer and create control buffers to its parameters.

;; -----------------------------------------------------------------------------------------------------
;; Load SC
;; {{{

(load "~/src/mus/lib.cl")
(in-package :sc-user)
(ql:quickload :ltk)
(use-package :sc-extensions)
(use-package :bdef)
(named-readtables:in-readtable :sc)
(init)   ;; start new server
(bpm 60)

;(connect)   ;; connect to a server already running

;; }}}

;; stop every instance
(mapcar #'server-quit (all-running-servers))

(all-running-servers)

(server-query-all-nodes)
(group-free-all)
(stop 1)

;; -----------------------------------------------------------------------------------------------------

(load-buf bd-snt "~/Mus/samples/bd.wav" 1)
(load-buf hh-snt "~/Mus/samples/hh.wav" 1)
(load-buf sbd-snt "~/Mus/samples/s-bd.wav" 1)

(synth 'bd-snt)
(synth 'hh-snt)
(synth 'hh)
(synth 'sbd-snt :rate 0)

;; ---

(defparameter *f* (bus-control))
(defparameter *o* (bus-audio))
(defparameter *g* (bus-control))
(proxy :saw (-<> 440 (saw.ar) (bpf.ar (in.kr *f*) 0.32)
                 (* (env-gen.kr (adsr 0.1 2 .6 .4) :gate (in.kr *g*)))
                 (pan2.ar) (out.ar *o* <>)))
(proxy :ctl (-<> (sin-osc.kr 1/2) (range -1000 2000) (+ 1600) (out.kr *f* <>)))
(proxy :gat (-<> (pulse.kr 5) (out.kr *g* <>)))

(proxy :gat nil)
(cbus-set *g* 1)

(proxy :wrt (-<> (in.ar *o*)
                 (+ (* 0.4 <>) (* 0.2 (Delay-n.ar <> 2)))
                 (pan2.ar) (out.ar 0 <>))
       :pos :tail)

(group-free-all)

;; ---

;; temporal recursion example
(defun dur (snt bt tm)
  (let ((s (at-beat bt (apply #'synth snt))))
    (at-beat (+ bt tm) (release s))))
(defun temprec (beat i)
  (at-beat beat (synth 'hh))
  (dur ['ssin :freq 440] beat 1/2)
  (clock-add (1+ beat) #'temprec (1+ beat) (1+ i)))

;;;;

(defparameter *root-note* 50)
(defparameter playing [])
(defparameter root-note-opt [50 53 55 60])
(defparameter sbus (bus-audio))

(defparameter chord-progression
  (make-generator () 
                  (loop :do (dolist (i [[0 2 4 6] [0 2 4 7] [0 2 4 9] [0 2 4]])
                              (yield i)))))

(defsynth sbus-ctl ((amp 1))
  (out.ar 0 (* amp (Delay-n.ar (in.ar sbus 2) 0.25 0.25))))
(defparameter sbus-c (synth 'sbus-ctl :amp 0))

(ctrl sbus-c :amp 0.7)

(proxy :noise (-> (brown-noise.ar) (* 0.2) (* (pulse.kr 1/4) (pulse.kr (/ 1.1 3))) (pan2.ar)))
(proxy :noise nil)

(mapcar (λ(i) (push (synth 'organ :out sbus :amp 0.4 :freq (midicps (+ *root-note* (sc *major* i)))) playing))
        (next chord-progression))

(slide sbus-c :amp 0 0.3 10)

; ---

(defparameter *bus* (bus-audio :chanls 2))

(proxy :echo 
       (with-controls ((freq 440))
         (-<> (in.ar *bus* 2)
              (freeverb.ar :room 0.7 :mix 0.5)
              (bpf.ar freq)
              (out.ar 0 <>)))
       :pos :tail)

(defparameter *s1* (synth 'puls :gate 0 :freq (midicps 30) :amp 0.15 :out *bus*))

(ctrl *s1* :amp 0.3)

(mseq (clock-quant 1) [['msin :dur 1/8] 'msin])

(defmetro metro1
  (when (zerop (mod (i) 4)) (slide :echo :freq 200 4000 4))
  (mseq (beat)
        [[:ctrl *s1* :gate 1 :freq (midicps 30) :dur 1/7]
         [:ctrl *s1* :gate 1 :freq (midicps 30) :dur 1/8]
         (λ() (ctrl *s1* :gate 1) (slide *s1* :freq (midicps 20) (midicps 30) 1/8))
         [:ctrl *s1* :gate 1 :freq (midicps 30) :dur 1/8]
         [:ctrl *s1* :gate 1 :freq (midicps 32) :dur 1/7]
         [:ctrl *s1* :gate 1 :freq (midicps 30) :dur 1/8]
         (λ() (ctrl *s1* :gate 1) (slide *s1* :freq (midicps 30) (midicps 40) 1/8))
         ])
  (msq 'bd '- 'bd (if (= (mod (i) 2) 0) (ssq 'hh 'bd)))
  )

(metro1 (clock-quant 1) 0)

;;---

(defparameter *g1* (bus-audio :chanls 2))
(defparameter *g1-amp* (bus-control))

(proxy :g1-stream
       (-<> (in.ar *g1* 2)
            (* 0.4 (in.kr *g1-amp*))
            (out.ar 0 <>)
            ) :pos :tail)

(bus-set *g1-amp* 1)

(proxy :noise1 (-<> (pink-noise.ar) pan2.ar (out.ar *g1* <>)))

(defmetro m1
  (msq :hh)
  (bsq
    (proxy :ng (out.kr *g1-amp* (pulse.kr 3)))
    (proxy :ng (out.kr *g1-amp* (pulse.kr 6)))
    (proxy :ng (out.kr *g1-amp* (* (pulse.kr 3) (pulse.kr 20))))
    (proxy :ng (out.kr *g1-amp* (* (pulse.kr 6) (pulse.kr 20))))
  ))

(m1 (clock-quant 1) 0)

(defmetro m1 nil)
(proxy :ng nil)

;;---

(proxy :sin 
       (with-controls ((gate 0) (note 50) (amp 1))
         (let* ((env (env-gen.kr (adsr 0.1 0.3 0.7 0.5) :gate gate))
                (sig (sin-osc.ar (midicps note))))
           (out.ar 0 (pan2.ar sig 0 (* env amp))))))

(proxy :sin nil)

(ctrl :sin :gate 1 :amp 0.7 :note 60)

(release :sin)

(free :sin)

;;---

(defparameter *g1* (bus-audio :chanls 2))

(synth 'organ :out *g1*)

(proxy :g1
       (-<> (in.ar *g1*)
            (bpf.ar (range (sin-osc.kr 1/14) 200 3000))
            (out.ar 0 <>)
            )
       :pos :tail)

;;---

(defparameter *sin2*
  (proxy :sin2 
         (with-controls ((out 0) (freq 220) (gate 1) (amp 0))
           (let* ((base-f (+ freq (range (sin-osc.kr 4) -2 2)))
                  (sig    (* (sin-osc.ar base-f) 0.4))
                  (sig    (+ sig (* (sin-osc.ar (* 1.5 freq)) 0.3)))
                  (sig    (+ sig (* (sin-osc.ar (+ (* 2 freq)) (range (sin-osc.kr 1 1) -1 1)) 0.1)))
                  (env    (env-gen.kr (adsr 0.1 0.3 0.7 0.5) :gate gate)))
             (out.ar out (pan2.ar sig 0 (* env amp)))))))

(slide *sin2* :amp 0 0.3 10)
(ctrl *sin2* :amp 0.3)

(defparameter *saw2*
  (proxy :saw2
         (with-controls ((out 0) (freq 220) (gate 1) (amp 0))
           (%> (+ freq (range (sin-osc.kr 4) -2 2))
               (saw.ar %)
               (+ % (* (saw.ar (* freq 1.5)) 0.3))
               (pan2.ar % 0 (* (env-gen.kr (adsr 50 0 1 50) :gate gate) amp))
               (out.ar out %)))))

(slide *saw2* :amp 0 0.2 10)

(proxy :sin (synth 'sin2 :freq (* 220 1)))

(proxy :saw (synth 'saw2 :freq (* 1.5 110) :amp 0.08))

(defpattern bass +inf+
  ;(λ(b d e)
  ;  (when e
  ;    (let ((s (synth 'msin :amp 0.7 :freq (midicps (+ (sc *major* e) 31 )))))
  ;      (at-beat (+ b d -1/8) (release s)))))
  (play-note 'msin)
  (per-beat
    [54 54 56 [31 :amp 0.1]]))

(bass :stop)

(defpattern drum +inf+
  (play-drum)
  (per-beat
    [ [:sim 'bd ['sbd :amp 0.2]] nil 'bd nil]
    [ 'bd nil 'bd [:seq ['sbd :amp 0.1] 'bd]]
  ))

(drum :start)

(defpattern hh +inf+
  (play-drum :amp 0.2)
  (per-beat ['hh 'hh 'hh 'hh 'hh 'hh 'hh 'hh]))

(hh :start)

(defpattern chords +inf+
  (λ(b d e) (when e (let ((s (synth 'msin :amp 0.10 :freq (midicps (+ 62 (sc *major* e))))))
                      (at-beat (+ b d 1) (release s)))))
  (per-beat
    [[:sim 1 5] nil]
    []
    [nil [:sim 3 7] nil nil]
    [])
  )

(chords :stop)

(defparameter sw1
  (proxy :saw1
         (with-controls ((gate 1) (freq 660) (amp 0.1))
           (%> (saw.ar freq)
               (* % amp)
               (pan2.ar % 0 (env-gen.kr (adsr 1 0 1 50) :gate gate))
               (out.ar 0 %)))))

(xslide sw1 :freq 440 660 2)
(slide sw1 :amp 0.2 0 10)

(stop)

;;---

(defsynth testdrum ((amp 0.4))
          (%> (x-line.kr 400 1 1/8)
              (sin-osc.ar %)
              (* % (env-gen.kr (perc 0.01 1/2 3) :act :free))
              ;(+ % (* 0.3 (pink-noise.ar) (line.kr 1 0 1/4)))
              (pan2.ar % 0 amp)
              (out.ar 0 %)))

(defparameter gated-noise (proxy :gated-noise
   (with-controls ((freq 1) (width 1/2))
     (%> (pink-noise.ar)
         (* % (pulse.kr freq width))
         (pan2.ar % 0 0.3)
         (out.ar 0 %)
         ))))

(defparameter sharp-saw (proxy :saw
   (with-controls ((freq 880) (gt 1) (amp 0))
     (%> (* 0.3 (saw.ar freq))
         (+ % (* 0.3 (saw.ar (+ freq 2))))
         (* % (env-gen.kr (adsr 0.2 0.4 0.7 0.8) :gate gt))
         (pan2.ar % 0.1 amp)
         ))))

(defparameter *rev-bus* (bus-audio))

(proxy :rev-bus
       (let* ((sig0 (in.ar *rev-bus*))
              (dl1 (* (delay-l.ar sig0 0.1 0.1) 0.03))
              (dl2 (* (delay-l.ar sig0 0.2 0.2) 0.01))
              (sig (+ sig0 dl1 dl2))
              (sigf1 (hpf.ar sig 1090))
              (sigf2 (lpf.ar sig 40))
              (sig (+ sigf1 sigf2))
              )
         (out.ar 0 (pan2.ar sig 0 1)))
       :pos :tail)

(slide sharp-saw :amp 0.001 0.2 10)

(ctrl :saw :freq 880 :amp 0.08)

(proxy :xxx (%> (saw.ar 30) (pan2.ar % (sin-osc.kr 8) 1/8)))

(proxy :sin (%> (sin-osc.ar 40) (pan2.ar % 0 1/2)))

(free :xxx)

(ctrl :gated-noise :freq 8 :width 8/10)

(defpattern noise-freq-ctl +inf+
  (λ(b d e) (ctrl :gated-noise :freq (+ e (random 4)) :width 8/10))
  (per-beat
    [3] [5] [9] [13]))

(noise-freq-ctl :start)

(defpattern drums :inf
  (play-drum :amp 0.3)
  (λ(i) (sim nil
             (per-beat i
                       (seq 'testdrum 'testdrum)
                       (seq 'testdrum 'testdrum)
                       (seq 'testdrum 'testdrum)
                       (seq 'testdrum nil nil nil 'testdrum nil nil 'testdrum)))))

(drums :start)

(free :gated-noise)

(defpattern saw-freq-ctl +inf+
  (λ(b d e) (ctrl :saw :freq (+ 110 (* 110 (random 10) ))))
  (per-beat [1]))

(saw-freq-ctl :start)

(saw-freq-ctl :stop)
(release :saw)

(stop)

(defparameter *amp-ctl* (bus-control))

(control-set (busnum *amp-ctl*) 0.1)

(proxy :amp
       (with-controls ((gate 0))
         (%> (env-gen.kr (perc 0.1 1/2 0.3) :gate gate)
             (out.kr *amp-ctl* %)
             ))
       :pos :tail)

(ctrl :amp :gate 1)
(release :amp)

(defpattern pat1 +inf+
  (λ(b d e) (when e (ctrl :amp :gate 1) (at-beat (+ b d -1/8) (release :amp))))
  (per-beat [t t t t nil]))

(pat1 :start)

(drums :start)

(proxy :sqr 
       (%> (* (saw.ar 220) 0.3)
           (+ % (* (pulse.ar 330) 0.3))
           (bpf.ar % (range (f-sin-osc.kr 1/2) 100 2100))
           (* % 0.4)
           (pan2.ar % 0 (in.kr *amp-ctl*))
           ))


(proxy :s (%> (sin-osc.ar 30) (+ % (sin-osc.ar 32)) (* % 0.2) (pan2.ar % (sin-osc.kr 1))))

(release :s)

(proxy :b
       (%> (sin-osc.ar (x-line.kr 400 1 1/3))
           (* % (env-gen.kr (perc 0.0 1 1)))
           (+ % (* (pink-noise.ar) (env-gen.kr (perc 0 1/3 1/5))))
           (pan2.ar %)
           ))

;;---

(defparameter *bus* (bus-audio :chanls 2))

(proxy :sqr
       (with-controls ((amp 0.6) (freq 220) (gt 0) (pf 20))
         (%> (saw.ar freq)
             (* % (env-gen.kr (perc 0.01 1/3 0.8) :gate gt))
             (pan2.ar % 0 amp)
             (lpf.ar % (range (sin-osc.kr 1/8) 100 2000))
             )))

(ctrl :sqr :pf 1900)

(ctrl :sqr :gt 0) (sleep 0.1) (ctrl :sqr :gt 1 :freq 110 :amp 0.4)

(ctrl :sqr :amp 0.6)

(defpattern sqr-pat +inf+
  (λ(b d e) (ctrl :sqr :gt 1 :freq (midicps e))
            (at-beat (+ b d -1/16) (ctrl :sqr :gt 0)))
  (per-beat
    [30 31 32 33 34 35 36 37]
    [30 33 35 40]
    [30 31 32 33 34 35 37 42]
    [30 33 35 45]))

(defpattern sqr-pat +inf+
  (λ(b d e) (ctrl :sqr :gt 1 :freq (midicps e))
            (at-beat (+ b d -1/16) (ctrl :sqr :gt 0)))
  (per-beat
    [30 32 34 37]
    [30 32 34 40]
    [30 32 34 37]
    [30 32 34 45]))

(sqr-pat :start)

(proxy :drone
       (with-controls ((freq (midicps (+ 54 12))) (amp .3))
         (%> (* 0.3 (sin-osc.ar freq))
             (+ % (* 0.3 (sin-osc.ar (+ 2 freq))))
             (* % amp)
             (pan2.ar %)
             )))

(proxy :drone nil)

(defpattern drum +inf+
  (play-drum)
  (per-beat
    ['testdrum [:sim 'testdrum 'hh]]))

(drum :start)

(defsynth snare ((out 0) (amp 1))
  (let* ((nois (bpf.ar (* 0.9 (white-noise.ar)) 11000))
         (sn   (sin-osc.ar (x-line.kr 500 1 1/4)))
         (env  (env-gen.kr (perc 0 1/3 amp) :act :free))
         (sig  (pan2.ar (+ sn (* 0.3 nois)) 0 env)))
    (out.ar out sig)))

(synth 'testdrum) (synth 'hh)


(proxy :testdrum 
          (%> (x-line.kr 400 10 1/8)
              (pulse.ar %)
              (* % (env-gen.kr (perc 0.01 1/2 3) :act :free))
              ;(+ % (* 0.3 (pink-noise.ar) (line.kr 1 0 1/4)))
              (pan2.ar % 0 0.7)
              (out.ar 0 %)))

(defparameter *s1* (synth 'msin))

(ctrl *s1* :freq 440)
(ctrl *s1* :amp 0.7)

(defparameter *s1* (proxy :s1 (with-controls ((amp 0.7) (freq 440))
                            (sin-osc.ar freq 0 amp))))

(defparameter w (wobble *s1* :freq 4 420 460 1))

(release w)

;;---

(defparameter *e-bus* (bus-audio :chanls 2))
(defparameter *s1-prev* 440)

(defparameter *s1* (proxy :s1
       (with-controls ((freq 440) (amp 0.7) (gt 0))
         (-<> (saw.ar freq)
              (pan2.ar 0 amp)
              (* (env-gen.kr (perc 0.001 0.7) :gate gt))
              (out.ar *e-bus* <>)
              ))))

(defparameter *s2* (proxy :s2
       (with-controls ((freq 440) (amp 0.7) (gt 0))
         (-<> (saw.ar freq)
              (pan2.ar 0 amp)
              (* (env-gen.kr (perc 0.001 0.7) :gate gt))
              (out.ar *e-bus* <>)
              ))))

(proxy :bus
       (-> (in.ar *e-bus*)
           (freeverb.ar :mix .7 :room 0.6)
           (bpf.ar (-> (sin-osc.kr 1/15) (range 120 1100)))
           )
       :pos :tail)

(proxy :wah
       (-> (sin-osc.ar (midicps (+ 36 (* 12 2))))
           (+ (sin-osc.ar (* (midicps (+ 36 24)) 1.5)))
           (+ (* 0.1 (sin-osc.ar (* (midicps (+ 36 36)) 1.5))))
           (* 0.5)
           (pan2.ar 0 0.1)
           ;(freeverb.ar :mix .7 :room 0.6)
           ))

(defpattern p1 +inf+
  (λ(b d e)
    (let* ((n (midicps (+ 36 (sc *pentatonic* e)))))
      (ctrl *s1* :gt 1 :freq n)
      ;(xslide *s1* :freq *s1-prev* n 1/7)
      (at-beat (+ b d -1/10)
               (ctrl :s1 :gt 0))
      (setf *s1-prev* n)))
  (per-beat
    [-1 0 1 2]
    [ 1 0 3 4]
    [-1 0 1 3]
    [ 5 1 3 0]
    [-1 0 1 2]
    [ 1 0 3 4]
    [ 7 3 2 8]
    [ 1 0 3 4]
    ))

(bpm 100)

(defpattern dr +inf+
  (play-drum :amp 0.5)
  (per-beat
    ['testdrum nil nil nil  'hh nil '(hh :amp 0.15) nil]
    ['testdrum nil nil nil  'hh nil nil nil]
    ['testdrum nil nil nil  'hh nil nil nil]
    ['testdrum nil nil nil  'hh nil '(testdrum :amp 0.1) nil]
    ))

(dr :stop)

(p1 :start)

(bdef :baa "~/Mus/dirt-samples/baa2/1.wav")

(bdef :baa)

(defparameter b1 (buffer-read "~/Mus/dirt-samples/baa2/1.wav"))

(synth :sample-1 :buffer b1 :start 0 :rate 1 :dur 1)

(defsynth bzz ((freq 110) (out 0))
  (-<> (sin-osc.ar (x-line.kr freq 30 1/8) 0 0.40)
       (+ (-> (gray-noise.ar)
              (* (env-gen.kr (perc 0.2 0.3 0.03)))
              (bpf.ar (* freq 4) 0.2)))
       (+ (-> (sin-osc.ar freq)
              (* (env-gen.kr (perc 0.001 0.4 0.5)))))
       (* (env-gen.kr (linen 0.001 0.3 0.3) :act :free))
       pan2.ar
       (out.ar out <>)
       ))

(synth 'bzz :freq 100)

(server-query-all-nodes)

;; Buffer positioning u-gens:
;; - BufRd -- the reader
;; - Line.ar -- address a phase linearly
;; - Phasor.ar -- loops from start to end

(defpattern z1 +inf+
  (λ(b d e) (unless (zerop e) (synth 'bzz :freq (->> (random 12) (sc *pentatonic*) (+ 50) midicps))))
  (per-beat
    [1 1 1 1]
    ))

(z1 :start)

;;---

(defparameter *foo*
   (proxy :foo
      (with-controls ((freq (midicps 30)) (amp 0) (gt 1))
        (-> (* 0.3 (sin-osc.ar freq))
            (+ (* 0.2 (sin-osc.ar (* freq 3))))
            (+ (* 0.06 (sin-osc.ar (+ 1 (* freq 3)))))
            (+ (* 0.02 (saw.ar (* 4 freq))))
            (* (env-gen.kr (adsr 0.1 0.3 0.7 2) :gate gt))
            (* amp)
            pan2.ar
          ))))

(slide *foo* :amp 0 1 40)

(defparameter *s1*
  (proxy :s1
    (with-controls ((freq (midicps 30)) (amp 1) (gt 1))
      (-> (* 0.4 (sin-osc.ar freq))
          (+ (* 0.3 (sin-osc.ar (* freq 3))))
          (* (env-gen.kr (adsr 0.1 0.3 0.7 2) :gate gt))
          (* amp)
          pan2.ar
          ))))

(ctrl :s1 :freq (midicps 30))

(bpm 50)

(p1 :stop)

(defsynth bt ((freq 200) (amp 0.6))
  (-<> (x-line.kr freq 40 0.1)
       sin-osc.ar
       (* amp)
       (* (env-gen.kr (perc 0.01 1) :act :free))
       (freeverb.ar :room 0.3 :mix 0.3)
       pan2.ar (out.ar 0 <>)
       ))

(defsynth ph ((out 0))
  (-<> (pink-noise.ar)
       (* (env-gen.kr (perc 0.01 0.4) :act :free))
       (bpf.ar (x-line.kr 1100 200 0.04) 0.14)
       (* 1.9)
       pan2.ar (out.ar 0 <>)
       ))

(defsynth ph1 ((out 0))
  (-<> (pink-noise.ar)
       (* (env-gen.kr (perc 0.01 0.4) :act :free))
       (bpf.ar 2200 0.1)
       (* 1.9)
       pan2.ar (out.ar 0 <>)
       ))

(synth 'ph1)

(defpattern dr +inf+
  (play-drum)
  (per-beat
    ;['(bt :freq 300) [:seq '(bt :freq 200 :amp 0.3) '(bt :freq 170 :amp 0.3)] '(bt :freq 100 :amp 0.2)]
    ;['(bt :freq 300) '(bt :freq 200 :amp 0.4)]
    ;['bt nil nil nil]
    ;[nil 'bt 'bt 'bt nil nil]
    ['bt 'ph nil 'bt]
    [nil 'ph1 nil nil]
    ))

(dr :start)
(dr :stop)

(defparameter *zz* (proxy :zz (with-controls ((freq (midicps 66)) (amp 0.1))
  (-> (* 0.4 (sin-osc.ar freq))
      (+ (* 0.2 (sin-osc.ar (+ freq (range (sin-osc.kr 0.1) 0 3)))))
      (* (env-gen.kr (adsr 0.1 0.3 0.7 2)))
      (pan2.ar 0 amp)
      ))))

(slide *zz* :amp 0 0.2 40)

(defparameter *nz* (proxy :nz (with-controls ((freq 1) (amp 0.071))
  (-> (pink-noise.ar)
      (* amp)
      (* (range (pulse.kr freq) 0 1))
      (hpf.ar (range (sin-osc.kr 0.2 0) 260 6000))
      pan2.ar
      ))))

(defpattern nzm +inf+
  (λ(b d e) (when e (ctrl :nz :freq e)))
  (per-beat
    [1]
    [1]
    [4]
    [4]
    [9]
    [16]
    ))

(nzm :start)

(defsynth pads ((freq 440) (amp 0.4) (gate 1))
  (-<> (sin-n 8 :freq-fn (λ(i) (* freq i)))
      (* (env-gen.kr (adsr 0.3 0.3 0.7 1) :act :free :gate gate))
      (pan2.ar 0 amp)
      (out.ar 0 <>)
      ))

(defpattern pads +inf+
  (λ(b d e) (when e (let ((s (synth 'pads :freq (midicps (+ 30 12 e)) :amp 0.02)))
                      (at-beat (+ b d) (release s)))))
  (per-beat
    [14 [:seq nil nil 15] 19]
    [14 12 19]
    ;[30 34 36 38 41 43 46 48 50 53 55]
    ;(reverse [30 34 36 38 41 43 46 48 50 53 55])
    ))

(pads :start)

(proxy :zee (-> (x-line.kr 700 20 0.1) sin-osc.ar (* 0.9) (* (env-gen.kr (perc 0.01 0.5))) pan2.ar))

(defsynth bdr ((freq 200))
  (out.ar 0 (+
    (-> (x-line.kr freq 20 0.2)
        sin-osc.ar
        (* 0.9)
        (* (env-gen.kr (perc 0.01 0.5)))
        pan2.ar)
    (-> (pink-noise.ar)
        (lpf.ar 4000)
        (* (env-gen.kr (perc 0.01 0.1)))
        (pan2.ar)
        ))))

(synth 'bdr)

(bpm 120)

(defpattern dr1 +inf+
  (play-drum)
  (per-beat
    [['zee :freq (+ 40 (* 200 (+ 1 (sin (/ (i) 4) )) ))]]))

(dr1 :stop)

;;---

(defparameter *bus* (bus-audio :chanls 2))

(proxy :bus
       (-<> (in.ar *bus* 2)
            (bpf.ar (range (sin-osc.kr 1/7) 100 1100)
                    (range (sin-osc.kr 1/11) 0.1 0.6))
            (freeverb.ar :room 0.8 :mix 0.5)
            ) :pos :tail)

(defsynth zee ((freq 440) (amp 0.8) (out *bus*))
  (-<> (sin-n 4 :freq-fn (λ(i) (* freq i)))
       (* amp)
       (* (env-gen.kr (perc 0.002 0.18) :act :free))
       pan2.ar (out.ar out <>)
       ))

(defpattern zee +inf+
  (play-note 'zee :note-fn (λ(e) (midicps (+ 48 (sc *pentatonic* e)))) :release nil)
  (per-beat
    (sim ;(seq (reverse [-1 0 2 3]))
         (seq [-1 0 2 3])
         (seq (mapcar (λ(e) [e :amp 0.2]) [1 2 3 4  7 8 9 10]))
         ;(seq (loop :repeat 8 :collect 10))
         ;(seq (loop :repeat 8 :collect 5))
         )))

(zee :start)
(zee :stop)

(defsynth dr ((out 0) (amp 0.5) (freq 500))
  (-<> (x-line.kr freq 30 0.1)
       (sin-osc.ar)
       (+ (hpf.ar (* 0.2 (pink-noise.ar)) (* 2 freq)))
       (* amp)
       (* (env-gen.kr (perc 0.01 0.3) :act :free))
       (out.ar out <>)
       ))

(defsynth hh ((out 0) (amp 0.2) (att 0.01) (dur 0.2))
  (-<> (white-noise.ar)
       (bpf.ar 10000)
       (* (env-gen.kr (perc att dur) :act :free))
       (* amp)
       (out.ar out <>)
       ))

(defpattern dr +inf+
  (play-drum)
  (per-beat
    (sim (seq '(dr :amp 0.2 :freq 400) '(hh :dur 0.1) '(hh :dur 0.2) (seq nil '(dr :amp 0.09 :freq 300)))
         (seq (loop :repeat 8 :collect ['hh :amp 0.1 :dur 0.051])))
    ))

(dr :start)
(dr :stop)

(proxy :drone
       (-<> (sin-n 7 :freq-fn (λ(i) (* (midicps (- 48 (* 12 1))) i)))
            (bpf.ar (range (sin-osc.kr 0.07) 200 1100) (range (sin-osc.kr 0.01) 0.3 0.6))
            ))

(defpattern drone +inf+
  (λ(b d e) (when e 
              (let ((nn (float (+ 2 (random 12)))))
                (proxy :drone
                       (-<> (sin-n nn :freq-fn (λ(i) (* (midicps (- 48 (* 12 1))) i)))
                            (bpf.ar (range (sin-osc.kr 0.1) 200 1100) (range (sin-osc.kr 0.3) 0.3 0.6))
                            )))))
  (per-beat [1]))

(drone :start)

;;---

(defparameter s31 (buffer-read-channel "~/Mus/31seconds.wav" :channels 1))

(defparameter *b1* (bus-audio :chanls 2))

(proxy :p1
       (-<> (buf-rd.ar 1 s31
                       (* (range (sin-osc.ar 0.4) 0.1 0.6) (buf-frames.ir s31)))
            (+ <> (* 0.1 (slope.ar <>)))
            ;(+ <> (* 0.041 (ringz.ar <>)))
            ))

(proxy :p1 nil)

(proxy :b1
       (-<> (in.ar *b1* 1)
            (+ <> (* 0.04 (slope.ar <>)))
            (* 0.001)
            freeverb.ar
            ;(rlpf.ar (range (sin-osc.kr 0.1) 200 1000))
            (* (line.kr 0 1 40))
            pan2.ar) :pos :tail)

(proxy :b1 nil)

(synth 'sample :buffer s31 :rate 0.3)

(defpattern p1 +inf+
  (λ(b d e) (when e (funcall e)))
  (per-beat
    (sim (λ() 
           (proxy :samp
                  (-<> (buf-rd.ar 1 s31
                                  (* (range (sin-osc.ar 0.4) 0.2 0.7) (buf-frames.ir s31)))
                       (out.ar *b1* <>)
                       )))
         (λ() 
           (proxy :samp1
                  (-<> (buf-rd.ar 1 s31
                                  (* (line.ar 0.0 1 20 :act :free) (buf-frames.ir s31)))
                       (out.ar *b1* <>)
                       ))))
    [nil]
    ))

(p1 :start)
(p1 :stop)

(defpattern dr +inf+
  (play-drum :amp 1)
  (per-beat
    ['bd '(bd :amp 0.1) 'bd '(bd :amp 0.1)]))

(dr :stop)

;;---

;; TODO: randomization
;; - rand.ir min max
;; - t-rand.kr min max --> float
;; - i-rand.kr min max --> integer
;; - d-rand <list> --> item from the list

(defsynth dng ((freq 440) (amp 0.5) (out 0) (gate 1))
  (-<> (sin-osc.ar freq 0 amp)
       (+ (sin-osc.ar (+ freq (t-rand.kr -1 2)) 0 (* 0.4 amp)))
       (* (env-gen.kr (adsr 0.007 0.1 0.7 0.3) :act :free :gate gate))
       pan2.ar
       (out.ar out <>)
       ))

(defsynth bd ((freq 200) (amp 0.5))
  (out.ar 0 (+
    (-> (x-line.kr freq 30 0.2)
        sin-osc.ar
        (* 0.9 amp)
        (* (env-gen.kr (perc 0.0 0.5) :act :free))
        pan2.ar)
    ;(-> (pink-noise.ar)
    ;    (lpf.ar 4000)
    ;    (* (env-gen.kr (perc 0.01 0.1)))
    ;    (pan2.ar)
        )))

(defsynth sn ((out 0) (amp 0.7))
  (-<> (brown-noise.ar)
       (* (env-gen.kr (linen 0.01 0.06 0.2) :act :free))
       (* amp)
       (hpf.ar 4000)
       pan2.ar
       (out.ar 0 <>)
       ))

(synth 'sn)

(defpattern dng +inf+
  (play-note 'dng :attr [:amp 1/8 :dur 0.7])
  (per-beat (mapcar (λ(e) (sim [(- e 12) :amp (/ 1 (+ 2 (random 4)))] e))
                    [63 60 60 60 65 60]))
  (once-every 4 0 [72 60 67 70])
  (once-every 4 2 [60 63 65 67])
  (once-every 4 3 [70 67 63 60])
  )

(dng :start)
(dng :stop)

(defpattern drm +inf+
  (play-drum)
  (per-beat
    ;(seq ['bd :amp 0.7] ['bd :amp 0.4] ['bd :amp 0.3] ['bd :amp 0.2] ['bd :amp 0.1] ['bd :amp 0.05])
    (seq 'bd '- 'bd '-  '- (seq 'bd nil nil 'bd) '- '-)
    )
  ;(once-every 2 0 (seq ['sn :amp 0.3] nil))
  )

(drm :start)

;;---

(defpattern drm +inf+
  (play-drum)
  (per-beat
    (sim (seq '(bd :rate 1/4) '- '(bd :rate 1/4) '-)
         (seq '- 'hh '- 'hh)
         ))
  (once-every 1 0 (seq (loop :repeat 8 :collect `(hh :amp ,(/ 3 (+ 4 (random 5))) :rate 2))))
  )

(defsynth sbs ((freq (midicps 30)) (out 0) (amp 0.5) (gate 1))
       (-<> (sin-n 7 :freq-fn (λ(i) (* freq i)))
            (+ (* 0.1 (saw.ar (+ 1 freq))))
            (* (+ 0.1 (range (sin-osc.kr 3 0) 0 0.3)))
            (* amp (env-gen.kr (adsr 0.1 0.2 0.7 0.5) :gate gate :act :free))
            freeverb.ar
            ;(bpf.ar (range (sin-osc.kr 1/8 (* 3/2 pi)) 700 4000) 0.1)
            pan2.ar
            (out.ar out <>)
            ))

;;---

(defsynth sin ((freq 440) (out 0) (amp 0.5))
  (-<> (sin-osc.ar freq 0 0.5)
       (+ (sin-osc.ar (* freq 5) 0 0.03))
       (* (range (sin-osc.kr 10) 0.6 1))
       (* (env-gen.kr (perc 0.001 3) :act :free))
       (* amp)
       pan2.ar
       (out.ar out <>)))

(synth 'sin)

(defpattern din +inf+
  (play-note 'sin :release nil :note-fn (λ(e) (->> e (sc *pentatonic*) (+ 60) midicps)))
  (λ(i) (per-beat i (sim
                       (seq (- (random 7) 3) (- (random 5) 3))
                       (once-every i 2 0 (seq [0]))
                       (once-every i 4 1 (seq [-4]))
                       ))))

(defsynth hh ((amp 0.3) (out 0))
  (-<> (pink-noise.ar)
       (* (env-gen.kr (perc 0.001 0.2) :act :free))
       (* amp)
       pan2.ar (out.ar out <>)))

(defpattern dr +inf+
  (play-drum)
  (λ(i) (per-beat i (once-every i 2 0 (seq 'hh)))))

(dr :stop)

(din :stop)

(din :start)

;;---

(defsynth boo ((freq (midicps 50)) (out 0) (gate 1) (amp 0.3))
  (-<> (sin-osc.ar freq)
       (+ (* 0.05 (pink-noise.ar)))
       (* amp)
       (* (env-gen.kr (perc 0.01 0.4) :act :free :gate gate))
       pan2.ar
       (out.ar out <>)))

(synth 'boo :amp 1)

(defsynth bum ((out 0) (amp 0.7) (f1 400) (f2 20) (d 1/8))
  (-<> (x-line.kr f1 f2 d)
       sin-osc.ar
       (* amp)
       (* (env-gen.kr (perc 0.0 1) :act :free))
       pan2.ar
       (out.ar out <>)
       ))

(synth 'bum :amp 0.5 :f1 400 :d 1/10)

(defpattern pat :inf
  (play-note 'boo :release nil
             :note-fn (λ(e) (midicps (+ 50 (sc *pentatonic* e)) )))
  (λ(i) (sim nil
           (per-beat i
                     (seq -1 0 3 5)
                     )
           (per-beat i
                     (seq (random 10) 6 5 4)
                     (seq 10 () () (+ 10 (random 4)))
                     )
           )))

(pat :start)

(def b1 ['bum :f1 200 :d 1/6])
(def b2 ['bum :f1 500 :d 1/16 :amp 0.7])
(def b3 ['bum :f1 (midicps 74) :d 1/6])

(def bs (bus-control))

(proxy :bs (out.kr bs (range (sin-osc.kr 0.1) 400 10000)))

(proxy :bs nil)

(defsynth hh ((out 0) (amp 0.06) (d 0.18))
  (-<> (white-noise.ar)
       (lpf.ar (in.kr bs))
       (* (env-gen.kr (perc 0.001 d) :act :free))
       (* amp)
       pan2.ar
       (out.ar out <>)
       ))

(synth 'hh)

(defpattern bum :inf
  (play-drum)
  (λ(i) (sim '-
             (seq b2 nil b2 nil)
             (per-beat i
                       (seq nil nil (seq 'hh 'hh 'hh) nil nil nil)
                       (seq nil ['hh :d 1/2]))
           )))

(bum :start)

(def b4 ['bum :amp 0.3 :f1 700 :f2 40 :d 1/8])

(defpattern bm 1
  (play-drum)
  (λ(i) (seq b4 b4 b4)))
(bm :start)

(proxy :drone
       (with-controls ((drone-f (midicps 52)))
         (-<> (sin-osc.ar drone-f)
              (+ (* 0.4 (sin-osc.ar (* 1.5 drone-f))))
              (+ (* 0.041 (saw.ar (+ (range (sin-osc.kr 6) -8 14) (* 4 drone-f)))))
              (* 0.50)
              pan2.ar)))

(release :drone)

(bum :start)

;; Clipping
(proxy :test
       (clip2 (sin-osc.ar 440) 0.07))
(proxy :test
       (wrap (sin-osc.ar 440) (range (sin-osc.kr 1) 0.2 0.7)))

(proxy :test
       (-<> (sin-osc.ar (+ (range (sin-osc.kr 10) -10 10) 440))
            (* (range (saw.kr 90) 0.3 0.9))
            (decimator.ar 500 32)
            (* 0.1)
            pan2.ar))

(proxy :test nil)

;;======

(defsynth boo ((freq (midicps 50)) (out 0) (gate 1) (amp 0.3))
  (-<> (sin-osc.ar freq)
       (+ (* 0.05 (pink-noise.ar)))
       (* amp)
       (* (env-gen.kr (perc 0.01 0.4) :act :free :gate gate))
       pan2.ar
       (out.ar out <>)))

(defsynth bum ((out 0) (amp 0.7) (f1 200) (f2 20) (d 1/6))
  (-<> (x-line.kr f1 f2 d)
       sin-osc.ar
       (* amp)
       (* (env-gen.kr (perc 0.0 0.7) :act :free))
       pan2.ar
       (out.ar out <>)
       ))

(defsynth click ((out 0) (amp 0.4) (freq (midicps 74)))
  (-<> (sin-osc.ar freq)
       (+ (* 0.7 (pink-noise.ar)))
       (* amp)
       (* (env-gen.kr (perc 0 0.1) :act :free))
       pan2.ar (out.ar out <>)
       ))

(synth 'click)

(defpattern drums :inf
  (play-drum)
  (λ(i)
    (sim nil
         (seq 'bum 'bum)
         ;(seq nil nil ['bum :amp 0.2] nil  nil nil ['click :amp 0.2] ['click :amp 0.1])
         ;(seq nil ['bum :f1 600 :d 0.01] nil nil)
         (once-every i 2 0 (seq nil nil nil nil  nil 'click nil nil))
         )))

(drums :start)

(def dec-bus (bus-control))

(proxy :dec-bus
       (-<> (sin-osc.kr 0.1)
            (range 2000 8000)
            (out.kr dec-bus <>)
            ))

(defsynth saw-bass ((freq 220) (out 0) (amp 0.5) (dur 1) (gate 1))
  (-<> freq
       ;saw.ar
       sin-osc.ar
       (* 0.6)
       (+ (* 1/5 (sin-osc.ar (* freq 3))))
       ;(decimator.ar (in.kr dec-bus) 32)
       (* amp)
       (* (env-gen.kr (adsr 0.01 0.02 0.7 0.5) :act :free :gate gate))
       pan2.ar
       (out.ar out <>)
       ))

(def s1 (synth 'saw-bass :freq 40 :dur 0.5 :amp 1))

(release s1)

(defpattern bass :inf
  (play-note 'saw-bass :note-fn (λ(n) (midicps (+ 40 (sc *pentatonic* n)))))
  (λ(i) 
    (sim nil
         (per-beat i
                   (seq 0 0 0 0)
                   (seq 0 2 4 6)
                   (seq 0 0 0 0)
                   (seq 4 2 3 1)
                   ))))

(bass :start)
(bass :stop)

;;----

(def rev-bus (bus-audio :chanls 2))

(defsynth bum ((out 0) (amp 0.7) (f1 200) (f2 20) (d 1/6))
  (-<> (x-line.kr f1 f2 d)
       sin-osc.ar
       (* amp)
       (* (env-gen.kr (perc 0.0 0.7) :act :free))
       pan2.ar
       (out.ar out <>)
       ))

(defsynth click ((out 0) (amp 0.4) (freq (midicps 74)))
  (-<> (sin-osc.ar freq)
       (+ (* 0.7 (pink-noise.ar)))
       (* amp)
       (* (env-gen.kr (perc 0 0.1) :act :free))
       pan2.ar (out.ar out <>)
       ))

(proxy :rev-bus
       (-<> (in.ar rev-bus 2)
            (decimator.ar 44100 4)
            (freeverb.ar :room 0.7)
            (bpf.ar (range (sin-osc.kr 0.1) 500 2000))
       )
       :pos :tail)

(def echo-bus (bus-audio :chanls 2))

(proxy :echo-bus
       (-<> (in.ar echo-bus 2)
            (freeverb.ar :room 1))
       :pos :tail)

(defsynth bss ((out 0) (amp 0.5) (freq 220))
  (-<> (saw.ar freq)
       (decimator.ar 44100 5)
       (* amp)
       (* (env-gen.kr (perc 0.07 0.9) :act :free))
       (bpf.ar freq)
       pan2.ar
       (out.ar out <>)
       ))

(defsynth hss ((out 0) (amp 0.5) (f 1000))
  (-<> (white-noise.ar)
       (* amp)
       (* (env-gen.kr (perc 0 0.1) :act :free))
       (bpf.ar f)
       pan2.ar
       (out.ar out <>)
       ))

(defsynth tss ((out 0) (amp 0.5))
  (-<> (pink-noise.ar)
       (* amp)
       (* (env-gen.kr (perc 0.0 0.3) :act :free))
       (hpf.ar 6000)
       pan2.ar
       (out.ar out <>)
       ))

(defpattern bss :inf
  (play-note 'bss :note-fn (λ(n) (midicps (+ 35 (sc *pentatonic* n)))))
  (λ(i)
    (sim nil
         (per-beat i
                   (seq -1 0 (random 6) 2)
                   (seq 0 (random 4) 4 (random 7))
                   ))))

(bss :stop)

(defsynth ding ((out 0) (amp 0.5) (freq (midicps (+ 35 (* 12 5)))))
  (-<> (sin-osc.ar freq)
       (* amp)
       (* (env-gen.kr (perc 0 0.3) :act :free))
       pan2.ar
       (out.ar out <>)
       ))

(defpattern drums :inf
  (play-drum :out echo-bus)
  (λ(i)
    (sim nil
         (seq ['bum :out 0 :amp 0.9] (sim ['hss :f (+ 500 (random 1000))] ['bum :out 0 :amp 0.9]))
         ;(seq (loop :repeat 16 :collect (if (= 0 (random 2)) ['ding :amp (/ 1 (+ 4 (random 9)))] nil)))
         (per-beat i
                   ;(seq 'bum)
                   ;(seq 'hss)
                   ;(seq 'bum ['bum :amp 0.4] ['bum :amp 0.1])
                   ;(seq 'hss)
                   ))))

(drums :stop)

; ---

(defsynth sin1 ((freq 440) (out 0) (amp 0.5) (gate 1))
  (-<> (sin-osc.ar freq)
       (* amp)
       (* (env-gen.kr (perc 0.05 1.7) :act :free :gate gate))
       pan2.ar
       (out.ar out <>)
       ))

(defpattern pat1 :inf
  (play-note 'sin1 :note-fn (λ(e) (midicps (+ 60 (sc *pentatonic* e)))))
  (λ(i)
    (sim nil
         (seq -10 -5 -15 -10)
         (per-beat i
                   ;(seq 0 1 2 3)
                   (seq (random 4) nil nil 1)
                   (seq nil (random 5) nil 2)
                   (seq 5 nil 3 (random 6))
                   (seq 2 (if (= 0 (random 2)) (random 4) nil) 0 nil)
                   ))))

(pat1 :stop)

(drums :start)

;; ----

(def bus1 (bus-audio :chanls 2))

(proxy :bus1
       (-<> (in.ar bus1 2)
            (bpf.ar (range (sin-osc.kr 0.07) 300 4000) 0.4)
            (freeverb.ar :room 1)
            ) :pos :tail)

(proxy :sins
       (with-controls ((freq (midicps 40)) (gt 1))
         (-<> (sin-osc.ar freq)
              (+ (* 0.5 (sin-osc.ar (* freq 2)) (range (sin-osc.kr 0.10) 0.2 1)))
              (+ (* 0.3 (sin-osc.ar (* freq 3)) (range (sin-osc.kr 0.11) 0.2 1)))
              (+ (* 0.17 (sin-osc.ar (* freq 4)) (range (sin-osc.kr 0.12) 0.2 1 )))
              (+ (* 0.1 (sin-osc.ar (* freq 5)) (range (sin-osc.kr 0.13) 0.2 1 )))
              (+ (* 0.17 (sin-osc.ar (* freq 6)) (range (sin-osc.kr 0.07) 0.2 1 )))
              (+ (* 0.08 (sin-osc.ar (* freq 7)) (range (sin-osc.kr 0.06) 0.2 1 )))
              (+ (* 0.04 (sin-osc.ar (* freq 8)) (range (sin-osc.kr 0.05) 0.2 1 )))
              (* 0.5)
              (* (range (saw.kr 7) 0 1))
              pan2.ar
              )))

(release :sins)

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
             ;(seq (loop :repeat 8 :collect ['click :out bus1 :amp (/ 1 (+ 4 (random 7)))]))
             ;(once-every i 4 1 (seq (loop :repeat 8 :collect ['bum :out bus1 :amp 0.9])))
             (per-beat i
                       (seq 'bum nil nil nil  nil nil 'bum nil)
                       (seq nil 'bum nil nil  'hss nil nil nil)
                       ))))

(drums :stop)

(defsynth sin1 ((freq 440) (out 0) (amp 0.5))
  (-<> (sin-osc.ar freq)
       (* amp)
       (* (env-gen.kr (perc 0.1 0.4) :act :free))
       pan2.ar (out.ar out <>)
       ))

(synth 'sin1 :freq (midicps 64))

(def b1 (bus-audio :chanls 2))

(def c1 (bus-control))

(control-set c1 32)

(proxy :c1-slide
       (-<> (line.kr 32 3 30) (out.kr c1 <>)))

(proxy :b1
       (-<> (in.ar b1 2)
            ;(decimator.ar 44100 (in.kr c1))
            ;(* 0.1)
            (freeverb.ar :room 1.2)
            ))

(defpattern sin1 :inf
  (play-note 'sin1 :attr [:out b1] :note-fn (λ(e) (midicps (+ 64 (sc *pentatonic* e)))))
  (λ(i) (sim nil
             (per-beat i
                       (seq 0 1 (random 4) 3 4 (random 6))))))

(sin1 :start)

(release s1)

;---

(proxy :test
       (-<> (sin-osc.ar 440)
            (* (env-gen.kr (perc 0.2 0.3) :gate (tr 1)))
            pan2.ar))

(release :test)

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
           (per-beat i
                     (seq 'bum (seq 'hss ['bum :amp 0.3]))
                     (seq 'bum 'hss)
                     ))))

(drums :stop)

(defsynth asin1 ((freq 440) (amp 0.5) (out 0) (gate 1))
  (-<> (sin-osc.ar freq)
       (* (env-gen.kr (adsr 0.07 0.1 0.7 0.4) :gate gate :act :free))
       (* amp)
       pan2.ar
       (out.ar out <>)))

(defpattern test 1
  (play-note 'asin1 :note-fn (λ(e) (midicps (+ 64 (sc *pentatonic* e)))))
  (λ(i) (per-beat i
                  (seq [0 :dur 3.4] [1 :dur 2] [2 :dur 1])
                  )))

(test :start)

;;----------------------------------

(defsynth saw1 ((freq 440) (amp 0.5) (out 0) (gate 1))
  (-<> (saw.ar freq)
       (+ (* 1 (sin-osc.ar (* 4 freq))))
       (* (env-gen.kr (adsr 0.07 0.1 0.7 0.4) :gate gate :act :free))
       (* amp)
       pan2.ar
       (out.ar out <>)
       ))

(let ((s (synth 'saw1 :freq (midicps 30))))
  (sleep 0.1)
  (release s))

(defpattern bass :inf
  (play-note 'saw1)
  (λ(i) (per-beat i
                  (seq [[30 :dur 1/3]])
                  (seq (λ(b d) (let ((s (synth 'saw1 :freq (midicps 30))))
                                 (at-beat (+ b 0.001) (xslide s :freq (midicps 30) (midicps 47) 1/5))
                                 (at-beat (+ b d -0.1) (release s))
                                 )))
                  (seq 47)
                  (seq 47)
                  )))

(bass :start)

(defsynth test ((out 0) (gate 1) (amp 0.5) (freq 440))
  (-<> (* (sin-osc.ar freq)
          (env-gen.kr (adsr 0.1 0.2 0.7 0.4) :gate gate))
       (+ (* 0.2 (saw.ar freq)
             (env-gen.kr (adsr 0.05 0.1 0.3 0.2) :gate gate)))
       (* (env-gen.kr (asr 0 1 0.5) :act :free :gate gate))
       (* amp)
       pan2.ar
       (out.ar out <>)))

(defsynth fm ((out 0) (gate 1) (amp 0.5) (freq 440) (mf 4) (ma 40))
  (-<> (sin-osc.ar (+ freq (* ma (sin-osc.kr (/ freq mf)))))
       (* amp)
       (* (env-gen.kr (perc 0.05 0.4) :act :free))
       pan2.ar
       (out.ar out <>)))

(proxy :ss
       (-<> (sin-osc.ar 440)
            (clip.ar (range (sin-osc.kr 0.1) -1 -0.001) (range (sin-osc.kr 0.2) 0.001 1))
            pan2.ar))

(release :ss)

(defpattern test-fm 1
  (play-note 'fm :note-fn (λ(e) (- (midicps e) 100)) :attr [:mf 2 :ma 30])
  (λ(i) (seq 60 63 65 67 70 72)))
(test-fm :start)

;;;;;;

(defsynth ssin ((out 0) (gate 1) (freq 440) (amp 0.5))
  (-<> (f-sin-osc.ar freq)
       (* amp (env-gen.kr (adsr 0.01 0.3 0.7 0.4) :act :free :gate gate))
       pan2.ar
       (out.ar out <>)
       ))

(defsynth ssaw ((out 0) (gate 1) (freq 440) (amp 0.5))
  (-<> (saw.ar freq)
       (* amp (env-gen.kr (adsr 0.01 0.2 0.6 0.7) :act :free :gate gate))
       pan2.ar
       (out.ar out <>)))

(def bus (bus-audio :chanls 2))

(def cl (bus-control))

(control-set (busnum cl) 0)

(proxy :bus
       (-<> (in.ar bus 2)
            (* (env-gen.kr (perc 0 2.2) :gate (in.kr cl)))
            ) :pos :tail)

(defpattern ssin :inf
  (play-note 'ssin
             :attr [:dur 2 :amp 0.2 :out bus]
             :note-fn (λ(n) (midicps (+ 60 (sc *minor* n)))))
  (λ(i) (per-beat i
                  (sim 0 2 4 6)
                  nil
                  (sim 0 2 5)
                  nil
                  )))

(defpattern ctl :inf
  (λ(b d e) (when e (control-set (busnum cl) e)))
  (λ(i) (per-beat i
                  (seq (loop :repeat 2 :for i :from 0 :collect (mod i 2))))))

(ctl :start)

(ssin :start)

(control-set (busnum cl) 0)

;;;;;;;;

(defsynth bass ((out 0) (amp 0.5) (gate 1) (freq 110))
  (-<> (sin-osc.ar freq)
       ;(+ (* 1/2 (sin-osc.ar (* freq 2))))
       (+ (* 1/3 (sin-osc.ar (* freq 3))))
       (+ (* 1/4 (sin-osc.ar (* freq 4))))
       (+ (* 0.01 (saw.ar (* freq 4))))
       ;(+ (* 1/5 (sin-osc.ar (* freq 5))))
       (* 1/2 amp (env-gen.kr (adsr 0.021 0.2 0.7 0.4) :act :free :gate gate))
       pan2.ar
       (out.ar out <>)
       ))

(bpm 60)

(defpattern bass :inf
  (play-note 'bass
             :attr [:out ab1]
             :note-fn (λ(e) (midicps (+ 30 (sc *minor* e)))))
  (λ(i) (sim nil
             (per-beat i
                       (seq [0 :dur 2] nil -1)
                       (seq 0)
                       ))))

(bass :stop)

(def ab0 (bus-audio :chanls 2))

(proxy :ab0
       (-<> (in.ar ab0 2)
            (* (line.kr 0.8 0 30))
            ) :pos :tail)

(def ab1 (bus-audio :chanls 2))

(proxy :ab1
       (-<> (in.ar ab1 2)
            ;(bpf.ar (range (sin-osc.kr 0.1) 200 5000))
            ) :pos :tail)

(defpattern keys :inf
  (play-note 'ssaw
             :attr [:amp 0.01 :out ab0]
             :note-fn (λ(e) (midicps (+ 66 (sc *minor* e)))))
  (λ(i) (sim nil
             (per-beat i
                       (seq 7 4 (random 4) 1 0 4 6 1)
                       (seq (random 8) 4 6 1  -1 4 (random 8) 1)
                       ))))

(keys :start)

(defpattern drums :inf
  (play-drum :amp 0.7)
  (λ(i) (sim nil
             (per-beat i
                       (seq nil nil nil ['click :amp 0.2] nil)
                       )
             (per-beat i
                       (seq (seq 'bum ['bum :amp 0.01]) 'ph)
                       ))))

(drums :start)

(defsynth fms ((out 0) (gate 1) (amp 0.5) (freq 220))
  (-<> (sin-osc.ar (+ freq (* (/ freq 4) (sin-osc.kr (/ freq 4)))))
       (* amp (env-gen.kr (adsr 0.05 0.2 0.7 0.4) :act :free :gate gate))
       pan2.ar
       (out.ar out <>)))

(defpattern fms :inf
  (play-note 'fms
             :note-fn (λ(i) (midicps (+ 55 (sc *pentatonic* i))))
             :attr [:amp 0.1])
  (λ(i) (sim nil
             (per-beat i
                       (seq 0)
                       (seq 1)
                       (seq [[-2 :dur 5/3]])
                       (seq nil (once-every i 8 3 (seq -1)))
                       ))))

(fms :start)

;;;;;;;;

(def c (bus-control))

(proxy :c
       (out.kr c (range (sin-osc.kr 0.1) 0 1)))

(def o (bus-audio :chanls 2))

(proxy :o
       (-<> (in.ar o 2)
            (lpf.ar (range (sin-osc.kr 0.14) 200 1000))
            (freeverb.ar :room 0.3 :mix 0.6)
            ))

(defsynth snt ((out 0) (amp 0.5) (gate 1) (freq 220))
  (-<> (sin-osc.ar freq)
       ;(+ (* 0.01 (sin-osc.ar (* freq 6))))
       (+ (* 0.051 (pulse.ar (* 1 freq)) (in.kr c)))
       (* amp (env-gen.kr (adsr 0.02 0.02 0.4 0.5) :act :free :gate gate))
       (out.ar out (pan2.ar <>))
       ))

(synth 'snt)

(defpattern snt :inf
  (play-note 'snt
             :attr [:amp 0.5  :out o]
             :note-fn (λ(i) (midicps (+ 60 (sc *pentatonic* i)))))
  (λ(i) (sim nil
             (if (= 0 (random 4)) (seq (random 6)) nil)
             (per-beat i
                       (seq -2 -1 0 1 2 3 4)
                       (seq 5 3 4 2 3 0 -1)
                       )
             )))

(snt :start)

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
             (per-beat i
                       (seq 'bum (sim 'bum 'hss))
                       ;(seq 'bum (seq (sim 'bum 'hss) nil nil ['bum :amp 0.2]))
                       )
             )))

(drums :start)

;;;

(defsynth saw ((freq 440) (freq0 440) (sltm 0) (amp 0.4) (out 0) (gate 1) (dur 1/8) (a 0) (r 0))
  (-<> (x-line.kr freq0 freq sltm)
       (saw.ar)
       (* amp (env-gen.kr (linen a dur r) :act :free :gate gate))
       pan2.ar (out.ar out <>)))

(synth 'saw :freq0 200 :freq 400 :sltm 0.1 :r 1)


(bpm 30)

(defsynth bmm ((out 0) (gate 1) (freq 100) (amp 0.4))
  (-<> (* 0.7 (sin-osc.ar (x-line.kr 220 20 0.2)))
       (+ (sin-osc.ar freq))
       (* 1/2 amp (env-gen.kr (perc 0 0.4) :gate gate :act :free))
       pan2.ar (out.ar out <>)))

(synth 'bmm)

(def
  b ['bmm :amp 0.021]
  h ['hh :d 0.10]
  z ['saw :freq 100 :amp 0.1])

(defpattern euc :inf
  (play-drum)
  (λ(i) (sim 'bum
             (seq ['saw :amp 0.1 :freq0 200 :freq 100 :sltm 0.3 :dur 0.4] 'ph1)
             (seq (euclidian z nil 3 8))
             (seq (rotate (euclidian 'bmm b 5 8) 1))
             (once-every i 2 0 (seq nil nil ['saw :freq 200 :amp 0.1]))
             )))

(euc :stop)

;;;

(defsynth mf ((freq 440) (amp 0.4) (out 0) (gate 1))
  (-<> (saw.ar freq)
       (lpf.ar (x-line.kr (* freq 20) freq 1/2))
       (bpf.ar freq (line.kr 3 0.1 1))
       (* amp (env-gen.kr (adsr 0.1 0.2 0.6 1) :gate gate :act :free))
       pan2.ar (out.ar out <>)))

(defpattern mf :inf
  (play-note 'mf
             :attr [:amp 0.2]
             :note-fn (λ(i) (->> i (sc *pentatonic*) (+ 40) midicps)))
  (λ(i) (sim nil
             (per-beat i
                       (seq -2 -1 0 1)
                       (seq 1 (rand-el 3 2) 2 -1))
             (per-beat i
                       (seq 0 1)
                       (seq 3 (rand-el 4 5))
                       ))))

(mf :stop)

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
             (seq (loop :repeat 12 :collect ['hh :d 0.1 :amp (/ 1 (+ 15 (random 7)))]))
             (seq (euclidian 'bum 'hh 5 8))
             (per-beat i
                       (seq 'bmm 'ph)
                       (seq 'bmm 'bmm 'ph nil))
             )))

(drums :stop)

(proxy :test
       (-<> (* 0.3 (saw.ar (+ 220 (range (sin-osc.kr 2) -1 1))))
            (+ (* 0.2 (range (saw.kr 0.1) 0 1) (saw.ar (* 3 220))))
            (+ (* 0.1 (range (saw.kr 0.02) 0 1) (saw.ar (* 4 220))))
            (+ (* 0.1 (range (saw.kr 0.03) 0 1) (saw.ar (* 5 220))))
            (* 0.3)
            (freeverb.ar :room 0.7 :mix 0.7)
            ;(lpf.ar (range (sin-osc.kr 0.05) 200 10000))
            pan2.ar))

(synth 'bum)

(defpattern test :inf
  (play-drum)
  (λ(i)
    (let ((e (euclidian 'bum 'ph1 5 8)))
      (sim (seq (rotate (euclidian 'ph 'hh (rand-el 2 3 5 7) 8) 3))
           (per-beat i 
                     (seq (subseq e 0 4))
                     (seq (subseq e 4))))
  )))

(test :stop)

(proxy :test
       (-<> [ (pulse.ar 440 (range (sin-osc.kr 3) 0.01 0.99))
              (pulse.ar 440 (range (sin-osc.kr 3.1) 0.99 0.01)) ]
            (* 0.1)
            ))

(proxy :test
       (-<> (* (pulse.ar 6)
               (saw.ar 440)
               )
            (rlpf.ar (range (sin-osc.kr 0.1) 200 7000))
            (* 0.1)
            (pan2.ar (sin-osc.kr 3))))

(bpm 40)

(defpattern drums :inf
  (play-drum)
  (λ(i) 
    (per-beat i
              (seq 'bum (sim 'bum 'ph))
              (seq 'bum (sim 'hh))
              )))

(def o1 (bus-audio :chanls 2))

(proxy :o1
       (-<> (in.ar o1 2)
            ;(clip.ar -0.1 0.1)
            (rlpf.ar (range (sin-osc.kr 0.1) 300 6000))
            (+ <>
               (* 0.15 (delay-n.ar <> 0.15))
               (* 0.15 (delay-n.ar <> 0.25)))
            (freeverb.ar :room 0.3 :mix 0.3)
            ))

(defsynth tst ((out 0) (amp 0.4) (freq 440) (gate 1))
       (-<> (* (saw.ar freq)
               (pulse.ar (+ freq 1.2)))
            (+ (* 1/5 (saw.ar (+ (* (sin-osc.kr 10) 4) (* freq 3)))))
            (+ (* 1/4 (sin-osc.ar (* 2 freq))))
            (* amp (env-gen.kr (adsr 0.1 0.5 0.2 0.5)  :gate gate  :act :free))
            pan2.ar
            (out.ar out <>)
            ))

(defpattern tst :inf
  (play-note 'tst
             :attr [:out o1 :amp 0.55]
             :note-fn (λ(i) (midicps (+ 45 (sc *pentatonic* i)))))
  (λ(i) (sim nil
             (per-beat i
                       (seq (seq -1) (seq 0) (seq 1) (seq 2))
                       ))))

(tst :start)
(tst :stop)

(def hh ['hh :amp 0.1 :d 0.15])

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
           (per-beat i 
                     (seq 'bum 'bum)
                     (seq 'bum hh 'bum ['bum :amp 0.3])
                     (seq (euclidian 3 8 'bum hh))
                     (seq (euclidian 5 8 'bum))))))

(drums :start)
(drums :stop)

(defsynth ssin ((out 0) (freq 440) (freq0 440) (slide 0) (gate 1) (amp 0.3))
  (-<> (x-line.kr freq0 freq slide)
       (+ (* 10 (sin-osc.kr 10)))
       (sin-osc.ar)
       (* amp (env-gen.kr (adsr 0.1 0.2 0.5 0.5) :gate gate :act :free))
       pan2.ar (out.ar out <>)))

(release s1)
(def s1 (synth 'ssin :freq0 20 :slide 1/4 :freq (midicps 76)))

(defpattern ssin :inf
  (λ(b d e)
    (when e 
      (let* ((n1 (midicps (+ 76 (sc *major* (car e)))))
             (n2 (midicps (+ 76 (sc *major* (cadr e)))))
             (s (synth 'ssin :freq n2 :freq0 n1 :slide 1/4)))
        (at-beat (+ b d) (release s)))))
  (λ(i) (sim nil
             (per-beat i
                       (seq [[-2 0] [0 2]])
             ))))

(ssin :start)

(proxy :ttt (with-controls ((freq 220) (cf 900))
              (-<> (pulse.ar freq)
                   (* 0.4)
                   (bpf.ar (in.kr bb) 0.09)
                   pan2.ar
                   )))

(def oo (bus-control))

(def bb (bus-control))

(control-set (busnum bb) 440)

(proxy :ooo (out.kr oo (range (sin-osc.kr 0.1) 200 4000)))

(defsynth ow ()
  (out.kr bb (in.kr oo))
  (env-gen.kr (perc 0 0.01) :act :free)
  )

(defpattern ptn :inf
  (play-note 'ow :release nil)
  (λ(i) (sim nil
             (per-beat i
                       (seq 1 1 1 1 1 1)
             ))))

(ptn :start)

(ctrl :ttt :freq 220)

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
             (per-beat i
                       (seq (euclidian 7 12 'bum nil))
             ))))

(drums :start)

;---

(defpattern saw :inf
  (play-note 'saw
             :attr [:out ab1]
             :note-fn (λ(e) (midicps (+ 40 (sc *pentatonic* e)))))
  (λ(i) (sim nil
             (seq -5 -3 -2)
             (seq 4 5 6 7 8 (random 10))
             (per-beat i
                       (seq -1 0 1 2)))))

(saw :start)
(saw :stop)

(defpattern ssin :inf
  (play-note 'ssin
             :attr [:out ab2 :amp 0.1]
             :note-fn (λ(e) (midicps (+ 40 (sc *pentatonic* e)))))
  (λ(i) (sim nil
             (seq (rotate (euclidian 5 8 10 13) (random 3))))))

(ssin :start)
(ssin :stop)

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
             (seq nil ['ph :d 0.2])
             (seq (loop :repeat 8 :collect ['hh :d 0.1 :amp (/ 0.7 (+ 4 (random 12)))]))
             (seq (rotate (euclidian 3 8 'click 'bmm) (random 3)))
             (per-beat i
                       (seq 'bum 'bum)
                       )
             )))

(drums :start)
(drums :stop)

(defpattern drums1 :inf
  (play-drum :out ab3)
  (λ(i) (seq (loop :repeat 8 :collect ['hh :d 0.07 :amp 0.4]))))

(drums1 :start)

(drums1 :start)

(release :drone)

(defpattern drone :inf
  (λ(b d e) (when e (ctrl :drone :f (midicps e))))
  (λ(i)
    (seq (per-beat i
                   52
                   52
                   52
                   50
                   50
                   52))))

(drone :start)

(defpattern zsin :inf
  (play-note 'ssin
             :attr [:amp 0.2 :out ab2]
             :note-fn (λ(e) (midicps (+ 52 (sc *pentatonic* e)))))
  (λ(i) (sim nil
             (seq (per-beat i
                       10 9 8 7  6 5 4 3  2 1 0 -1)))))

(zsin :start)
(zsin :stop)

(release :drone)

(defsynth ssin ((freq 440) (amp 0.4) (out 0) (gate 1))
  (-<> freq
       (lag.kr 1 1)
       (sin-osc.ar freq)
       (* amp (env-gen.kr (adsr 0.1 0.3 0.5 0.4) :act :free :gate gate))
       (pan2.ar) (out.ar out <>)))

(release s)
(def s (synth 'ssin :freq 220))

(ctrl s :freq 440)

;;---

(def ca (bus-control))

(proxy :ca (out.kr ca (range (sin-osc.kr 0.1) 100 450)))

(defsynth fmsin ((freq0 440) (freq 440) (slide 0) (amp 0.3) (out 0) (gate 1)
                             (q 4) (depth 200) (a 0.04) (d 0.3) (s 0.6) (r 0.5))
  (-<> (x-line.kr freq0 freq slide)
       (+ (* q <>) (* depth (sin-osc.kr <>)))
       (sin-osc.ar)
       (* amp (env-gen.kr (adsr a d s r) :act :free :gate gate))
       pan2.ar (out.ar out <>)
       ))

(defun cpsf (i) (midicps (+ 30 (sc *pentatonic* i))))
(defpattern fmsin :inf
  (play-note 'fmsin 
             :attr [:q 3 :depth 300 :amp 0.3 :out 0
                       :a 0.01 :d .2 :s 0.3 :r 0.3]
             :note-fn (let ((prev nil)) 
                        (λ(i)
                          (if (vectorp i)
                              [:freq0 (cpsf (elt i 0)) :freq (cpsf (elt i 1)) :slide 0.1]
                              [:freq (cpsf i)]))))
  (λ(i) (sim nil
             (per-beat i
                       (seq 0 0 0 0 0 3)
                       (seq [5 :freq0 (cpsf 3) :slide 0.1] 3 4 4 4 4)
                       ;(seq 4 4 4 #(5 7) 2 3)
                       ;(seq 5 4 3 #(2 5) 5 #(5 8))
                       ))))

(fmsin :start)
(fmsin :stop)

(proxy :drone
       (-<> (pulse.ar (midicps 30))
            (bpf.ar 200 0.2)
            pan2.ar
            ))

(def s ['ssnt :s1 1 :s2 1 :s3 0.3 :amp 0.1])

(defpattern zzz :inf
  (play-note 'ssnt
             :note-fn (λ(i) [:freq (midicps (+ 40 0 (sc *pentatonic* (+ i 0))))])
             :attr [:out 0
                         :s1 1 :s2 0 :s3 0 :amp 0.4
                         :a 0.2 :d 0.2 :s 0.4 :r 0.5])
  (λ(i) 
    (sim
      nil
      (seq [8 10 7 8 7 3 4 (random 6)])
      (seq (rotate [8 (random 10) 7 8 (random 7) 3 4 5] 3))
      )))

(zzz :start)
(zzz :stop)

(defpattern drums :inf
  (play-drum :out 0)
  (λ(i) (sim nil
             (seq (euclidian 5 8 ['hh :amp 0.1 :d 0.1] 'ph1))
             ;(seq (loop :repeat 8 :collect ['hh :d 0.1 :amp (/ 3 (+ 15 (random 10)))]))
             (seq 'bum 'bum)
             (seq nil ['hh :amp 0.3 :d 0.2])
             ;(seq 'bdr 'ph)
             )))

(drums :start)
(drums :stop)

(defpattern bass :inf
  (play-note 'fmsin
             :note-fn (let ((prev nil))
                        (λ(i) [:freq (midicps (+ 40 (sc *pentatonic* i)))]))
             :attr [:out 0
                         :depth 200 :q 4
                    :a 0.05 :d 0.1 :s 0.7 :r 0.4])
  (λ(i) (per-beat i
                  (seq -1 (random 3))
                  (seq [[1 :amp 0.2]])
                  (seq -2 0)
                  nil
                  )))

(bass :start)
(bass :stop)

(proxy :test
       (-<> (impulse.ar 482)
            (pan2.ar)))

(release :test)

;;;;

(bpm 50)

(def ptn1 (make-generator () (loop :do (loop :for i :from 0 :to 4 :do (yield (sc #(0 3 5 7 10) i))))))

(defpattern ptn :inf
  (play-note 'ssnt
             :attr [:s3 1/4 :s2 1/4 :s1 1/2 :amp 0.1 :out ab1]
             :note-fn (λ(i) [:freq (midicps (+ 60 i))]))
  (λ(i) (seq (loop :repeat 8 :collect (next ptn1)))))

(ptn :start)
(ptn :stop)

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
             (seq 'bum 'bum)
             (seq nil 'hss)
             (once-every i 4 1 (seq nil nil nil nil  nil 'bum nil nil))
             (seq nil ['click :freq (midicps (+ 60 24)) :d 0.05 :amp .1])
             (once-every i 4 0 (seq nil nil nil (seq ['hh :amp 0.12 :d 0.10] ['hh :amp 0.12 :d 0.10])))
             )))

(drums :start)
(drums :stop)

(defpattern bass :inf
  (play-note 'fmsnt
             :attr [:amp 0.2]
             :note-fn (λ(i) [:freq (midicps(+ 34 i))]))
  (λ(i) (per-beat i
                  (seq 0)
                  [[3 :slide 0.2 :freq0 (midicps (+ 34 0))]]
                  (seq 5 5 5 5 5 5)
                  (seq [7 :slide 0.2 :freq0 (midicps (+ 34 5)) :dur 3] nil nil 10))))

(bass :start)
(bass :stop)

;;;;;

(def depth 100)
(def zn [:amp 0.3 :out 0 :depth 60 :dur 1/5])

(defpattern bass :inf
  (play-note 'fmsnt
             :attr [:amp 0.3 :out ab1
                         :depth depth  :q 6
                         :a 0.0 :d 0.1 :s 0.8 :r 0.5]
             :note-fn (λ(i) [:freq (midicps (+ 40 i))]))
  (λ(i) (sim nil
             (seq (euclidian 5 8
                             (append [-3 :freq0 (midicps (+ 40 -5)) :slide 0.051] zn)
                             (append [-5 :freq0 (midicps (+ 40 -3)) :slide 0.051] zn)))
             (per-beat i
                       (sim nil
                            (seq [0 :dur 1/3 :depth depth] nil nil (seq [0 :dur 2/3] (if (= (random 2) 0) nil [0 :dur 2/3]))))
                       nil))))

(bass :start)
(bass :stop)

(defpattern bass-alt :inf
  (λ(b d e) (when e (setf depth e)))
  (λ(i) (seq (rand-el 20 50 100 200 300 400))))

(bass-alt :start)

(proxy :zzz
       (-<> (saw.ar (+ (midicps 83) (* 20 (sin-osc.kr 10))))
            (* (line.kr 0 0.04 10))
            pan2.ar))

(release :zzz)

(defmacro defseq (name aseq)
  (let ((seq0 (gensym))
        (spx (gensym)))
    `(let ((,seq0 ,aseq) (,spx ,aseq))
       (defun ,name ()
         (prog1 (car ,spx)
           (setf ,spx (cdr ,spx))
           (if (null ,spx) (setf ,spx ,seq0)))))))

(defseq sq1 [0 1 2 5  4 5 6 0  8 3 6 3  4 3 2 1])

(defpattern sn :inf
  (play-note 'ssaw
             :attr [:amp 0.1 :out ab2
                         :a 0.0 :d 2.2 :s 0
                         ]
             :note-fn (λ(i) [:freq (midicps (+ 64 (sc *pentatonic* i)))]))
  (λ(i) 
    (seq (sq1) (sq1) (sq1) (sq1))))

(sn :stop)
(sn :start)

;;;;;

(defsynth ding ((freq 440) (amp 0.6) (out 0))
  (-<> (+ (* 1/2 (sin-osc.ar freq))
          (* 1/4 (sin-osc.ar (* 2.5 freq))))
       (* amp (env-gen.kr (perc 0 0.1) :act :free))
       pan2.ar (out.ar out <>)))

(defsynth ph ((out 0) (amp 0.6) (bp 1800))
  (-<> (pink-noise.ar)
       (bpf.ar bp 1.9)
       (* amp (env-gen.kr (perc 0 0.2) :act :free))
       pan2.ar (out.ar out <>)
       ))

(defsynth bd ((f 1600) (d 0.02) (out 0) (amp 0.4))
  (-<> (x-line.kr f 20 d)
       (sin-osc.ar)
       (* amp (env-gen.kr (perc 0 3) :act :free))
       pan2.ar (out.ar out <>)))

(defsynth noise ((out 0) (amp 0.1) (d 0.04))
  (-<> (clip-noise.ar)
       (* amp (env-gen.kr (linen 0 d 0) :act :free))
       pan2.ar (out.ar out <>)))

(def n ['noise :amp 0.1])
(def n1 ['noise :amp 0.01 :d 0.09])
(def b ['bd :f 800 :d 0.051 :amp 0.7])

(defpattern drums :inf
  (play-drum :out ab2 :amp 0.4)
  (f_ (sim nil
           (seq (rotate (euclidian 3 8 n n1) (random 4)))
           (once-every _ 4 0 (seq nil ['noise :amp 0.1 :d 1/3]))
           (seq b b)
           (seq (euclidian 3 8 ['ph :bp 11800] 'ph))
           (per-beat _
                     (seq 'ding 'ding nil 'ding)
                     nil
                     (seq 'ding nil nil (seq 'ding 'ding))
                     (seq nil (seq 'ding 'ding) nil 'ding)
                     ))))

(proxy :ab2
       (-<> (in.ar ab2 2)
            (* (line.kr 1 0 10))
            ;(freeverb.ar :mix (line.kr 0.51 0 10) :room 0.5)
            ))

(drums :start)
(drums :stop)

(defsynth fm-bass ((freq 220) (freq0 220) (slide 0) (out 0) (amp 0.5) (gate 1))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (sin-osc.ar (* 3 fq))
         (* 400) (+ fq)
         (sin-osc.ar)
         (* amp (env-gen.kr (adsr 0.01 0.2 0.6 0.4) :act :free :gate gate))
         pan2.ar (out.ar out <>))))

(defpattern bass :inf
  (play-note 'fm-bass
             :attr [:amp 0.3 :out ab1])
  (f_ (sim nil
           (per-beat _
                     (seq [[66 :freq0 (midicps 54) :slide 1/8]])
                     (seq [[45 :freq0 (midicps 66) :slide 1/8]])
                     (seq [[42 :freq0 (midicps 66) :slide 1/8]])
                     (seq [[49 :freq0 (midicps 38) :slide 1/8]]))
           (seq (euclidian 5 8 [54 :dur 0.1 :amp 0.3] [66 :dur 0.1 :amp 0.3]))
           (seq (rotate (euclidian 5 8 [42 :dur 0.1 :amp 0.3] [54 :dur 0.1 :amp 0.3]) 3))
           (seq (rotate (euclidian 3 8 [33 :dur 0.1 :amp 0.3] [45 :dur 0.1 :amp 0.3]) 6))
           )))

(bass :start)
(bass :stop)

(proxy :ab1
       (-<> (in.ar ab1 2)
            (* (line.kr 1 0 10))
            (bpf.ar (range (sin-osc.kr 0.1) 200 4000))
            ;(freeverb.ar :room 0.7 :mix 0.4 :damp 0.7)
            ))

(defsynth ssin ((freq 440) (freq0 440) (slide 0) (amp 0.6) (out 0) (gate 1)
                           (a 0.0) (d 0.2) (s 0.7) (r 0.3))
  (-<> (x-line.kr freq0 freq slide)
       (sin-osc.ar)
       (* amp (env-gen.kr (adsr a d s r) :act :free :gate gate))
       pan2.ar (out.ar out <>)))

(defpattern ssin :inf
  (play-note 'ssin
             :note-fn (f_ [:freq (midicps (+ 78 (sc *pentatonic* _)))])
             :attr [:amp 0.17 :out ab3 :dur 1/32])
  (f_ (per-beat _
                (seq -3 -2 -1 0 1 2 3 4)
                (seq 5 4 3 2 1 0 -1 -2))))

(proxy :ab3
       (-<> (in.ar ab3)
            ;(* (line.ar 1 0 10))
            (freeverb.ar :mix 0.6)
            ))

(ssin :start)
(ssin :stop)

(proxy :drone
       (let ((freq (midicps 52)))
         (-<> (+ [(* 1/2 (sin-osc.ar freq)) (* 1/2 (sin-osc.ar (1+ freq)))]
                 [(* 1/3 (sin-osc.ar (* freq 2))) (* 1/3 (sin-osc.ar (+ 1.2 (* freq 2))))]
                 [(* 1/4 (sin-osc.ar (* freq 3))) (* 1/4 (sin-osc.ar (+ 1.4 (* freq 3))))]
                 ;[(* 1/5 (sin-osc.ar (* freq 4))) (* 1/5 (sin-osc.ar (+ 1.5 (* freq 4))))]
                 )
              (* 0.3)
            )))

(release :drone)

(defpattern ssin :inf
  (play-note 'ssin
             :note-fn (λ(e) [:freq (midicps (+ 64 (sc *pentatonic* e)))])
             :attr [:amp 0.3 :out fade-out])
  (f_ (sim nil
           (once-every _ 4 0
                       (seq [[3 :d 1 :s 0.7 :r 5]]))
           (per-beat _
                     (seq 1 0 -1)
                     (seq 2 -1 0))
           )))

(ssin :start)

(def fade-out (bus-audio :chanls 2))

(proxy :fade-out
       (-<> (in.ar fade-out 2)
            (* (line.kr 1 0 10))
            ))

(defpattern percs :inf
  (play-drum :out fade-out)
  (f_ (sim nil
           (seq 'bd 'bd)
           (per-beat _
                     (seq 'noise)
                     (seq nil 'noise nil ['noise :d 1/3] nil)
                     nil
                     (seq 'noise (seq (loop :repeat (rand-el 4 5 7 10) :collect 'noise)))
                     ))))

(percs :start)
(percs :stop)

(def ab4 (bus-audio :chanls 2))

(proxy :ab4
       (-<> (in.ar ab4 2)
            (+ <> (* 1/2 (delay-l.ar <> 0.17)))
            (freeverb.ar :mix 0.4 :room 0.7)
            ))

(defpattern fms :inf
  (play-note 'fm-bass
             :attr [:amp 0.3 :out ab4]
             :note-fn (f_ [:freq (midicps (+ 28 (sc *pentatonic* _)))]))
  (f_ (per-beat _
                (seq -1 0 1 2 -1 0)
                )))

(fms :start)
(fms :stop)

;;;;;

(defsynth fm-bass ((freq 220) (freq0 220) (slide 0) (out 0) (amp 0.5) (gate 1)
                              (q 3) (depth 400)
                              (a 0.01) (d 0.2) (s 0.6) (r 0.4))
  (let ((fq (x-line.kr freq0 freq slide)))
    (-<> (sin-osc.ar (* q fq))
         (* depth) (+ fq)
         (sin-osc.ar)
         (* amp (env-gen.kr (adsr a d s r) :act :free :gate gate))
         pan2.ar (out.ar out <>))))

(defpattern bass :inf
  (play-note 'fm-bass
             :attr [:q 1 :depth 400
                    :a 0.0 :r 3 :amp 0.2 :dur 1/2]
             :note-fn (f_ [:freq (midicps (+ 30 (sc *minor* _)))]))
  (f_ (sim nil
           (sim nil
                (per-beat _
                          (sim (seq 7) 0)
                          (sim [6 :slide 1/4 :freq0 (midicps (+ 30 (sc *minor* 2)))] nil)
                          (seq 5)
                          (seq 4 (rand-el 9 8 nil))
                          )))))

(bass :start)
(bass :stop)

(def o nil)

(defpattern solo :inf
  (play-note 'ssin
             :attr [:amp 0.3 :s 0.3 :r 1 :dur 2/3]
             :note-fn (f_ [:freq (midicps (+ 54 (sc *minor* _)))]))
  (f_ (per-beat _
                (seq 0 2)
                (seq nil 3 4)
                (seq 7 4)
                (seq nil 3 2)
                (seq nil 3 4)
                (seq 2 -1)
                )))

(solo :start)
(solo :stop)

(defsynth hh ((out 0) (amp 0.3) (d 0.2))
  (-<> (white-noise.ar)
       (bpf.ar 10000)
       (* amp (env-gen.kr (perc 0 d) :act :free))
       pan2.ar (out.ar out <>)
       ))

(defsynth bd ((f 1600) (d 0.02) (out 0) (amp 0.4))
  (-<> (x-line.kr f 20 d)
       (sin-osc.ar)
       (* amp (env-gen.kr (perc 0 3) :act :free))
       pan2.ar (out.ar out <>)))

(defpattern drums :inf
  (play-drum)
  (f_ 
    (sim (seq ['bd :amp 0.7 :d 0.10 :f 400] 'bd)
         ;(once-every _ 4 0 (seq nil nil 'bd ))
         ;(once-every _ 4 1 (seq nil (seq 'bd 'bd 'bd)))
         (seq (rotate (mapcar (f_ (when _ [_ :amp (/ 1 (+ 4 (random 5)))])) (euclidian 5 8 'hh 'bd)) (random 3)))
         )))

(drums :start)

(drums :stop)
(bass :stop)
(solo :stop)

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
             (per-beat i
                                             (seq )))))

(drums :start)
(drums :stop)

;;;;;

(proxy :drone
       (let ((f (midicps 52)))
         (-<> (* (sin-osc.ar f) (-> 0.1 sin-osc.kr (range 0.2 1)))
              (+ (-> f (* 2) sin-osc.ar (* 1/2) (* (-> 0.11 sin-osc.kr (range 0.2 1)))))
              (+ (-> f (* 3) sin-osc.ar (* 1/3) (* (-> 0.12 sin-osc.kr (range 0.2 1)))))
              (+ (-> f (* 4) sin-osc.ar (* 1/4) (* (-> 0.13 sin-osc.kr (range 0.2 1)))))
              (+ (-> f (* 5) sin-osc.ar (* 1/5) (* (-> 0.15 sin-osc.kr (range 0.2 1)))))
              (* 1/5)
              pan2.ar)))

(release :drone)

(defsynth bass ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (-<> (x-line.kr freq0 freq slide)
       (saw.ar)
       (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
       pan2.ar (out.ar out <>)))

(def ab1 (bus-audio :chanls 2))

(proxy :ab1
  (-<> (in.ar ab1 2)
       ;(bpf.ar (range (sin-osc.kr 0.05) 700 6000))
       ))

(defsynth fm-bass ((freq 220) (amp 0.2) (out 0) (depth 400) (q 2) (gate 1)
                             (a 0.0) (d 0.2) (s 0.5) (r 0.5))
  (-<> (sin-osc.ar freq) (* depth) (+ (* q freq))
       (sin-osc.ar)
       (* amp (env-gen.kr (adsr a d s r) :act :free :gate gate))
       pan2.ar (out.ar out <>)))

(defsynth bd ((out 0) (amp 0.3))
  (-<> (x-line.ar 400 60 0.073)
       (sin-osc.ar)
       (+ (-> (pink-noise.ar) (lpf.ar 400) (* 0.6)) )
       (* amp (env-gen.kr (perc 0.0 2/3) :act :free))
       pan2.ar (out.ar out <>)))

(drums :start)
(drums :stop)

(defpattern bass :inf
  (play-note 'fm-bass
             :attr [:amp 0.9 :out 0 :q 1 :depth 40 :dur 2/4 :r 1/2 :a 0.021]
             :synth-fn (λ(s e) (let ((f (getf e :freq)))
                                 [(apply #'synth (cons s (mergeplist e [:freq (* 2 f)
                                                                        :amp 1/10 ])))
                                  (apply #'synth (cons s (mergeplist e [:freq f])))]))
             :note-fn (f_ [:freq (midicps (+ 30 (sc *pentatonic* _)))]))
  (f_ (sim nil
           (per-beat _
                     ;(seq -1 0 0 0)
                     ;(seq 0 0 -1 0)
                     ;(seq 0 0 0 0)
                     ;(seq 1 0 0 1)
                     (seq (euclidian 3 8 0 5))
                     (seq (euclidian 2 8 0 4))
                     (seq (euclidian 4 8 0 3))
                     (seq (euclidian 3 8 0 3))
                     (seq (euclidian 5 8 0 5))
                     (seq (euclidian 6 8 0 4))
                     (seq (euclidian 5 8 0 3))
                     (seq (euclidian 3 8 0 6))
                     ))))

(bass :start)
(bass :stop)

(defsynth ssin ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> (sin-osc.ar fq)
              (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
              pan2.ar (out.ar out <>)))))

(def ab2 (bus-audio :chanls 2))

(proxy :ab2
  (-<> (in.ar ab2 2)
       (freeverb.ar :mix (line.kr 0 0.7 15))
       ))

(defpattern solo :inf
  (play-note 'ssin
             :attr [:out 0 :amp 0.1 :a 0.0 :d 0.2 :s 0.4 :r .5 :dur 1/8]
             :synth-fn (f(s e) (when (and s e) (let ((f (getf e :freq)))
                                                 [(apply #'synth (cons s e))
                                                  (apply #'synth (cons s (mergeplist e [:freq (* f 1.5) :amp 0.07])))])))
             :note-fn (f_ [:freq (midicps (+ 54 12 (sc *pentatonic* _)))]))
  (λ(i) (sim nil
             (per-beat i
                       (seq (euclidian 3 8 0 5))
                       (seq (euclidian 5 8 0 4))
                       (seq (euclidian 3 8 0 3))
                       (seq (euclidian 3 8 0 (rand-el 1 3 4 6)))
                  ))))

(solo :start)
(solo :stop)

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
             (seq nil 'hh)
             (per-beat i
                       (seq 'bd 'bd)
                       (seq 'bd 'bd)
                       (seq 'bd 'bd)
                       (seq 'bd 'bd)
                       (seq 'bd 'bd)
                       (seq 'bd (seq 'bd 'bd nil nil))
                       (seq 'bd 'bd)
                       (seq nil 'bd)
                       ))))

(drums :start)
(drums :stop)

;;;;;

(defsynth snare ((freq 60) (freq0 1600) (slide 0.02) (amp 0.2)
              (out 0) (gate 1)
              (a 0.001) (d 0.2))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> (sin-osc.ar fq)
              (* amp (env-gen.kr (perc a d)))
              (+ (* 1/2 (bpf.ar (white-noise.ar) fq)
                    (env-gen.ar (perc a (* d 4)) :act :free)))
              pan2.ar (out.ar out <>)))))

(defsynth bd ((freq 40) (freq0 400) (slide 0.04) (amp 0.7)
              (out 0) (gate 1)
              (a 0.1) (d 0.2))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> (sin-osc.ar fq)
              (* amp (env-gen.kr (perc a d) :act :free))
              pan2.ar (out.ar out <>)))))

(defsynth hh ((out 0) (amp 0.1) (d 0.1) (f 900))
  (-<> (pink-noise.ar)
       (bpf.ar f)
       (* amp (env-gen.kr (perc 0 d)))
       pan2.ar (out.ar out <>)))

(def sn ['snare :d 0.1 :freq0 300])
(def d1 ['hh :d 0.4 :f 7000])
(def d2 ['hh :d 0.1 :f 1900])

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
             (seq 'bd 'bd)
             (seq nil ['hh :d 1])
             (seq (rotate (euclidian 5 8 d1 d2) (random 7)))
             (per-beat i
                       (seq 'bd sn)
                       (seq (seq 'bd 'bd) nil sn (seq 'bd sn))
                       ))))

(drums :start)
(drums :stop)

(def df 220)

(proxy :drone
       (-<> 0
            (+ (* 1/4 (sin-osc.ar (* df 1)) (range (sin-osc.kr 0.1) 0.1 0.9)))
            (+ (* 1/4 1/2 (sin-osc.ar (* df 2)) (range (sin-osc.kr 0.2) 0.1 0.9)))
            (+ (* 1/4 1/3 (sin-osc.ar (* df 3)) (range (sin-osc.kr 0.3) 0.1 0.9)))
            (+ (* 1/4 1/4 (sin-osc.ar (* df 4)) (range (sin-osc.kr 0.4) 0.1 0.9)))
            (bpf.ar (range (sin-osc.kr 1) 100 2000))
            (* 0.2)
            pan2.ar))

(release :drone)

(def depth-cb (bus-control))

(proxy :depth-cb
  (-<> (sin-osc.kr 0.1)
       (range 90 500)
       (out.kr depth-cb <>)
       ))

(release :depth-cb)

(defsynth fm-bass ((freq 220) (amp 0.2) (out 0) (depth 400) (q 2) (gate 1)
                             (a 0.0) (d 0.2) (s 0.5) (r 0.5))
  (-<> (sin-osc.ar freq) (* (in.kr depth-cb)) (+ (* q freq))
       (sin-osc.ar)
       (* amp (env-gen.kr (adsr a d s r) :act :free :gate gate))
       pan2.ar (out.ar out <>)))

(defpattern bass :inf
  (play-note 'fm-bass
             :attr [:amp 0.141]
             :note-fn (f_ [:freq (midicps (+ 30 (sc *pentatonic* _)))]))
  (λ(i) (sim nil
             (seq (rotate (euclidian 3 8 0 5) (random 4)))
             (per-beat i
                  (seq 0 0 nil 2)
                  (seq 3 4 -1)
                  ))))

(bass :start)

(bass :stop)
(drums :stop)

(->> (loop :for i :from 0 :to 9 :collect i) (mapcar (curry #'sc *pentatonic*)))

(defsynth ssin ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> (sin-osc.ar fq)
              (* (sin-osc.ar (+ 2 fq)))
              (clip2 0.01)
              (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
              pan2.ar (out.ar out <>)))))

(def sq1 (loop :for i :from 2 :to 5 :collect (sc *pentatonic* i)))

(defpattern ssin :inf
  (play-note 'ssin
             :attr []
             :note-fn (f_ [:freq (midicps (+ 30 (sc *pentatonic* _)))]))
  (λ(i) (sim nil
                  (seq sq1)
                  ;(seq (rotate (copy-list sq1) 3))
                  )))

(ssin :start)
(ssin :stop)

;;;;;

(proxy :test
       (-<> [(saw.ar 440) (saw.ar 441)]
            (* 0.5 (env-gen.kr (perc 0.0 0.5) :act :free))
            )
       )

(release :test)

(defsynth bd ((freq 40) (freq0 540) (slide 0.1) (amp 0.3)
              (out 0) (gate 1)
              (a 0.0) (d 0.2) (s 0.7) (r 0.5))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> (sin-osc.ar fq)
              (* amp (env-gen.kr (linen a 1/3 d) :gate gate :act :free))
              pan2.ar (out.ar out <>)))))

(synth 'bd)

(defsynth hh ((out 0) (amp 0.3) (d 0.1))
  (-<> (white-noise.ar)
       (hpf.ar 8000)
       (* amp (env-gen.kr (perc 0.0 d) :act :free))
       pan2.ar (out.ar out <>)
    ))

(defsynth snare ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.0) (d 0.2))
  (-<> (white-noise.ar)
       (lpf.ar 880)
       (* amp (env-gen.kr (perc a d) :gate gate :act :free))
       pan2.ar (out.ar out <>)))

(synth 'snare :d 0.4)

(synth 'hh)

(synth 'snare)

(release :test)

(def ff (midicps 54))

(proxy :drone
       (-<> (* [(sin-osc.ar ff) (sin-osc.ar (+ ff 2))] (range (sin-osc.kr 0.1) 0.2 0.9))
            (+ (* 1/2 [(sin-osc.ar (* 2 ff)) (sin-osc.ar (+ (* 2 ff) 1))] (range (sin-osc.kr 0.1) 0.2 0.9)))
            (+ (* 1/3 [(sin-osc.ar (* 3 ff)) (sin-osc.ar (+ (* 3 ff) 1))] (range (sin-osc.kr 0.11) 0.2 0.9)))
            (+ (* 1/4 [(sin-osc.ar (* 4 ff)) (sin-osc.ar (+ (* 4 ff) 1))] (range (sin-osc.kr 0.12) 0.2 0.9)))
            ;(+ (* 1/5 [(sin-osc.ar (* 5 ff)) (sin-osc.ar (+ (* 5 ff) 1))] (range (sin-osc.kr 0.13) 0.2 0.9)))
            ;(+ (* 1/7 [(sin-osc.ar (* 6 ff)) (sin-osc.ar (+ (* 8 ff) 1))] (range (sin-osc.kr 0.14) 0.2 0.9)))
            (* (sin-osc.kr 10))
            (* 0.121)))

(release :drone)

(def bass-cb (bus-control))

(control-set (busnum bass-cb) 1)

(proxy :bass
       (-<> [(sin-osc.ar (midicps 30)) (+ (* 1/2 (sin-osc.ar (+ 1 (midicps 30)))))]
            (* 0.4 (in.kr bass-cb))
            ))

(defsynth bass-peak ((gate 1))
  (out.kr bass-cb (env-gen.kr (adsr 0.5 0 1 0.5) :act :free :gate gate) ))

(defpattern bass-alt :inf
  (play-note 'bass-peak)
  (λ(i) (per-beat i
                  (seq 1 nil 1 nil 1 nil 1 nil)
                  )))

(bass-alt :start)
(bass-alt :stop)

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
             ;(seq nil ['snare :amp 0.7])
             ;(seq (rotate (euclidian 3 8 ['hh :amp (/ 1 (+ 4 (random 8)))] ['hh :d 0.3] ) (random 4)))
             (per-beat i
                       (seq 'bd 'bd)
                       (seq 'bd 'bd)
                       (seq 'bd 'bd)
                       (seq 'bd (seq 'bd 'bd nil nil))
                       ))))

(drums :start)
(drums :stop)

(defsynth ssaw ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.0014) (d 0.2) (s 0.2) (r 1.5))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> [(saw.ar fq) (saw.ar (+ 2 fq))]
              (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
              (out.ar out <>)))))

(bpm 50)

(defpattern bass :inf
  (play-note 'fm-bass
             :attr [:amp 0.11 :depth 600 :q 4]
             :note-fn (f_ [:freq (midicps (+ 54 -12 (sc *pentatonic* _)))]))
  (λ(i) (per-beat i
                  (seq (rotate (euclidian 5 8 0 5) (random 3)))
                  (seq (rotate (euclidian 3 8 0 5) (random 3)))
                  (seq (rotate (euclidian 3 8 0 4) (random 3)))
                  (seq (rotate (euclidian 5 8 0 3) (random 3)))
                  )))

(bass :start)
(bass :stop)

(def fadeout (bus-audio :chanls 2))

(proxy :fadeout
  (-<> (in.ar fadeout 2)
       (* (line.kr 1 0 20))
       ))

(defpattern ssaw :inf
  (play-note 'ssaw
             :attr [:out ab1]
             :note-fn (f_ [:freq (midicps (+ 54 (sc *pentatonic* _)))]))
  (λ(i) (sim nil
             (random 6)
             (per-beat i
                       ;(seq 6 7 6 5  3 4 2 5)
                       )
             (per-beat i
                  (seq -1 0 1 2)
                  (seq -2 -1 0 1)
                  ))))

(def ab1 (bus-audio :chanls 2))

(stop ssaw)

(proxy :ab1
  (-<> (in.ar ab1 2)
       (freeverb.ar :mix 0.8 :room 0.6)
       (bpf.ar (range (sin-osc.kr 0.1) 200 10000))
       (* (line.kr 1 0 10))
       ))

(ssaw :start)
(ssaw :stop)

(defpattern drums :inf
  (play-drum :out fadeout)
  (λ(i) (sim nil
             (seq (seq 'hh 'hh 'hh 'hh) 'snare)
             (per-beat i
                       (seq 'bd 'bd)
                       (seq 'bd (seq (loop :repeat (random 5) :collect 'bd)))
                       ))))

(drums :start)
(drums :stop)

(proxy :test (sin-osc.ar 440))

(release :test)

;;;;;;

(defun note (n) (midicps (+ 50 (sc *major* n))))

(defpattern test :inf
  (play-note 'ssin
             :attr []
             :note-fn (f_ [:freq (note _)]))
  (λ(i) (per-beat i
                  (sim 0 2 4 6) nil
                  (sim 1 3 5 7) nil
                  (sim 2 4 6 8) nil
                  (sim 3 5 7 9) nil
                  )))

(test :start)
(test :stop)

(synth 'ssin)

;;;;;;;;

(defpattern test :inf
  (play-note 'fm-bass
             :attr [:q 3 :depth 300]
             :note-fn (f_ [:freq (midicps (+ 54 -24 (sc *pentatonic* _)))]))
  (λ(i) (per-beat i
                  (seq -1 0 1)
                  (seq nil 2 3 nil)
                  (seq -1 0 1)
                  (seq 2 3 4 5)
                  )))

(test :start)
(test :stop)

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
             (per-beat i
                       (seq 'bd 'bd)))))

(drums :start)
(drums :stop)

(def ab1 (bus-audio :chanls 2))

(proxy :ab1
  (-<> (in.ar ab1 2)
       ;(* 10)
       ;[(clip2 (nth 0 <>) 1) (clip2 (nth 1 <>) 1/2)]
       ;(* 0.3)
       ;(decimator.ar 4100 5)
       (+ <>
          (* 1/2 (delay-n.ar <> 0.4))
          (* 1/3 (delay-n.ar <> 0.8))
          ;(* 1/4 (delay-n.ar <> 0.12))
          ;(* 1/8 (delay-n.ar <> 0.16))
          )
       (* 1/3)
       ))

(defpattern ssin :inf
  (play-note 'ssin
             :attr [:out ab1]
             :note-fn (f_ [:freq (midicps (+ 54 12 (sc *pentatonic* _)))]))
  (λ(i) (per-beat i
                  (seq (seq 3 2 1) (seq -1 0 -2))
                  nil
                  (seq (seq 3 2 1) (seq -1 0 -2))
                  (seq -1 0 1)
                  )))

(ssin :start)
(ssin :stop)

;;;;

(def ab1 (bus-audio :chanls 2))

(proxy :ab1
  (-<> (in.ar ab1 2)
       (bpf.ar (range (sin-osc.kr 0.07) 300 6000))
       ))

(defsynth ssq
  ((freq 440) (freq0 440) (slide 0) (amp 0.3)
   (out 0) (gate 1)
   (a 0.001) (d 0.2) (s 0.7) (r 0.9))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> (saw.ar fq)
              (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
              pan2.ar (out.ar out <>)))))

(defpattern ssq :inf
  (play-note 'ssq
             :attr [:dur 1/5 :out ab1]
             :note-fn (f_ [:freq (midicps (+ 54 -12 (sc *pentatonic* _)))]))
  (λ(i) (sim nil
           (per-beat i
                  (seq -1 0)
                  (seq 2 3 (seq 4 5) 7)
                  ))))

(ssq :start)
(ssq :stop)

;;;;

(def db1 (bus-audio :chanls 2))

(proxy :db1
  (-<> (in.ar db1 2)
       ))

(defpattern drums :inf
  (play-drum :amp 0.7 :out db1)
  (λ(i) (sim nil
             (per-beat i
                       (seq 'bd 'hh 'snare (seq 'hh 'bd))
                       (seq (per-beat i 'bd (seq 'bd 'bd) 'bd 'bd) 'hh 'snare 'hh)))))

(drums :start)
(drums :stop)

(def ab1 (bus-audio :chanls 2))

(proxy :ab1
  (-<> (in.ar ab1 2)
       (rlpf.ar (range (sin-osc.kr 0.1) 200 2000))
       ))

(defpattern bass :inf
  (play-note 'ssaw
             :attr [:amp 0.59 :dur 1/8 :depth 100 :q 2 :out ab1 :dur 1/4]
             :note-fn (f_ [:freq (midicps (+ 54 -24 (sc *pentatonic* _)))]))
  (λ(i) (sim nil
             (per-beat i
                  (seq 5 0 0 0 0 5 0 0)
                  (seq 5 0 0 0 0 4 0 0)
                  (seq 5 6 0 0 0 5 0 0)
                  (seq 5 1 2 0 0 4 0 6)
                  ))))

(bass :start)
(bass :stop)

(def ab2 (bus-audio :chanls 2))

(proxy :ab2
  (-<> (in.ar ab2 2)
       ;(+ <> (* 1/2 (delay-n.ar <> 0.2)) (* 1/4 (delay-n.ar <> 0.3)))
       (freeverb.ar :mix 0.4 :room 0.3 :damp 0.8)
       ))

(def ab2 (bus-audio :chanls 2))

(proxy :ab2
  (-<> (in.ar ab2 2)
       (+ <> (* 1/2 (delay-n.ar <> 0.3)))
       ;(freeverb.ar :room 0.7 :mix 0.7)
       (* (sin-osc.kr 8))
       ))

(defpattern ssin :inf
  (play-note 'ssin
             :attr [:amp 0.05 :dur 1/4 :out ab2]
             :note-fn (f_ [:freq (midicps (+ 54 24 (sc *pentatonic* _)))]))
  (λ(i) (sim nil
           (per-beat i
                     (seq (euclidian 3 8 0 -2))
                     (seq (euclidian 5 8 0 -2))
                     (seq (euclidian 3 8 0 -2))
                     (seq (euclidian 3 8 0 2))
                  ))))

(ssin :start)
(ssin :stop)

(def fade (bus-audio :chanls 2))

(proxy :fade
  (-<> (in.ar fade 2)
       (* (line.kr 1 0 10))
       ))

(ssin :start)
(ssin :stop)

(ssin :stop)
(bass :stop)
(drums :stop)
;;;;

(bpm 50)

(defsynth sss ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out fade) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> (white-noise.ar)
              (bpf.ar fq 0.4)
              (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
              pan2.ar (out.ar out <>)))))

(defpattern sss :inf
  (play-note 'sss
             :attr [:dur 1/9 :a 0.001 :r 0.2 :s 0.3]
             :note-fn (f_ [:freq (midicps (+ 54 (sc *pentatonic* _)))]))
  (λ(i) (per-beat i
                  (seq 0 2 4 6)
                  (seq (loop :repeat 5 :collect (- (random 20) 10)))
                  (seq -1 2 2 4 -12 -10)
                  (seq (loop :for i :from 0 :to 20 :collect i))
                  )))

(sss :start)
(sss :stop)

(defpattern drums :inf
  (play-drum :amp 0.5)
  (λ(i) (sim nil
             ;(once-every i 4 0 (seq (loop :for i :from 20 :downto 1 :collect ['bd :amp (/ 1 i)])))
             (seq 'bd 'bd)
             (per-beat i
                       (seq 'bd 'snare)
                       ))))

(drums :start)
(drums :stop)

(def ab1 (bus-audio :chanls 2))

(proxy :ab1
  (-<> (in.ar ab1 2)
       (rlpf.ar 200)
       ))

(defpattern fmbass :inf
  (play-note 'saw
             :attr [:amp 0.121 :dur 1/8 :q 4 :depth 100 :out ab1]
             :note-fn (f_ [:freq (midicps (+ 54 (sc *pentatonic* _)))]))
  (λ(i) (sim nil
             (seq 0 (sim 1 nil -1) 2 (sim 3 5))
             (seq 3 2 0 1)
             )))

(fmbass :start)
(fmbass :stop)

(defsynth saw ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> (saw.ar fq)
              (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
              pan2.ar (out.ar out <>)))))

(synth 'hh)

(def f (midicps 30))
(proxy :drone
       (-<> (* (sin-osc.ar f) (range (sin-osc.kr 23.34) 0.6 0.9))
            ;(+ (* 1/2 (sin-osc.ar (* f 2)) (range (sin-osc.kr 0.1) 0.2 1)))
            ;(+ (* 1/3 (sin-osc.ar (* f 3))))
            ;(+ (* 1/4 (sin-osc.ar (* f 4)) (range (sin-osc.kr 0.2) 0.2 1)))
            ;(+ (* 1/5 (sin-osc.ar (* f 5)) (range (sin-osc.kr 0.3) 0.2 1)))
            ;(+ (* 1/6 (sin-osc.ar (* f 6)) (range (sin-osc.kr 0.4) 0.2 1)))
            (+ (* 1/7 (sin-osc.ar (* f 7)) (range (sin-osc.kr 0.5) 0.2 1)))
            (* 1/3)
            pan2.ar))

(defsynth rum ((freq 440) (freq0 440) (slide 0) (amp 0.3)
                          (wf 1)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> (sin-osc.ar fq)
              (+ (* 1/2 (sin-osc.ar (* fq 2)) (range (sin-osc.kr (* wf 1)) 0.2 1)))
              (+ (* 1/4 (sin-osc.ar (* fq 4)) (range (sin-osc.kr (* wf 2)) 0.2 1)))
              (+ (* 1/5 (sin-osc.ar (* fq 5)) (range (sin-osc.kr (* wf 3)) 0.2 1)))
              (+ (* 1/6 (sin-osc.ar (* fq 6)) (range (sin-osc.kr (* wf 4)) 0.2 1)))
              (* amp (env-gen.kr (adsr a d s r) :act :free :gate gate))
              pan2.ar (out.ar out <>)))))

(defpattern rum :inf
  (play-note 'rum
             :attr [:amp 1 :wf 1 :a 1 :d 0 :s 1 :r 1 :depth 100 :q 1]
             :note-fn (f_ [:freq (midicps (+ 54 -24 (sc *pentatonic* _)))]))
  (λ(i) (sim nil
           (per-beat i
                  (seq -1 [0 :dur 2] nil 1)
                  ))))

(rum :start)
(rum :stop)

(release :s)

(def ab1 (bus-audio :chanls 2))

(proxy :ab1
  (-<> (in.ar ab1 2)
       ;(* 4)
       ;(clip -0.5 0.5)
       ;(* 1/4)
       ;(rlpf.ar (range (sin-osc.kr 0.1) 150 1000))
       ))

(defpattern bass :inf
  (play-note 'fm-bass
             :attr [:amp 0 :depth 250 :q 1 :dur 1/2 :out ab1]
             :note-fn (f_ [:freq (midicps (+ 30 (sc *pentatonic* _)))]))
  (λ(i) (sim nil
             (seq 0 1 2 3)
             (seq (euclidian 3 8 [10 :slide 1/20 :freq0 20] [5 :slide 1/22 :freq0 20]))
             (per-beat i
                  ;(seq 0 0 0 0)
                  ))))

(bass :start)
(bass :stop)

(defpattern drums :inf
  (play-drum :amp 0.7)
  (λ(i) (sim nil
             (per-beat i
                       (seq ['bd :amp 1/4] 'hh)
                       )
             )))

(drums :start)
(drums :stop)

(defpattern bass :inf
  (play-note 'fm-bass
             :attr [:amp 1/5 :dur 1/4]
             :note-fn (f_ [:freq (midicps (+ 54 -24 (sc *pentatonic* _)))]))
  (λ(i) (per-beat i
                  (seq -1 0 1 2)
                  )))

(bass :start)
(bass :stop)

;;;;

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
             (seq 'bd 'bd)
             (per-beat i
                       (seq 'click)))))

(def f (midicps 30))
(proxy :drone
       (-> (* 1/2 (sin-osc.ar (* f 1)) (range (sin-osc.kr 0.1 0)    .4 .9))
           (+ (* 1/4 (sin-osc.ar (* f 4)) (range (sin-osc.kr 0.1 (* 1/4 pi)) .4 .9)))
           (+ (* 1/6 (sin-osc.ar (* f 6)) (range (sin-osc.kr 0.1 (* 2/4 pi)) .4 .9)))
           (+ (* 1/8 (sin-osc.ar (* f 8)) (range (sin-osc.kr 0.1 (* 3/4 pi)) .4 .9)))
           (+ (* 1/9 (sin-osc.ar (* f 10)) (range (sin-osc.kr 0.2 (* 4/4 pi)) .4 .9)))
           (freeverb.ar :mix 0.6 :room 0.7)
           pan2.ar
           ))

(release :drone)

(proxy :delaytest
       (-<> (sin-osc.ar 220)
            (* (pulse.kr 1))
            (+ <> (* 1/2 (comb-l.ar <> 0.6 0.2 1)))
            pan2.ar
            ))

(release :delaytest)

(defsynth ding ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> (sin-osc.ar fq)
              (* amp (env-gen.kr (perc 0.01 1) :act :free))
              pan2.ar (out.ar out <>)))))

(def ab1 (bus-audio :chanls 2))

(proxy :ab1
  (-<> (in.ar ab1 2)
       (+ <> (* 1/2 (comb-l.ar <> 1/3 1/3 3)))
       ))

(defpattern ding :inf
  (play-note 'ding
             :attr [:out ab1]
             :note-fn (f_ [:freq (midicps (+ 54 (sc *pentatonic* _)))]))
  (λ(i) (per-beat i
                  (seq -1 5 1 2)
                  (seq -1 7 3)
                  )))

(ding :start)
(ding :stop)

(def b ['bd :tm 0.05])
(def s ['snare :freq 3000 :amp 0.1])

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
             (seq nil ['snare :freq 8000 :d 0.1])
             (seq (euclidian 3 8 ['hh :amp 0.7] ['hh :amp 0.2]))
             (per-beat i
                       ))))

(drums :start)
(drums :stop)

(def bass-bus (bus-audio :chanls 2))

(proxy :bass-bus
  (-<> (in.ar bass-bus 2)
       (* (line.kr 0 1 4))
       ))

(bpm 30)

(defpattern bass :inf
  (play-note 'ssin
             :attr [:amp 0.301 :q 1 :depth 100
                         :dur 0.8
                         :a 0.54 :d 0.5 :s 0.4 :r 3]
             :note-fn (f_ [:freq (midicps (+ 54 (sc *minor* _)))]))
  (λ(i) (sim nil
           (per-beat i
                  (sim 0 4)
                  (sim 4 8)
                  (sim 2 6)
                  (sim 3 7)
                  (sim 2 4)
                  (sim 4 8)
                  (sim 3 6)
                  (sim 1 10)
                  ))))

(bass :start)
(bass :stop)


(def ab1 (bus-audio :chanls 2))

(proxy :ab1
  (-<> (in.ar ab1 2)
       (+ <> (* 1/4 (comb-l.ar <> 1 0.3 1/2)))
       ))

(defsynth ssin1 ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 0.5))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> (sin-osc.ar fq)
              (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
              pan2.ar (out.ar out <>)))))

(def s1 (gen-list [0 2 3 4 7 8 2 5 3 4]))

(defpattern ssin :inf
  (play-note 'ssin1
             :attr [:out ab1 :a 0.002 :d 0.01 :s 0.3 :r 0.5]
             :note-fn (f_ [:freq (midicps (+ 54 (sc *pentatonic* _)))]))
  (λ(i) (per-beat i
                  (seq (loop :for i :from 0 :to 7 :collect (funcall s1 i)))
                  (seq (loop :repeat 8 :collect (funcall s1)))
                  )))

(ssin :start)
(ssin :stop)

(proxy :bass
       (-<> (sin-osc.ar (midicps (+ 54 -24)))
            (clip2 (range (sin-osc.kr 0.07 0) 0.2 1))
            (* 0.4)
            pan2.ar))

(release :bass)

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
             (seq nil 'snare)
             (per-beat i
                       (seq 'bd 'bd)))))

(drums :start)
(drums :stop)

(defpattern bbs :inf
  (play-note 'ssin
             :attr [:a 0.001 :d 0.1 :s 0.4 :r 0.5]
             )
  (λ(i) (per-beat i
                  (seq 42 42 42 42)
                  )))

(bbs :start)
(bbs :stop)

;;;;

(def s1 (gen-list [0 1 3 4]))

(defpattern ssin :inf
  (play-note 'ssin
             :attr [:amp 0.4]
             :note-fn (f_ [:freq (midicps (+ 54 (sc *minor* _)))]))
  (λ(i) (sim nil
             (seq (loop :repeat 2 :collect [-7 :dur 1/3]))
             (per-beat i
                  (seq (loop :repeat 6 :collect (funcall s1)))
                  ))))

(defun fq (n) (midicps (+ 54 (sc *minor* n))))

(proxy :aa
       (-> (saw.ar (fq -7))
           ;(* (range (sin-osc.kr (line.kr 1 40 10)) 0.1 0.9))
           (rlpf.ar (range (sin-osc.kr 0.1) 200 7000))
           (* 0.01)
           ;(* (line.kr 0 0.3 4))
           pan2.ar))

(ssin :start)
(ssin :stop)

(def s2 (gen-list (mapcar (f_ (- _ 14)) [0 7 7])))

(defpattern bss :inf
  (play-note 'ssaw
             :attr [:amp 0.2]
             :note-fn (f_ [:freq (fq _)]))
  (λ(i) (per-beat i
                  (seq (funcall s2 0) (funcall s2) (funcall s2))
                  (seq (loop :repeat 6 :collect (funcall s2)))
                  )))

(bss :start)
(bss :stop)

(let ((i 0)
      (sq [0 2 3 4]))
  (defun ssq ()
    (prog1 (nth (mod i (length sq)) sq)
      (setf i (1+ i)))))

(ssq)

;;;;


(def x (gen-rand [2 3 4 6 7 2]))
(def x2 (gen-list [7 3 2 1]))

(defpattern ssin :inf
  (play-note 'fm-bass
             :attr [:dur 1/4 :q 3 :depth 40 :out ab1 :amp 0.4]
             :note-fn (f_ [:freq (midicps (+ 54 (sc *pentatonic* _)))]))
  (λ(i) (sim nil
           ;(funcall x2)
           (per-beat i
                  (let ((q (cons 0 (loop :repeat 3 :collect (funcall x)))))
                    (sim (seq q)
                         (seq (mapcar (f_ [(+ _ 3) :amp 0]) q)))))
                  )))

(ssin :start)
(ssin :stop)

(def ab1 (bus-audio :chanls 2))

(proxy :ab1
  (-<> (in.ar ab1 2)
       (+ <> (* 1/5 (comb-l.ar <> 0.2 0.2 0.5)))
       ;(* (line.kr 1 0 5))
       ))

(defpattern bass :inf
  (play-note 'fm-bass
             :attr [:q 1 :depth 100 :amp 0.7 :out echo
                       :a 0.01 :d 0.01 :s 0.6 :r 0.7]
             :note-fn (f_ [:freq (midicps (+ 54 -24 (sc *pentatonic* _)))]))
  (λ(i) (sim nil
             (per-beat i
                       (seq (mapcar (f_ [_ :dur 1/2]) (euclidian 3 8 0 5)))
                       (seq (mapcar (f_ [_ :dur 1/8]) (euclidian 3 8 0 5)))
                       (seq (mapcar (f_ [_ :dur 1/2]) (euclidian 3 8 0 5)))
                       (seq (mapcar (f_ [_ :dur 1/8]) (euclidian 3 8 0 5)))
                       (seq (mapcar (f_ [_ :dur 1/2]) (euclidian 3 8 0 4)))
                       (seq (mapcar (f_ [_ :dur 1/8]) (euclidian 3 8 0 4)))
                       (seq (mapcar (f_ [_ :dur 1/2]) (euclidian 3 8 0 3)))
                       (seq (mapcar (f_ [_ :dur 1/8]) (euclidian 3 8 0 3)))
                  ;(seq [0 :dur 2 :amp 0.7] nil nil [0 :dur 1/4 :amp 0.5])
                  ))))

(bass :start)
(bass :stop)

(def echo (bus-audio :chanls 2))

(proxy :echo
  (-<> (in.ar echo 2)
       ;(freeverb.ar :mix 0.6 :room 0.7)
       ))

(defun fq (n) (midicps (+ 54 (sc *pentatonic* n))))
(proxy :drone
       (-<> (sin-osc.ar (fq -5))
            (+ (sin-osc.ar (fq -2)))
            (+ (* 1/3 (sin-osc.ar (fq 0))))
            (* 1/8 (line.kr 0 1 5))
            pan2.ar))

(release :drone)

(defpattern drums :inf
  (play-drum :freq 1200 :tm 0.02 :amp 0.6 :bass 30)
  (λ(i) (sim nil
             (seq nil 'snare)
             (per-beat i
                       (seq 'bd 'bd)
                       (seq 'bd nil 'bd (seq nil 'bd))
                       ))))

(drums :start)

(drums :stop)

(ssin :stop)
(bass :stop)
(release :drone)

;;;;

(def g (gen-list (loop :for i :from 0 :to 12 :collect i)))
(def r (gen-rand [0 2 3 5]))

(def prev 0)

(defun nt (i) (midicps (+ 54 (sc *pentatonic* i))))

(defpattern g1 :inf
  (play-note 'fm-bass
             :attr [:amp 0.2 :q 1 :depth 100 :out 0]
             :note-fn (f_ [:freq (nt _)]))
  (λ(i) (sim nil
             (per-beat i
                       (seq [[-5 :d 1/2 :s 0.1 :amp 0.6]])
                       (sim (seq [[-2 :d 1/2 :s 0.1 :amp 0.4]])
                            (seq [[0 :d 1/2 :s 0.1 :amp 0.4]]))
                       (seq [[-6 :d 1/2 :s 0.1 :amp 0.6]])
                       (seq [[-5 :d 1/2 :s 0.1 :amp 0.6]])
                       )
             (let ((n (funcall r)))
               (prog1 (seq [[n :freq0 prev :slide 1/6 :a 0.3 :d 1 :s 0.1]])
                 (setf prev (nt n))))
             ;(seq (loop :for i :from 10 :downto 3 :collect i))
             (per-beat i
                       ;(seq (loop :for i :from 3 :to 10 :collect i))
                       ))))

(g1 :start)
(g1 :stop)

(defpattern drums :inf
  (play-drum)
  (λ(i) (sim nil
             (seq (euclidian 3 8 'bd 'hh))
             (per-beat i
                       (seq 'click)))))

(drums :start)
(drums :stop)

;;;;

(defun fq (i)
  (midicps (+ 54 (sc *pentatonic* i))))

(def ab1 (bus-audio :chanls 2))

(proxy :ab1
  (-<> (in.ar ab1 2)
       (+ <> (* 1/5 (comb-l.ar <> 0.3 0.3 0.5)))
       (rlpf.ar (range (sin-osc.kr 0.2) 90 700))
       (freeverb.ar :mix 0.6 :room 0.3)
       ))

(defpattern bass :inf
  (play-note 'fm-bass
             :attr [:amp 0.5 :q 1 :depth 200 :dur 1/3 :out ab1]
             :note-fn (f_ [:freq (fq (+ -10 _))]))
  (λ(i) (per-beat i
                  (seq (cons [0 :amp 1/4] (loop :repeat 3 :collect [0 :amp (/ 1 (+ 6 (random 8)))])))
                  )))

(bass :start)
(bass :stop)

(def r1 (gen-xrand [
                    [0 2 3 4]
                    [1 2 5 4]
                    [0 3]
                    [0 1 3]
                    ;(loop :for i :from 0 :to 10 :collect i)
                    ]))

(defpattern ssin :inf
  (play-note 'fm-bass
             :attr [:amp 0.2 :q 1 :depth 130
                         :a 0.001 :d 0.3 :s 0.1 :r 0.9]
             :note-fn (λ(i) [:freq (fq i)]))
  (λ(i) (per-beat i
                  ;(seq (funcall r1))
                  (seq (funcall r1))
                  )))

(ssin :start)
(ssin :stop)

(defpattern drums :inf
  (play-drum :amp 0.1 :freq 900 :tm 0.01 :d 0)
  (λ(i) (sim nil
             (per-beat i
                       (seq 'bd ['bd :amp 0.01] ['bd :amp 0.009] ['bd :amp 0.006])))))

(drums :start)
(drums :stop)

;;;;

(def ssn-bus (bus-audio :chanls 2))

(defpattern ssn :inf
  (play-note 'ssin
             :attr [:out ssn-bus]
             :note-fn (f_ [:freq (midicps (+ 54 (sc *pentatonic* _)))]))
  (λ(i)
    (sim nil
         (per-beat i
                   (seq -1 0 1 2)
                   ))))

(ssn :start)
(ssn :stop)

;; Mixer

(proxy :mixer
  (-<> (* 1/8 (in.ar ssn-bus 2))))

;;;;

;;;;;

(stop)
