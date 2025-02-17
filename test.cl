(load "clseqs/all.lisp")
(use-package :clseqs)
(use-package :sc)
(sc-init)
(clock-bpm 60)
(metro-start)
(named-readtables:in-readtable :sc)
(init-synths)
;
(def _ nil)

(synth 'metronome :freq 880)
(sleep 1/2)
(synth 'metronome :freq 440)
(sleep 1/2)
(synth 'metronome :freq 440)
(sleep 1/2)
(synth 'metronome :freq 440)

(metro-add
  :metro
  (λ(i)
    (SA [:fn (m-play-synth 'metronome :release nil)]
        (per-beat i (S 82 70) (S 70 70)))))

(metro-add :metro)

(metro-add
  :track
  (m-tracker
    []
    [(m-play-drum)   (m-play-synth 'fm-bass :attr [:amp 1/10 :q 1 :depth 190])]
     (S 'bd 'hh)     (U C4 E4 G4 C2)
     (S 'snare (S _ _ 'hh 'hh)) 
                     (U D4 G4 B1)
     (S (U 'bd 'hh) (S 'bd 'hh))
                     (U E4 A4 A1)
     (S 'snare (S _ (SA [:amp 0.001] 'hh) 'hh 'hh))
                     (U F4 A4 C4 F2)))

(metro-add
  :track
  (SA [:fn (m-play-drum) :attr [:amp 0.001]]
      ['hh :amp 0.7]))

(metro-add :track)

(metro-add
  :drum
  (SAL [:fn (m-play-drum)]
       (euclidian 3 8 'bd)))

(metro-add :snare (SA [:fn (m-play-drum)]  _ _ 'hh 'snare))

(metro-add :drum)
(metro-add :snare)

(synth 'bd)

(proxy :echo
  (let* ((delay 0.03)
         (decay 0.5)
         (sig (sound-in.ar 0))
         (local (+ (local-in.ar 2) (dup sig))))
    (loop :repeat 1 :do (setf local (allpass-l.ar local delay delay 1)))
    (local-out.ar (* local decay))
    (out.ar 0 local)))

(proxy :echo nil)

;; with external server
(load "clseqs/all.lisp")
(use-package :sc)
(use-package :clseqs)
;(sc-connect)
(sc-init)
(init-synths)

(synth 'clap)

(metro-add
  :clap
  (λ(i)
    (SA [:fn (m-play-drum)]
        (per-beat i
                  (S (U 'bd 'cl) 'hh 'hh 'hh 'hh 'hh 'hh (U 'hh (per-beat-n 4 i 'bd _)))
                  (S (U 'bd 'cl) 'hh 'hh 'clap 'hh (U 'bd 'hh) 'cl 'hh)))))

(stop)
