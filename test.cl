(load "clseqs/all.lisp")
;(require :asdf)
;(push #P"/home/aerdman/src/cl-music/clseqs/" asdf:*central-registry*)
;(asdf:load-system :clseqs)
;
(use-package :clseqs)
(use-package :sc)
(sc-init)
(clock-bpm 60)
(metro-start)
(named-readtables:in-readtable :sc)
(init-synths)
;
(def _ nil)

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

(synth 'bd)

(metro-add
  :drum
  (SAL [:fn (m-play-drum)]
       (euclidian 3 8 'bd)))

(metro-add :snare (SA [:fn (m-play-drum)]  _ _ 'hh 'snare))

(metro-add :drum)
(metro-add :snare)
