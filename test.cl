(require "clseqs" "./clseqs/package.cl")
(require "clseqs-sc" "./clseqs/sc.cl")
(require "clseqs-util" "./clseqs/util.cl")
(require "clseqs-sequences" "./clseqs/sequences.cl")
(require "clseqs-mus" "./clseqs/mus.cl")
(use-package :clseqs)
(use-package :sc)
(sc-init)
(clock-bpm 60)
(metro-start)
(named-readtables:in-readtable :sc)
(init-synths)

(def _ nil)

(metro-add
  :track
  (m-tracker
    []
    [(m-play-drum)   (m-play-synth 'fm-bass :attr [:amp 1/10 :q 1 :depth 190])]
     (S 'bd 'hh)     (U C4 E4 G4)
     (S 'snare
        (S _ _ 'hh 'hh)) 
                     (U D4 G4)
     (S (U 'bd 'hh)
        (S 'bd 'hh))
                     (U E4 A4)
     (S 'snare (S _ (SA [:amp 0.001] 'hh) 'hh 'hh))
                     (U F4 C5)
    ))

(metro-add
  :track
  (SA [:fn (m-play-drum) :attr [:amp 0.001]]
      ['hh :amp 0.7]))

(metro-add :track)

(synth 'bd)
