(require "clseqs" "./clseqs/package.cl")
(require "clseqs-sc" "./clseqs/sc.cl")
(require "clseqs-util" "./clseqs/util.cl")
(require "clseqs-sequences" "./clseqs/sequences.cl")
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
    [:quant 4]
    [(m-play-drum)   (m-play-synth 'ssin :attr [:amp 1/10])]
     'bd             (S C4 D4)
     'snare          (S E4 F4)
     'hh             (S G4 A4)
     'snare          (S B4 C5)
    ))

(metro-add :track)

(synth 'bd)
