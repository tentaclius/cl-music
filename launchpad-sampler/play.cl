(ql:quickload :arrow-macros)
(load "../clseqs/all.lisp")
(load "./launchpad-samples.cl")
(use-package :clseqs)
(use-package :sc)
(use-package :arrow-macros)
(import '(cl-launchpad:midi-init cl-launchpad:pad-map cl-launchpad:make-pad cl-launchpad:pad-map-reset cl-launchpad:pad-map-redraw))
(sc-init)
(clock-bpm 60)
(metro-start)
(named-readtables:in-readtable :sc)
(init-synths)
(midi-init)
(def _ nil)

; 36 37 38 39 68 69 70 71
; 40 41 42 43 72 73 74 75
; 44 45 46 47 76 77 78 79
; 48 49 50 51 80 81 82 83
; 52 53 54 55 84 85 86 87
; 56 57 58 59 88 89 90 91
; 60 61 62 63 92 93 94 95
; 64 65 66 67 96 97 98 99

(pad-map (list (make-pad :note 1 :file "~/Mus/Samples/selection/HatClosed-Med.wav")))
