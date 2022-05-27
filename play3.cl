(load "~/src/mus/lib.cl")
(in-package :sc-user)
(ql:quickload :ltk)
(use-package :sc-extensions)
(use-package :bdef)
(named-readtables:in-readtable :sc)

(connect)   ;; start new server
(bpm 60)

;;;;

(def guitar1-buf (buffer-read "/home/aerdman/src/mus/guitar1.wav"))
(def guitar2-buf (buffer-read "/home/aerdman/src/mus/guitar2.wav"))
(def bass-buf (buffer-read "/home/aerdman/src/mus/bass.wav"))

;; Drums
(def bd ['bd :freq 200 :bass 20 :dur 0.05])
(defpattern drums
  (play-drum)
  (λ(i) (seq bd 'hh 'snare 'hh)))

(drums :start 4)
(drums :stop)

(defpattern fun
  (λ(b d e)
    (when (= 0 (mod e 8))
      (dur b 8
           ['sample-1 :buffer guitar1-buf :rate 2 :amp 2]
           ;['sample-1 :buffer guitar2-buf :rate 1 :amp 1]
           ['sample-1 :buffer bass-buf]
           )))
  (λ(i) (writeln i) (seq i)))

(fun :start)
(fun :stop)

;;;;

(stop)
