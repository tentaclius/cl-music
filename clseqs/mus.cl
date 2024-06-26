(require "clseqs-util")
(provide "clseqs-mus")
(in-package #:clseqs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MIDI

;; midi notes
(def
  C-0 12    C0  12
  C#0 13    Db0 13
  D-0 14    D0  14
  D#0 15    Eb0 15
  E-0 16    E0  16
  F-0 17    F0  17
  F#0 18    Gb0 18
  G-0 19    G0  19
  G#0 20    Ab0 20
  A-0 21    A0  21
  A#0 22    Bb0 22
  B-0 23    B0  23
  ;
  C-1 24    C1  24
  C#1 25    Db1 25
  D-1 26    D1  26
  D#1 27    Eb1 27
  E-1 28    E1  28
  F-1 29    F1  29
  F#1 30    Gb1 30
  G-1 31    G1  31
  G#1 32    Ab1 32
  A-1 33    A1  33
  A#1 34    Bb1 34
  B-1 35    B1  35
  ;
  C-2 36    C2  36
  C#2 37    Db2 37
  D-2 38    D2  38
  D#2 39    Eb2 39
  E-2 40    E2  40
  F-2 41    F2  41
  F#2 42    Gb2 42
  G-2 43    G2  43
  G#2 44    Ab2 44
  A-2 45    A2  45
  A#2 46    Bb2 46
  B-2 47    B2  47
  ;
  C-3 48    C3  48
  C#3 49    Db3 49
  D-3 50    D3  50
  D#3 51    Eb3 51
  E-3 52    E3  52
  F-3 53    F3  53
  F#3 54    Gb3 54
  G-3 55    G3  55
  G#3 56    Ab3 56
  A-3 57    A3  57
  A#3 58    Bb3 58
  B-3 59    B3  59
  ;
  C-4 60    C4  60
  C#4 61    Db4 61
  D-4 62    D4  62
  D#4 63    Eb4 63
  E-4 64    E4  64
  F-4 65    F4  65
  F#4 66    Gb4 66
  G-4 67    G4  67
  G#4 68    Ab4 68
  A-4 69    A4  69
  A#4 70    Bb4 70
  B-4 71    B4  71
  ;
  C-5 72    C5  72
  C#5 73    Db5 73
  D-5 74    D5  74
  D#5 75    Eb5 75
  E-5 76    E5  76
  F-5 77    F5  77
  F#5 78    Gb5 78
  G-5 79    G5  79
  G#5 80    Ab5 80
  A-5 81    A5  81
  A#5 82    Bb5 82
  B-5 83    B5  83
  ;
  C-6 84    C6  84
  C#6 85    Db6 85
  D-6 86    D6  86
  D#6 87    Eb6 87
  E-6 88    E6  88
  F-6 89    F6  89
  F#6 90    Gb6 90
  G-6 91    G6  91
  G#6 92    Ab6 92
  A-6 93    A6  93
  A#6 94    Bb6 94
  B-6 95    B6  95
  ;
  C-7 96    C7  96
  C#7 97    Db7 97
  D-7 98    D7  98
  D#7 99    Eb7 99
  E-7 100   E7  100
  F-7 101   F7  101
  F#7 102   Gb7 102
  G-7 103   G7  103
  G#7 104   Ab7 104
  A-7 105   A7  105
  A#7 106   Bb7 106
  B-7 107   B7  107
  ;
  C-8 108   C8  108
  C#8 109   Db8 109
  D-8 110   D8  110
  D#8 111   Eb8 111
  E-8 112   E8  112
  F-8 113   F8  113
  F#8 114   Gb8 114
  G-8 115   G8  115
  G#8 116   Ab8 116
  A-8 117   A8  117
  A#8 118   Bb8 118
  B-8 119   B8  119
  ;
  C-9 120   C9  120
  C#9 121   Db9 121
  D-9 122   D9  122
  D#9 123   Eb9 123
  E-9 124   E9  124
  F-9 125   F9  125
  F#9 126   Gb9 126
  G-9 127   G9  127)

;; arturia minilab mkII knob ctl
(def
  knob01 112
  knob02 74 
  knob03 71 
  knob04 76 
  knob05 77 
  knob06 93 
  knob07 73 
  knob08 75 
  ;
  knob09 114
  knob10 18 
  knob11 19 
  knob12 16 
  knob13 17 
  knob14 91 
  knob15 79 
  knob16 72 )


(defun chord (&optional (mod nil) (shift 0) (root 0) (scale *chromatic*))
  (mapcar
    (curry (λ(n) (+ root (sc scale (+ n shift)))))
    (case mod
      ((nil :3) (list 0 2 4))
      ((:7)     (list 0 2 4 6))
      ((:5)     (list 0 4 7))
      ((:sus4)  (list 0 2 3))
      ((:sus2)  (list 0 1 4))
      ((:sus9)  (list 0 4 8))
      ((:7sus4) (list 0 2 3 6)))))

(defvar *chromatic* #(0 1 2 3 4 5 6 7 8 9 10 11))
(defvar *pentatonic* #(0 3 5 7 10))
(defvar *major* #(0 2 4 5 7 9 11))
(defvar *minor* #(0 2 3 5 7 8 10))

(defun euclidian (n m snt &optional (nl nil))
  (let ((ii 0))
    (loop :for i :from 0 :to (1- m)
          :collect (if (= (floor (* ii (/ m n))) i)
                       (prog1 (if (functionp snt) (funcall snt) snt) (incf ii))
                       (if (functionp nl) (funcall nl) nl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTRUMENTS


(export '(init-synths))
