(load "~/src/mus/lib.cl")
(in-package :sc-user)
(ql:quickload :ltk)
(use-package :sc-extensions)
(use-package :bdef)
(named-readtables:in-readtable :sc)
(init)   ;; start new server
(bpm 60)

;;;;

;; wavy sin synth
(defsynth instrument ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out 0) (gate 1)
              (a 0.1) (d 0.2) (s 0.7) (r 1.5))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> (* 1/2 (sin-osc.ar fq))
              (+ (* 1/4 (sin-osc.ar (* freq 8)) (range (sin-osc.kr 3) 0.1 0.9)))
              (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
              pan2.ar (out.ar out <>)))))

;; square
(defsynth instrument ((freq 440) (freq0 440) (slide 0) (amp 0.3)
              (out bus) (gate 1)
              (a 0.01) (d 0.2) (s 0.7) (r 0.2))
  (-<> (let ((fq (x-line.kr freq0 freq slide)))
         (-<> (saw.ar fq)
              (* amp (env-gen.kr (adsr a d s r) :gate gate :act :free))
              (rlpf.ar (* 2 freq))
              pan2.ar (out.ar out <>)))))

;; `xset r off`
(def *root* (+ -12 57))
(def notes {
  "Z" 0 "S" 1 "X" 2 "D" 3 "C" 4 "V" 5
  "G" 6 "B" 7 "H" 8 "N" 9 "J" 10 "M" 11
  "COMMA" 12 "L" 13 "PERIOD" 14 "SEMICOLON" 15 "SLASH" 16
  ;
  "Q" 12 "2" 13 "W" 14 "3" 15 "E" 16 "R" 17
  "5" 18 "T" 19 "6" 20 "Y" 21 "7" 22 "U" 23
  "I" 24 "9" 25 "O" 26 "0" 27 "P" 28
  })
(def prev-fq nil)
(defun note-on (fq)
  (synth 'instrument :freq fq :a 0.001))
;(defun note-on (fq)
;  (prog1
;    (if prev-fq
;        (synth 'instrument :freq fq :freq0 prev-fq :slide 0.1)
;        (synth 'instrument :freq fq))
;    (setf prev-fq fq)))
(spawn
  (def synths {})
  (ltk:with-ltk ()
    (let* ((frm (make-instance 'ltk:frame)))
      (ltk:configure frm :padding "3 3 12 12")
      (ltk:grid frm 0 0)
      ;(ltk:bind ltk:*tk* "<Motion>"
      ;          (λ(e)
      ;            (control-set (busnum mod-fq) (float (/ (ltk:event-x e) (ltk:window-width ltk:*tk*))))
      ;            (control-set (busnum mod-amp) (float (/ (ltk:event-y e) (ltk:window-height ltk:*tk*))))))
      (ltk:bind ltk:*tk* "<KeyRelease>"
                (λ(e)
                  (let ((snt (gethash (ltk:event-char e) synths)))
                    (when snt
                      (release snt)
                      (setf (gethash (ltk:event-char e) synths) nil)))))
      (ltk:bind ltk:*tk* "<Key>"
                (λ(e)
                  (let* ((scale (gethash (format nil "~a" (ltk:event-char e)) notes))
                         (freq (if scale (midicps (+ *root* (sc *chromatic* scale))) nil)))
                    (when freq (setf (gethash (ltk:event-char e) synths) (note-on freq)))
                    (when scale (format t "~a~C~a~C~a~%" scale #\tab (midicps scale) #\tab freq))
                    )))))
  )


(stop)
