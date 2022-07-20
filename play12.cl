
(defpattern ssin1
  (play-note 'ssin
             :release t
             :attr [:r 1 :amp 0.3]
             :note-fn (λ(n) [:freq (midicps (+ 60 (sc *minor* n)))]))
  (λ(i)
     (per-beat i
      (seq (sim 0 2 4 6) nil nil (sim 0 2 5))
      (seq (sim 0 2 4) nil nil)
      (seq nil nil nil (sim 0 3 4))
      nil
      )))

(ssin1 :start)
(ssin1 :stop)

;;;

(proxy :bass-fx
       (-<> (abus-in :bass)
            (lpf.ar 700)
            ):pos :tail)
(defpattern bass
  (play-note 'saw-bass
             :attr [:amp 0.60 :dur 1/14 :a 0.009 :d 0.008 :r .5 :lpf 7 :out (abus :bass)]
             :note-fn (λ(n) [:freq (midicps (+ 60 -24 (sc *pentatonic* n)))]))
  (λ(i)
    ;(per-beat i
    ;(seq 0 2 0 0)
    ;(seq 1 0 3 4)
    ;)
    (seq 0 2 0 3  (random 5) (random 5) 0 0)
    ))

(bass :start)
(bass :stop)

;;;

(defpattern drums
  (play-drum)
  (λ(i)
    (let ((o nil)
          (d ['bd :dur 0.1 :amp .4])
          (h ['hh :dur 0.05]))
      (sim o
           (once-every i 4 3 (seq o o (seq h h) o))
           (per-beat i
                     (seq d d))))))

(drums :start)
