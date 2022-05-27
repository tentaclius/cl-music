# cl-music

Playing with cl-collider, building my framework around it along the way.

`lib.cl` has some valuable imports and definitions for faster coding. Most noticable addition is `defpattern`, a macro that defines temporal-recursive function and helps to build patterns by distributing the list of events to the time frame in TydalCycles manner.

```
(defpattern <pattern-name>
  (lambda (beat duration event)  ;; a function to process the note and run corresponding synths, see (play-note) and (play-drum)
    ...)
  (lambda (i)  ;; a function to generate a pattern. Use (seq ...) for sequential events and (sim ...) for simultaneous,
    ...)       ;; combine them recursively. There are also some useful macros to extend the pattern beyond the beat, e.g.
               ;; (per-beat ...) and (once-every ...).
```
