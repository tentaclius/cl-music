(require "mylisp" "init.cl")
(defpackage :play (:use :cl :sc :mylisp))
(in-package :play)
(named-readtables:in-readtable :sc)
(sc-init)   ;; start new server
;(connect)
(clock-bpm 60)
(ql:quickload :cl-alsaseq)

(proxy :drone
       (-<> (mix [(sin-osc.ar 110 0 0.9)
                  (sin-osc.ar 220 0 (var-lag.kr (lf-noise0.kr 5)))
                  (sin-osc.ar 330 0 (var-lag.kr (lf-noise0.kr 5)))
                  (saw.ar (+ 440 (range (var-lag.kr (lf-noise0.kr 4)) -5 5)) 0.4)
                  ])
            (* 0.1)
            greyhole.ar
            splay.ar
            ))

(release :test)

(defun midi-map (messages)
  (dolist (message messages)
    (let* ((event-type (getf message :event-type))
           (event-data (getf message :event-data))
           (source (car (getf message :source)))
           (destination (car (getf message :dest))))
      (declare (ignorable source destination))
      (writeln event-type " " event-data)
      (when (equal event-type :snd_seq_event_controller)
        (let* ((param (getf event-data 'cl-alsaseq:param))
               (value (getf event-data 'calispel:value))
               (port (href *port-map* param)))
          (when port
            (control-set (cbus port) value))))
      (when (equal event-type :snd_seq_event_noteon)
        (let* ((note (getf event-data 'cl-alsaseq:note))
               (port (href *note-map* note)))
          (when port
            (control-set (cbus port) 1))))
      (when (equal event-type :snd_seq_event_noteoff)
        (let* ((note (getf event-data 'cl-alsaseq:note))
               (port (href *note-map* note)))
          (when port
            (control-set (cbus port) 0))))
      )))

(midihelper:stop-midihelper)
(midihelper:start-midihelper :master 96 'midi-map)



(defparameter *midi-channel* 0)

;;;;;;;
(defmacro with-midi-event ((var data type &optional (flags 0)
                                &key (queue snd_seq_queue_direct))
                           &body body)
  `(let ((,var (convert-to-foreign (list
                                    'type ,type
                                    'queue ,queue
                                    'flags ,flags
                                    )
                                   '(:struct snd_seq_event_t))))
     (with-foreign-slots (((:pointer data))
                          ,var (:struct snd_seq_event_t))
       (let ((,data data))
         ,@body))))

(defmacro! with-midi-ctrl-event ((var type channel param value) &body body)
  `(with-midi-event (,var ,g!data (ev-key-int ,type))
     (let ((,g!channel ,channel)
           (,g!param ,param)
           (,g!value ,value))
       (with-foreign-slots (((:pointer channel)
                             (:pointer param)
                             (:pointer value))
                            ,g!data (:struct snd_seq_ev_ctrl_t))
         (setf (mem-ref channel :uchar) ,g!channel)
         (setf (mem-ref param :uint) ,g!param)
         (setf (mem-ref value :int) ,g!value)))
     ,@body))

(with-foreign-slots (((:pointer source) (:pointer dest))
                       event (:struct snd_seq_event_t))
    (set-addr-slots source my-port 0)
    (set-addr-slots dest SND_SEQ_ADDRESS_UNKNOWN SND_SEQ_ADDRESS_SUBSCRIBERS)
    (snd_seq_event_output *seq event)
    (snd_seq_drain_output *seq))
;;;;;;;

(midihelper:send-event (midihelper:ev-noteon 9 77 120))

(midihelper:send-event (midihelper:ev-noteoff *midi-channel* 36 0))

(midihelper:send-event (midihelper:ev-pgmchange *midi-channel* 2)) ;; Send a program change message to switch to program #2.


;;;;;;

; amidi -p hw:1,0,0 --send-hex="F0 00 20 6B 7F 42 02 00 10 71 7F F7"
; https://github.com/bear24rw/alsa-utils/blob/master/amidi/amidi.c
; F0 00 20 6B 7F 42 02 00 10 7n cc F7
; F0 00 20 6B 7F 42 02 00 10 71 7F F7
; n - controller number from 0 to F
; cc - color
;   00 - black
;   01 - red
;   04 - green
;   05 - yellow
;   10 - blue
;   11 - magenta
;   14 - cyan
;   7F - white

(ql:quickload :cl-alsaseq)
(in-package #:cl-alsaseq)
;
(defparameter SysexLst '(#xF0 #x00 #x20 #x6B #x7F #x42 #x02 #x00 #x10 #x71 #x7F #xF7))
(defparameter SysexMsg (foreign-alloc :uint8 :initial-contents SysexLst))

(defmacro! with-midi-sysex-event ((var type ptr len) &body body)
  `(with-midi-event (,var ,g!data (ev-key-int ,type) :queue 0)
     (let ((,g!ptr ,ptr)
           (,g!len ,len))
       (with-foreign-slots (((:pointer ptr)
                             (:pointer len))
                            ,g!data (:struct snd_seq_ev_ext_t))
         (setf (mem-ref ptr :pointer) ,g!ptr)
         (setf (mem-ref len :uint) ,g!len)))
     ,@body))
;
(defun send-sysex (ptr len ctrl-type *seq my-port)
  ;(event-type-assert ctrl-type :SND_SEQ_EVENT_SYSEX :SND_SEQ_EVENT_SYSEX)
  (with-midi-sysex-event (event ctrl-type ptr len)
    (with-foreign-slots (((:pointer flags)) event (:struct snd_seq_event_t))
      (setf (mem-ref flags :uchar) 4))
    (send-midi *seq my-port event)))

(send-sysex SysexMsg 12 :SND_SEQ_EVENT_SYSEX *seq* *port*)

(send-note 127 60 0 :SND_SEQ_EVENT_NOTEON *seq* *port*)

(send-note )

(ql:quickload :cl-alsaseq)
(in-package #:cl-alsaseq)
;
(defparameter theEvent
  (convert-to-foreign
    (list 'type (ev-key-int :SND_SEQ_EVENT_SYSEX)
          'queue snd_seq_queue_direct
          'flags 4)
    '(:struct snd_seq_event_t)))
;
(defparameter *seq (open-seq "CLsysex"))
(defparameter *seq* (mem-ref *seq :pointer))
(defparameter *port* (open-port "out" *seq* :output))
;
(defparameter SysexLst '(#xF0 #x00 #x20 #x6B #x7F #x42 #x02 #x00 #x10 #x71 #x7F #xF7))
(defparameter SysexMsg (foreign-alloc :uint8 :initial-contents SysexLst))

(with-foreign-slots ((data
                      (:pointer source)
                      (:pointer dest))
                     theEvent (:struct snd_seq_event_t))
  (setf (foreign-slot-value source '(:struct snd_seq_addr_t) 'port) *port*)
  (setf (foreign-slot-value dest '(:struct snd_seq_addr_t) 'client) SND_SEQ_ADDRESS_SUBSCRIBERS)
  (setf (foreign-slot-value dest '(:struct snd_seq_addr_t) 'port) SND_SEQ_ADDRESS_UNKNOWN)
  (with-foreign-slots ((ptr
                        (:pointer len))
                       data (:struct snd_seq_ev_ext_t))
    (setf ptr SysexMsg)
    (setf (mem-ref len :uint) 12)))

(snd_seq_event_output *seq* theEvent)

;; SysEx message content from octave change
;DBGmess:(:EVENT-TYPE :SND_SEQ_EVENT_PORT_UNSUBSCRIBED :EVENT-DATA
;         (CL-ALSASEQ::PORT 0 CL-ALSASEQ::CLIENT 20) :SOURCE
;         ((CL-ALSASEQ::PORT 1 CL-ALSASEQ::CLIENT 0)) :DEST
;         ((CL-ALSASEQ::PORT 0 CL-ALSASEQ::CLIENT 130)))


(def mh (mk-midi-reader "CLmidi"))
;
(defun event-handler (msg)
  (format t "~S~%" msg))

(start-midi-reader mh #'event-handler)

(cl-alsaseq:send-note 127 60 0 :SND_SEQ_EVENT_NOTEON
           (midi-reader-seq mh) (midi-reader-out-port mh))

(send-note-on mr 60 127 0)

(stop-midi-reader mh)
