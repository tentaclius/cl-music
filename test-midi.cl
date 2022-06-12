(load "~/src/cl-music/lib.cl")
(in-package :sc-user)
(named-readtables:in-readtable :sc)
(init)   ;; start new server
;(connect)
(bpm 60)
(ql:quickload :cl-alsaseq)

(def *port-map* (hshm 114 :drums
                      18 :drn1
                      19 :saw
                      16 :drone
                      74 :fold))
;
(def *note-map* (hshm 36 :drums-trg
                      37 :drn1-trg
                      38 :saw-trg
                      39 :drone-trg))

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
            (control-set (cbus port) 0)))))))

(midihelper:start-midihelper :master 96 'midi-map)

(midihelper:stop-midihelper)


(defparameter *midi-channel* 0)

;;;;;;;
(defmacro with-midi-event ((var data type
                                &key (queue snd_seq_queue_direct))
                           &body body)
  `(let ((,var (convert-to-foreign (list
                                    'type ,type
                                    'queue ,queue
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

(midihelper:send-event (midihelper:ev-noteon *midi-channel* 36 120))

(midihelper:send-event (midihelper:ev-noteoff *midi-channel* 36 0))

(midihelper:send-event (midihelper:ev-pgmchange *midi-channel* 2)) ;; Send a program change message to switch to program #2.
