(ql:quickload "portmidi")

(defvar *midi-out* (pm:open-output (pm:get-default-output-device-id) 1024 0))

(defvar *midi-in* (pm:open-input (pm:get-default-input-device-id) 1024))

*midi-out*

*midi-in*
