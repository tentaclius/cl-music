(ql:quickload "portmidi")

(pm:initialize)

(defvar *midi-out* (pm:open-output (pm:get-default-output-device-id) 1024 0))

(defvar *midi-in* (pm:open-input (pm:get-default-input-device-id) 1024))

(pm:list-devices)

(defvar seq-in (pm:open-input 3 1024))

(pm:read-midi seq-in)
