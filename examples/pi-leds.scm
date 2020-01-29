;; Setting the LEDs on the Raspberry Pi 2 and higher (Pi 1 only
;; supports setting LED 0)


(: putfile-bashcode [string? contents] [path-string? path] -> string?
   "Generate bash code to write `contents` to `path`."
   (
    shell-quote
    swap
    shell-quote "echo " swap " > " 3 list strings-append
    swap string.append))


(: sudo-putfile [string? contents] [path-string? path] ->
   "Like putfile but uses sudo / bash / echo to write the file. NOTE:
this leads to entries in /var/log/auth.log for every single
invocation (disk wear/space use)!"
   (putfile-bashcode
    shell-quote "sudo bash -c " swap string.append
    xrun-bash))


;; better solution:

(: new-sudoserver (-> port?)
   "Open a new instance of bash running under sudo, with its stdin
connected to the returned port."
   ("sudo" ("bash") open-receiver-command))

(: sudoserver.run ([port? server] [string? code] ->)
   (over swap port.println port.flush))

(: sudoserver-putfile
   [port? server] [string? contents] [path-string? path] ->
   "Like putfile but uses the given sudoserver to write the file."
   (putfile-bashcode sudoserver.run))


new-sudoserver 1 list 'current-sudoserver set!

(: current-putfile
   (current-sudoserver -rot sudoserver-putfile))



(: led-sys-basedir [fixnum-natural0? ledNo] -> path-string?
   (.string "/sys/devices/platform/leds/leds/led" swap .append))

(: led-trigger-none [fixnum-natural0? ledNo] ->
   "Run this first to remove automatic triggers that might be set up."
   (led-sys-basedir "/trigger" .append "none" swap current-putfile))

(: led-set [fixnum-natural0? ledNo] [uint8? brightness] ->
   "Set brightness of the given LED. (On the current Raspberry Pis,
anything other than 0 is full brightness.)"
   (
    .string
    swap led-sys-basedir "/brightness" .append
    current-putfile))


(: led-blink [fixnum-natural0? ledNo] -> !
   "Forever blink the specified LED."
   (
    dup led-trigger-none
    (
     ;; really run forever:
     100 set-fuel

     dup 255 led-set
     1 sleep
     dup 0 led-set
     1 sleep) loop))

