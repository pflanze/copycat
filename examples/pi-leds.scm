;; Setting the LEDs on the Raspberry Pi 2 and higher (Pi 1 only
;; supports setting LED 0)


(: led-sys-basedir
   (.string "/sys/devices/platform/leds/leds/led" swap .append))

(: sudo-putfile [string? contents] [path-string? path] ->
   "Like putfile but uses sudo / bash / echo to write the file."
   (
    shell-quote
    swap
    shell-quote "echo " swap " > " 3 list strings-append
    swap string.append
    shell-quote "sudo bash -c " swap string.append
    ;; (XX should better not capture stdout)
    bash drop))

(: led-trigger-none [fixnum-natural0? ledNo] ->
   "Run this first to remove automatic triggers that might be set up."
   (led-sys-basedir "/trigger" .append "none" swap sudo-putfile))

(: led-set [fixnum-natural0? ledNo] [uint8? brightness] ->
   "Set brightness of the given LED. (On the current Raspberry Pis,
anything other than 0 is full brightness.)"
   (
    .string
    swap led-sys-basedir "/brightness" .append
    sudo-putfile))


(: blink [fixnum-natural0? ledNo] -> !
   "Forever blink the specified LED."
   (
    dup led-trigger-none
    (
     dup 255 led-set
     1 sleep
     dup 0 led-set
     1 sleep) loop))
