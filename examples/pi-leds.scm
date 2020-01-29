;; Setting the LEDs on the Raspberry Pi 2 and higher (Pi 1 only
;; supports setting LED 0)


"sudoserverlib.scm" include


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

