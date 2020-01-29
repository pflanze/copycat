;; A library of utilities to offer writing files as root, even though
;; copycat is running as non-root user.


(: putfile-bashcode [string? contents] [path-string? path] -> string?
   "Generate bash code to write `contents` to `path`."
   (
    shell-quote
    swap
    shell-quote "echo " swap " > " 3 list strings-append
    swap string.append))

;; A solution that starts a new sudo session on every invocation:

(: sudo-putfile [string? contents] [path-string? path] ->
   "Like putfile but uses sudo / bash / echo to write the file. NOTE:
this leads to entries in /var/log/auth.log for every single
invocation (disk wear/space use)!"
   (putfile-bashcode
    shell-quote "sudo bash -c " swap string.append
    xrun-bash))

;; A better solution: use a persisting sudo bash instance:

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


;; Instead of carrying around a sudoserver instance everywhere, store
;; it as a (global, duh) name:

(new-sudoserver 1 list 'current-sudoserver set!) try
() ("Warning: new-sudoserver:" println show) if-Ok

;; And, a switchable ("how?", todo: parameterize) implementation:

(: current-putfile
   (current-sudoserver -rot sudoserver-putfile))

