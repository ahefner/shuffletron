(in-package :cl-user)

(push :shuffletron-deploy *features*)

(load "shuffletron.asd")

(asdf:oos 'asdf:compile-op :shuffletron)
(asdf:oos 'asdf:load-op :shuffletron)

;;; Don't need this nonsense:
(sb-alien:unload-shared-object "librt.so")
(sb-alien:unload-shared-object "librt.so.1")

;;(print sb-sys:*shared-objects*)

(sb-ext:save-lisp-and-die "shuffletron-bin"
                          :executable t
                          :toplevel #'shuffletron:run
                          :save-runtime-options t)

