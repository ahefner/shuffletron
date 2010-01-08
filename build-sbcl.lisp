(in-package :cl-user)

(load "shuffletron.asd")
(asdf:oos 'asdf:compile-op :shuffletron)
(asdf:oos 'asdf:load-op :shuffletron)

(sb-ext:save-lisp-and-die "shuffletron-bin"
                          :executable t
                          :save-runtime-options t
                          :toplevel #'shuffletron:run)

