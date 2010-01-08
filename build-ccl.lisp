;;;; Not sure where CCL users collect their systems. Some folks
;;;; set this up in their init file. If so, sorry!

(push '(MERGE-PATHNAMES ".sbcl/systems/" (USER-HOMEDIR-PATHNAME))
      asdf:*central-registry*)

(push :shuffletron-deploy *features*)
(load "shuffletron.asd")
(asdf:oos 'asdf:compile-op :shuffletron)
(asdf:oos 'asdf:load-op :shuffletron)

(ccl:save-application #+x86-64 "shuffletron-ccl64" #-x86-64 "shuffletron-ccl"
                      :toplevel-function #'shuffletron:run
                      :mode #o755 
                      :prepend-kernel t)

