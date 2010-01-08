
#.(require :asdf)

(push '(MERGE-PATHNAMES ".sbcl/systems/" (USER-HOMEDIR-PATHNAME))
      asdf:*central-registry*)

(load "shuffletron.asd")
(asdf:oos 'asdf:compile-op :shuffletron)
(asdf:oos 'asdf:load-op :shuffletron)

(shuffletron:run)


