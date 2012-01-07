(asdf:defsystem :shuffletron
  :name "Shuffletron"
  :description "Music player"
  :version "0.0.5"
  :author "Andy Hefner <ahefner@gmail.com>"
  :license "MIT-style license"
  :depends-on (:osicat :mixalot :mixalot-mp3 :mixalot-vorbis :mixalot-flac)
  :components ((:module src
                :serial t
                :components ((:file "packages")
                             (:file "util")
                             (:file "global")
                             (:file "help")
                             (:file "profiles")
                             (:file "library")
                             (:file "query")
                             (:file "tags")
                             (:file "audio")
                             (:file "ui")
                             (:file "alarms")
                             (:file "main")
                             (:file "status-bar")))))
