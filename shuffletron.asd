(asdf:defsystem :shuffletron 
  :name "Shuffletron"
  :description "An MP3 player"
  :version "0.0.2"
  :author "Andy Hefner <ahefner@gmail.com>"
  :license "MIT-style license"
  :depends-on (:mixalot :mixalot-mp3 :osicat)
  :serial t
  :components ((:file "shuffletron")))

