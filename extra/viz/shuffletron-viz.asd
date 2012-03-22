(asdf:defsystem :shuffletron-viz
  :name "Shuffletron Visualizer"
  :description "Psychedlic Circus"
  :version "?"
  :author "Andy Hefner <ahefner@gmail.com>"
  :license "MIT-style license"
  :depends-on (:shuffletron :playpen :napa-fft3)
  :components ((:file "viz")))
