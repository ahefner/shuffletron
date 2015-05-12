(in-package :shuffletron)

;;;; Global state, related macros, whatever.

(defparameter *shuffletron-version*
  (asdf:component-version (asdf:find-system "shuffletron")))

(defvar *argv* nil)
(defvar *debug-mode* nil)


