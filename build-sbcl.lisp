(in-package :cl-user)

(push :shuffletron-deploy *features*)

(load "shuffletron.asd")

(asdf:oos 'asdf:compile-op :shuffletron)
(asdf:oos 'asdf:load-op :shuffletron)

;;; Don't need this nonsense:
#+linux (sb-alien:unload-shared-object "librt.so")
#+linux (sb-alien:unload-shared-object "librt.so.1")

;;; Round up generated shared objects and put them in libs subdirectory:
#+linux
(let ((libdir (merge-pathnames
                (make-pathname :directory '(:relative "libs"))
                *load-pathname*)))
  (format t "Gathering generated objects:~%")
  (ensure-directories-exist libdir)
  (loop for library in (cffi:list-foreign-libraries :type :grovel-wrapper)
        for n upfrom 0
        as filename = (cffi:foreign-library-pathname library)
        as newname = (format nil "gen~D" n)
        do
        ;; cffi-grovel previously didn't give generated libraries
        ;; meaningful or guaranteed unique names, so I rename them
        ;; myself.
        (format t "~D: ~A~%" n filename)
        (alexandria:copy-file filename
                              (merge-pathnames
                                (make-pathname :name newname :type "so")
                                libdir))
        (cffi:close-foreign-library library)
        (cffi:load-foreign-library (format nil "~A.so" newname))))

(print sb-sys:*shared-objects*)

(sb-ext:save-lisp-and-die "shuffletron-bin"
                          :executable t
                          :toplevel #'shuffletron:run
                          :save-runtime-options t)

