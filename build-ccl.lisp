;;;; Not sure where CCL users collect their systems. Some folks
;;;; set this up in their init file. If so, sorry!

(push '(MERGE-PATHNAMES ".sbcl/systems/" (USER-HOMEDIR-PATHNAME))
      asdf:*central-registry*)

(trace ccl:close-shared-library)
(trace ccl::shared-library-at)

(push :shuffletron-deploy *features*)
(load "shuffletron.asd")
(asdf:oos 'asdf:compile-op :shuffletron)
(asdf:oos 'asdf:load-op :shuffletron)

(print ccl::*shared-libraries*)
(trace cffi:close-foreign-library)

(format t "Gathering generated objects:~%")
(loop for library in (cffi:list-foreign-libraries :type :grovel-wrapper)
      for n upfrom 0
      as filename = (cffi:foreign-library-pathname library)
      as newname = (format nil "gen~D" n)
      do
      (format t "~D: ~A~%" n filename)
      (alexandria:copy-file filename
                            (merge-pathnames
                             (make-pathname :name newname
                                            :directory '(:relative "libs")
                                            :type "so")
                             *load-pathname*))
      (cffi:close-foreign-library library)
      (cffi:load-foreign-library (format nil "~A.so" newname)))

(ccl:save-application #+x86-64 "shuffletron-ccl64" #-x86-64 "shuffletron-ccl"
                      :toplevel-function #'shuffletron:run
                      :mode #o755 
                      :prepend-kernel t)

