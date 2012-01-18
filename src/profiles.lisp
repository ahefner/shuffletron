(in-package :shuffletron)

;;; Profile support (for multiple libraries in different
;;; locations). The "default" profile stores its settings directly
;;; under ~/.shuffletron/ for backward compatibility with existing
;;; settings. Alternate profiles store them under
;;; ~/.shuffletron/profiles/<ProfileName>/.

(defvar *profile* "default")

(defun subpath (list) (subseq list 0 (1- (length list))))

(defun prefpath (prefname &key (profile *profile*))
  (let ((name (if (listp prefname) (car (last prefname)) prefname))
        (subpath (if (listp prefname) (butlast prefname) nil))
        (*profile* profile))
    (merge-pathnames
     (make-pathname :directory `(:relative ".shuffletron" "profiles" ,*profile* ,@(mapcar #'string subpath))
                    :name (and name (string name)))
     (user-homedir-pathname))))

(defun pref (name &optional default)
  (handler-case (values (file (prefpath name)) t)
    (file-error (c)
      (when (probe-file (prefpath name))
        (format t "Problem reading ~A:~%~A~%" (prefpath name) c))
      (values default nil))
    (reader-error (c)
      (format t "Error parsing contents of ~A:~%~A~%" (prefpath name) c)
      (values default nil))))

(defun (setf pref) (value name)
  (ensure-directories-exist (prefpath name))
  (setf (file (prefpath name)) value))

(defun all-profiles ()
  (mapcar (lambda (x) (car (last (pathname-directory x))))
          (directory
           (merge-pathnames
            (make-pathname :directory '(:relative ".shuffletron" "profiles" :wild-inferiors)
                           :name "library-base")
            (user-homedir-pathname)))))

(defun get-profile-base (profile-name)
  (let ((*profile* profile-name))
    (pref "library-base")))

