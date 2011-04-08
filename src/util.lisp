(in-package :shuffletron)

;;;; POSIX directory walker

(defmacro with-posix-interface (() &body body)
  `(let ((cffi:*default-foreign-encoding* :iso-8859-1))
    ,@body))

(defun find-type-via-stat (path name)
  ;; Call stat, map back to d_type form since that's what we expect.
  (let ((mode (osicat-posix:stat-mode (osicat-posix:stat (dfn path name)))))
    (cond
      ((osicat-posix:s-isdir mode) osicat-posix:dt-dir)
      ((osicat-posix:s-isreg mode) osicat-posix:dt-reg)
      (t osicat-posix:dt-unknown))))

(defun %split-list-directory (path)
  (with-posix-interface ()
    (let ((dir (osicat-posix:opendir path))
          dirs files)
      (unwind-protect
	   (loop
            (multiple-value-bind (name type) (osicat-posix:readdir dir)
              ;; Some OSes (and ancient glibc versions) don't support
              ;; d_type. We fall back to stat in that case.
              (when (and name (eql type osicat-posix:dt-unknown))
                (setf type (find-type-via-stat path name)))
              (cond
                ((null name) (return-from %split-list-directory (values dirs files)))
                ((eql type osicat-posix:dt-dir) (push name dirs))
                ((eql type osicat-posix:dt-reg) (push name files)))))
	(osicat-posix:closedir dir)))))

(defun split-list-directory (path)
  (multiple-value-bind (dirs files) (%split-list-directory path)
    (values
     (delete-if (lambda (str) (or (string= str ".") (string= str ".."))) dirs)
     files)))

(defun dfn (a b)
  (declare (type string a b))
  (if (and (char= #\/ (elt a (1- (length a))))
	   (zerop (length b)))
      a
      (concatenate 'string a (if (char= #\/ (elt a (1- (length a)))) "" "/") b)))

(defun abs-sorted-list-directory (path)
  (multiple-value-bind (dirs files) (split-list-directory path)
      (flet ((absolutize (list)
               (mapcar (lambda (filename) (dfn path filename))
                       (sort list #'string<=))))
        (values (absolutize dirs) (absolutize files)))))

(defun walk (path fn)
  "Walk directory tree, ignoring symlinks."
  (multiple-value-bind (dirs files) (abs-sorted-list-directory path)
    (map nil fn files)
    (dolist (dir dirs) (walk dir fn)))
  (values))

(defun rel (path filename)
  (let ((index (mismatch (dfn path "") filename)))
    (if (zerop index)
	(error "File ~A is not in path ~A" filename path)
        (subseq filename index))))
