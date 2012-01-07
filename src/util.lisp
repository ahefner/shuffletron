(in-package :shuffletron)

;;;; File names

(defun dfn (a b)
  "Append a file name to a path, adding a directory separator if necessary."
  (declare (type string a b))
  (if (and (char= #\/ (elt a (1- (length a))))
	   (zerop (length b)))
      a
      (concatenate 'string a (if (char= #\/ (elt a (1- (length a)))) "" "/") b)))

(defun rel (path filename)
  (let ((index (mismatch (dfn path "") filename)))
    (if (zerop index)
	(error "File ~A is not in path ~A" filename path)
        (subseq filename index))))

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

;;;; Little utilities

(defun emptyp (seq) (or (null seq) (zerop (length seq))))

(defun build-sequence-table (seq &optional (key #'identity) (test #'equal))
  (let ((table (make-hash-table :test test)))
    (map nil (lambda (elt) (setf (gethash (funcall key elt) table) elt)) seq)
    table))

;;;; S-Expression File I/O Accessor

(defun file (filename)
  (with-open-file (in filename :external-format :latin1)
    (with-standard-io-syntax ()
      (let ((*read-eval* nil))
        (read in)))))

(defun write-sexp-file (filename object)
  (with-open-file (out filename
                       :external-format :latin1
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (with-standard-io-syntax ()
      (pprint object out))))

(defsetf file (filename) (object)
  `(write-sexp-file ,filename ,object))

;;;; Parsing

;;; Awful anaphora in these parsing macros, they often assume IN
;;; is the name of the stream variable.

(defmacro parsing ((&optional string) &body body)
  (if string
      `(with-input-from-string (in ,string) (catch 'fail ,@body))
      `(catch 'fail ,@body)))

;;; Beware disjunctive definitions where branches are prefixes of
;;; later branches.  The first match will be accepted, and there's no
;;; backtracking if that was the wrong one.
(defmacro disjunction ((&optional string) &body branches)
  (if string
      `(or ,@(loop for branch in branches collect `(parsing (,string) ,branch)))
      (let ((start (gensym)))
        `(let ((,start (file-position in)))
           (or ,@(loop for branch in branches
                       collect `(progn
                                  (assert (file-position in ,start))
                                  (parsing (,string) ,branch))))))))

;;; Parser result value: parse succeeds only when non-NIL.
(defun val (x) (or x (throw 'fail nil)))

;;; Lexical elements:

(defun num (in)
  (loop with accum = nil
        as next = (peek-char nil in nil)
        as digit = (and next (digit-char-p next 10))
        while digit do
        (read-char in)
        (setf accum (+ digit (* (or accum 0) 10)))
        finally (return (val accum))))

(defun colon (in) (val (eql #\: (read-char in nil))))
(defun mod60 (in) (let ((n (num in))) (val (and (< n 60) n))))
(defun eof (in) (val (not (peek-char nil in nil))))
(defun whitespace (in) (val (peek-char t in nil)))
(defun match (in match)
  (every (lambda (x) (val (char-equal x (val (read-char in nil))))) match))

