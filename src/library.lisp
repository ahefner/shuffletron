(in-package :shuffletron)

(defvar *library* nil)
(defvar *filtered-library* nil "The library, excluding songs tagged 'ignore'")
(defvar *local-path->song* (make-hash-table :test 'equal))
(define-symbol-macro *library-base* (pref "library-base"))

(defstruct song full-path local-path tags smashed properties matchprops id3 id3-p)

(defun init-library ()
  (setf *library* (make-array 0 :fill-pointer 0 :adjustable t)))

(defun mp3-p (filename)
  (not (mismatch filename "mp3" :test #'char-equal :start1 (- (length filename) 3))))

(defvar *library-progress* 0)

(defun emptyp (seq) (or (null seq) (zerop (length seq))))

(defun smash-string (string)
  (substitute #\Space #\_ (string-downcase string)))

(defun carriage-return () (format t "~C" (code-char 13)))

(defun add-mp3-file (full-filename relative-filename)
  (let ((song (make-song :full-path full-filename
                         :local-path relative-filename
                         :smashed (smash-string relative-filename)
                         :tags nil)))
    (vector-push-extend song *library*)
    (setf (gethash (song-local-path song) *local-path->song*) song)))

(defun library-scan (path)
  (let ((*library-progress* 0))
    (clrhash *local-path->song*)
    (when (probe-file path)
      (walk path
            (lambda (filename)
              (when (mp3-p filename)
                (incf *library-progress*)
                (when (zerop (mod *library-progress* 10))
                  (carriage-return)
                  (format t "Scanning. ~:D files.." *library-progress*)
                  (force-output))
                (add-mp3-file filename (rel path filename)))))
      t)))

(defun songs-needing-id3-scan () (count-if-not #'song-id3-p *library*))

(defun save-id3-cache ()
  (setf (pref "id3-cache")
        (map 'vector (lambda (song) (list (song-local-path song)
                                          (song-id3-p song)
                                          (song-id3 song)))
             *library*))
  (values))

(defun load-id3-cache ()
  (loop for (name id3-p id3) across (pref "id3-cache" #())
        as song = (gethash name *local-path->song*)
        when (and song id3-p)
        do (setf (song-id3-p song) t
                 (song-id3 song) id3)))

(defun scan-id3-tags (&key verbose adjective)
  (format t "~&Scanning ID3 tags (~D).~%" (songs-needing-id3-scan))
  (when verbose (fresh-line))
  (loop with pending = (and verbose (songs-needing-id3-scan))
        with n = 1
        for song across *library*
        unless (song-id3-p song) do
        (when verbose
          (carriage-return)
          (format t "Reading ~Atags: ~:D of ~:D" (or adjective "") n pending)
          (force-output))
        (setf (song-id3 song) (mpg123:get-tags-from-file (song-full-path song) :no-utf8 t)
              (song-matchprops song) nil
              (song-id3-p song) t)
        (incf n)
        finally
        (when (and pending (not (zerop pending))) (terpri)))
  (save-id3-cache))

(defun build-sequence-table (seq &optional (key #'identity) (test #'equal))
  (let ((table (make-hash-table :test test)))
    (map nil (lambda (elt) (setf (gethash (funcall key elt) table) elt)) seq)
    table))

(defun compute-filtered-library ()
  (setf *filtered-library* (remove-if (lambda (song) (find "ignore" (song-tags song) :test #'string=)) *library*)))
