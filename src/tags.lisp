(in-package :shuffletron)

;;; Tags names have a particular encoding on disk and in memory which
;;; is distinct from what is seen by the user. This allows us to have
;;; tags with forward slashes without getting screwed by the filesystem,
;;; and a tag named by a single asterisk without getting screwed by CCL.

(defun tag-unescaped-char-p (character)
  (or (<= 65 (char-code character) 90)
      (<= 97 (char-code character) 122)))

(defun decode-as-filename (string)
  (with-input-from-string (in string)
    (with-output-to-string (out)
      (loop as next = (peek-char nil in nil)
            while next do
            (cond ((char= next #\%)
                   (read-char in)
                   (write-char (code-char
                                (logior (ash (digit-char-p (read-char in) 16) 4)
                                        (digit-char-p (read-char in) 16)))
                               out))
                  (t (write-char (read-char in) out)))))))

(defun encode-as-filename (string)
  (with-input-from-string (in string)
    (with-output-to-string (out)
      (loop as next = (peek-char nil in nil)
            while next do
            (cond ((tag-unescaped-char-p next) (write-char (read-char in) out))
                  (t (read-char in)
                     (write-char #\% out)
                     (write-char (digit-char (ldb (byte 4 4) (char-code next)) 16) out)
                     (write-char (digit-char (ldb (byte 4 0) (char-code next)) 16) out)))))))

(defun list-tag-files ()
  (directory (merge-pathnames (make-pathname :name :wild)
                              (prefpath '("tag" nil)))))

(defvar *tag-file-registry* (make-hash-table :test 'equal))

(defun invalidate-tags ()
  (clrhash *tag-file-registry*)
  (loop for song across *library* do (setf (song-tags song) nil)))

(defun tag-pathname-dirty-p (pathname)
  (not (eql (file-write-date pathname)
            (gethash (pathname-name pathname) *tag-file-registry*))))

(defun tag-file-dirty-p (tag)
  (tag-pathname-dirty-p (prefpath (list "tag" tag))))

(defun dirty-tag-pathnames ()
  (remove-if-not #'tag-pathname-dirty-p (list-tag-files)))

(defun dirty-tags ()
  (mapcar #'pathname-name (dirty-tag-pathnames)))

(defun note-tag-write-time (tag)
  (setf (gethash tag *tag-file-registry*)
        (file-write-date (prefpath (list "tag" tag)))))

(defun load-tag-group (tag)
  ;; Delete all old occurrences of tag:
  (loop for song across *library*
        do (alexandria:deletef (song-tags song) tag :test #'string=))
  ;; Apply new tag:
  (loop for name across (pref (list "tag" tag))
        as song = (gethash name *local-path->song*)
        when song do (pushnew tag (song-tags song) :test #'string=))
  (note-tag-write-time tag))

(defun load-tags ()
  "Load all tags from disk."
  (map nil #'load-tag-group (dirty-tags)))

(defun songs-matching-tags (query)
  (remove-if-not (lambda (tags) (intersection query tags :test #'string=))
                 *library* :key #'song-tags))

(defun songs-matching-tag (tag) (songs-matching-tags (list tag)))

;;; Yeah, there's a couple related race conditions with these file
;;; write times. If you really want to go out of your way to tickle
;;; them, you might lose some tags. Boo hoo.

(defun save-tags-list (tag)
  (setf (pref (list "tag" tag))
        (map 'vector #'song-local-path (songs-matching-tag tag)))
  (note-tag-write-time tag))

(defun tag-songs (songs tag)
  (load-tags)
  (map nil (lambda (song)
             (unless (member tag (song-tags song) :test #'string=)
               (push tag (song-tags song))))
       songs)
  (save-tags-list tag)
  (when (string= tag "ignore") (compute-filtered-library)))

(defun tag-song (song tag) (tag-songs (list song) tag))

(defun untag-songs (songs tag)
  (load-tags)
  (map nil (lambda (song)
             (when (member tag (song-tags song) :test #'string=)
               (setf (song-tags song) (delete tag (song-tags song) :test #'string=))))
       songs)
  (save-tags-list tag)
  (when (string= tag "ignore") (compute-filtered-library)))

(defun untag-song (song tag) (untag-songs (list song) tag))

;;; Song start times

(defun song-start-prefname (song)
  (list "start-time" (encode-as-filename (song-local-path song))))

(defun song-start-time (song)
  (pref (song-start-prefname song)))

(defun (setf song-start-time) (time song)
  (setf (pref (song-start-prefname song)) time))
