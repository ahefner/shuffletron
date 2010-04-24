;;;; Shuffletron, a music player.

;;;; Copyright (c) 2009,2010 Andy Hefner

;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sellcopies of the Software, and to 
;;;; permit persons to whom the Software is furnished to do so, subject
;;;;  to the following conditions:

;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(defpackage :shuffletron
  (:use :common-lisp :mixalot :mixalot-mp3)
  (:nicknames :shuf)
  (:export #:run #:*shuffletron-version* 
           #:emptyp
           #:walk #:rel #:dfn
           #:*profile* #:pref #:prefpath
           #:*library* #:*filtered-library* #:*library-base*
           #:song #:song-full-path #:song-local-path #:song-tags
           #:song-properties #:song-id3 #:song-id3-p
           #:song-start-time
           #:songs-matching-tags #:songs-matching-tag
           #:tag-songs #:tag-song #:untag-songs #:untag-song
           #:decode-as-filename #:encode-as-filename
           #:*selection* #:selection-history*
           #:querying-library-p #:set-selection 
           #:reset-query #:refine-query #:query
           #:with-stream-control #:with-playqueue
           #:*mixer* #:*current-stream* #:*playqueue*
           #:song-of #:stopped
           #:*loop-mode* #:*wakeup-time*
           #:end-stream #:play-song #:play-songs #:play-next-song
           #:toggle-pause #:unpause
           #:current-song-playing
           #:playqueue-and-current
           #:queue-remove-songs #:queue-remove-indices
           #:parse-item-list #:parse-tag-list
           #:tag-current-song #:untag-current-song
           #:kill-tag #:tag-count-pairs
           #:parse-ranges #:expand-ranges #:extract-ranges
           #:sgr #:spacing 
           #:time->string #:utime->string #:parse-relative-time
           #:parse-alarm-args
           #:parse-and-execute))

(in-package :shuffletron)

(defparameter *shuffletron-version* "0.0.5")
(defvar *argv* nil)

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

;;;; Preferences

;;; Profile support (for multiple libraries in different
;;; locations). The "default" profile stores its settings directly
;;; under ~/.shuffletron/ for backward compatibility with existing
;;; settings. Alternate profiles store them under
;;; ~/.shuffletron/profiles/<ProfileName>/.

(defvar *profile* "default")

(defun profile-path-component ()
  (if (equal *profile* "default")
      nil
      (list "profiles" *profile*)))

(defun file (filename)
  (with-open-file (in filename :external-format :latin1)
    (with-standard-io-syntax ()
      (let ((*read-eval* nil))
        (read in)))))

(defun do-write-file (filename object)
  (with-open-file (out filename
                       :external-format :latin1
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (with-standard-io-syntax ()
      (pprint object out))))

(defsetf file (filename) (object)
  `(do-write-file ,filename ,object))

(defun subpath (list) (subseq list 0 (1- (length list))))

(defun prefpath (prefname)
  (let ((name (if (listp prefname) (car (last prefname)) prefname))
        (subpath (if (listp prefname) (subpath prefname) nil)))
    (merge-pathnames
     (make-pathname :directory `(:relative ".shuffletron" ,@(profile-path-component) ,@(mapcar #'string subpath))
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

;;;; Library

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
  (loop for (name id3-p id3) across (pref "id3-cache")
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

;;;; Tags

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

;;;; Audio

(defvar *mixer* nil)

(defclass mp3-jukebox-streamer (mp3-streamer)
  ((song :accessor song-of :initarg :song)
   (stopped :accessor stopped :initform nil)
   (enqueue-on-completion :accessor enqueue-on-completion 
                          :initform nil
                          :initarg :enqueue-on-completion)))

(defun audio-init ()
  (setf *mixer* (create-mixer :rate 44100)))

;;;; State

(defvar *debug-mode* nil)

(defvar *eval-support* 'smart
  "Whether Lisp evaluation from the Shuffletron prompt is allowed.
  May be NIL, T or 'SMART.")

(defvar *selection* nil)
(defvar *selection-changed* nil)
(defvar *selection-history* nil)
(defvar *history-depth* 16)

(defun querying-library-p () (= (length *selection*) (length *filtered-library*)))

(defun set-selection (new-selection &key (record t))
  (when record
   (push *selection* *selection-history*)
   (when (> (length *selection-history*) *history-depth*)
     (setf *selection-history* (subseq *selection-history* 0 *history-depth*))))
  (setf *selection* new-selection
        *selection-changed* t)
  (values))

(defun reset-query ()
  (set-selection (copy-seq *filtered-library*) :record nil)
  (loop for x across *library* do (setf (song-matchprops x) nil)))

(defmacro any (&body forms)
  "Similar to the OR macro, but doesn't short-circuit."
  (let ((syms (loop repeat (length forms) collect (gensym "ANY"))))
    `((lambda ,syms (or ,@syms)) ,@forms)))

(defun do-query (substring update-highlighting)
  (declare (optimize (speed 3)))
  ;; Query and update highlighting:
  (loop for song across *selection*
        with query = (coerce (string-downcase substring) 'simple-string)
        with new-selection = (make-array 0 :adjustable t :fill-pointer 0)
        do
        (labels 
            ((field (keyword)
               (let* ((string (if (eql keyword :filename)
                                  (song-local-path song)
                                  (getf (song-id3 song) keyword)))
                      (searchable (if (eql keyword :filename)
                                      (song-smashed song)
                                      string))
                      (found nil))
                 (when searchable
                   ;; There's no guarantee that this really holds...
                   (check-type searchable simple-string)
                   ;; The case insensitive search is painfully slow, so shortcut
                   ;; around it for filenames, which are already downcased.
                   (if (eql keyword :filename)
                       (setf found (search query searchable))
                       (setf found (search query searchable :key #'char-downcase))))
                 (when (and found update-highlighting)
                   (setf (getf (song-matchprops song) keyword)
                         (fill (the (simple-array bit 1)
                                 (or (getf (song-matchprops song) keyword)
                                     (make-array (length string) :element-type 'bit)))
                               1 :start found :end (+ found (length query)))))
                 found)))
          (when (any (field :filename)
                     (field :artist)
                     (field :album)
                     (field :title))
            (vector-push-extend song new-selection)))
        finally (return new-selection)))

(defun refine-query (substring)
  (set-selection (do-query substring t)))

(defun query (substring) (do-query substring nil))

;;; Lock discipline: If you need both locks, take the playqueue lock first.
;;; This locking business is very dodgy.

(defvar *cslock* (bordeaux-threads:make-lock "current stream lock"))
(defvar *i-took-the-cs-look* nil)
(defvar *current-stream* nil)

(defmacro with-stream-control (() &body body)
  `(let ((*i-took-the-cs-look* t))
     (bordeaux-threads:with-lock-held (*cslock*) ,@body)))

(defvar *pqlock* (bordeaux-threads:make-lock "play queue lock"))
(defvar *playqueue* nil)

(defvar *loop-mode* nil)

(defvar *wakeup-time* nil
  "Time to wake up if alarm clock is enabled.")

(defmacro with-playqueue (() &body body)
  `(bordeaux-threads:with-lock-held (*pqlock*)
     (when *i-took-the-cs-look*
       (format t "~&You took the PQ lock inside the CS lock. Don't do that.~%")
       #+SBCL (sb-debug:backtrace)
       #+CCL (ccl:print-call-history :detailed-p nil))
     ,@body))

(defun end-stream (stream)
  ;; TODO: Fade out nicely.
  (setf (stopped stream) t)
  (mixer-remove-streamer *mixer* stream)
  (setf *current-stream* nil)
  (update-status-bar))

(defun play-song (song &key enqueue-on-completion)
  (with-stream-control ()
    (when *current-stream* (end-stream *current-stream*))
    (let ((new (make-mp3-streamer (song-full-path song)
                                  :prescan (pref "prescan" t)
                                  :class 'mp3-jukebox-streamer
                                  :song song
                                  :enqueue-on-completion enqueue-on-completion))
          (start-at (song-start-time song)))
      (setf *current-stream* new)
      (mixer-add-streamer *mixer* *current-stream*)
      (when start-at 
        ;; Oh, look, another race. Great API I have here.
        (streamer-seek new *mixer* (* start-at (mixer-rate *mixer*))))))
  (update-status-bar))

(defun play-songs (songs)
  (when (> (length songs) 0)
    (play-song (elt songs 0) :enqueue-on-completion *loop-mode*)
    (show-current-song))
  (when (> (length songs) 1)
    (with-playqueue ()
      (setf *playqueue* (concatenate 'list (subseq songs 1) *playqueue*)))))

(defun play-next-song ()
  (with-playqueue ()
    (cond
      (*playqueue* 
       (let ((next (pop *playqueue*)))
         (play-song next)
         (when *loop-mode*
           (setf *playqueue* (append *playqueue* (list next))))))
      (t (with-stream-control ()
           (when *current-stream* (end-stream *current-stream*)))))))

(defmethod streamer-cleanup ((stream mp3-jukebox-streamer) mixer)
  (declare (ignore mixer))
  (call-next-method)
  ;; If stopped is set, someone else can be expected to be starting up
  ;; the next song. Otherwise, we have to do it ourselves.
  (unless (stopped stream)
    ;; If the song completed 
    (when (enqueue-on-completion stream)
      (with-playqueue ()
        (setf *playqueue* (append *playqueue* (list (song-of stream))))))
    (with-stream-control ()
      (when (eq stream *current-stream*)
        (setf *current-stream* nil)))
    ;; We do this call in new thread, because we are in the mixer
    ;; thread here, and scanning the next file could take long enough
    ;; to stall it.
    (bordeaux-threads:make-thread (lambda () (play-next-song)))))

(defun toggle-pause ()
  (with-stream-control ()
    (when *current-stream*
      (if (streamer-paused-p *current-stream* *mixer*)
          (streamer-unpause *current-stream* *mixer*)
          (streamer-pause *current-stream* *mixer*)))))

(defun unpause ()
  (with-stream-control ()
    (when *current-stream*
      (when (streamer-paused-p *current-stream* *mixer*)
        (streamer-unpause *current-stream* *mixer*)
        t))))

(defun current-song-playing ()
  (let ((stream *current-stream*))
    (and stream (song-of stream))))

(defun playqueue-and-current ()
  (let ((current (current-song-playing)))
    (if current
        (cons current *playqueue*)
        *playqueue*)))

(defun queue-remove-songs (songs)
  (let ((set (build-sequence-table songs #'identity #'eq)))
    (with-playqueue ()
      (setf *playqueue*
            (remove-if (lambda (song) (gethash song set)) *playqueue*)))))

(defun queue-remove-indices (indices)
  (with-playqueue ()
    (setf *playqueue* (list-remove-by-index *playqueue* indices))))

(defun list-remove-by-index (list indices)
  (loop with seq = list
        with list = (sort (remove-duplicates indices) #'<)
        for index upfrom 0
        for song in seq
        unless (and (eql index (car list)) (pop list))
        collect song))

;;;; Tagging UI

(defun item-list-delim (char) (or (char= char #\Space) (char= char #\,)))

(defun parse-item-list (string start)
  (cond
    ((null start) nil)
    (t (let* ((istart (position-if-not #'item-list-delim string :start start))
              (iend   (and istart (position-if #'item-list-delim string :start istart)))
              (item (and istart (subseq string istart iend))))
         (and item (cons item (parse-item-list string iend)))))))

(defun parse-tag-list (tags-arg)
  (mapcar #'encode-as-filename (parse-item-list tags-arg 0)))

(defun show-song-tags (song &key (no-tags-fmt ""))
  (load-tags)
  (cond 
    ((not song) (format t "No song is playing.~%"))
    ((song-tags song) (format t "Tagged: ~{~A~^, ~}~%" 
                              (mapcar #'decode-as-filename (song-tags song))))
    (t (format t no-tags-fmt))))

(defun show-current-song-tags ()
  (let ((song (current-song-playing)))
    (when song (show-song-tags song :no-tags-fmt "No tags.~%"))))

(defun tag-current-song (tags-arg)
  (let* ((song (current-song-playing))
         (tags (parse-tag-list tags-arg)))
    (cond 
      (song
       (dolist (tag tags) (tag-song song tag))
       (show-current-song-tags))
      (t (format t "No song is playing.~%")))))

(defun untag-current-song (tags-arg)
  (let* ((song (current-song-playing))
         (tags (if tags-arg 
                   (parse-tag-list tags-arg)
                   (song-tags song))))                  
    (if song
        (dolist (tag tags) (untag-song song tag))
        (format t "No song is playing.~%"))))

(defun kill-tag (tag)  
  (loop for song across *library*
        with num-killed = 0
        do
        (when (find tag (song-tags song) :test #'string=)
          (setf (song-tags song) (delete tag (song-tags song) :test #'string=))
          (incf num-killed))
        finally (format t "Removed ~:D occurrence~P of ~A~%" 
                        num-killed num-killed (decode-as-filename tag)))
  (save-tags-list tag))

(defun tag-count-pairs (songs)
  (let* ((all-tags (loop for song across songs
                         appending (song-tags song)))
         (no-dups nil)
         (counts (make-hash-table :test 'equal)))
    (dolist (tag all-tags)
      (cond
        ((gethash tag counts) (incf (gethash tag counts)))
        (t (setf (gethash tag counts) 1
                 no-dups (cons tag no-dups)))))
    (setf no-dups (sort no-dups #'string<=))
    (loop for tag in no-dups collect (cons tag (gethash tag counts)))))

(defun show-all-tags ()
  (format t "All tags in ~A: ~{~A~^, ~}~%" 
          (if (querying-library-p) "library" "query")
          (loop for (tag . count) in (tag-count-pairs *selection*)
                as printable = (decode-as-filename tag)
                if (= 1 count) collect printable
                else collect (format nil "~A(~A)" printable count))))

;;;; UI

;;; Temporal coupling ==> Recursive lock as workaround ==> Interesting.
(defvar *output-lock* (bordeaux-threads:make-recursive-lock "Output Lock"))
(defmacro with-output (() &body body)
  `(bordeaux-threads:with-recursive-lock-held (*output-lock*)
     ,@body))

(defvar *term-rows* 80)
(defvar *term-cols* 25)

(defun get-terminal-size ()
  (cffi:with-foreign-object (winsize :unsigned-short 4)
    (and (zerop (cffi:foreign-funcall "ioctl" 
                                      :int 1        ; fd
                                      :int #x5413   ; TIOCGWINSZ (Linux!!)
                                      :pointer winsize
                                      :int))
         (values (cffi:mem-aref winsize :unsigned-short 0)
                 (cffi:mem-aref winsize :unsigned-short 1)))))

(defun update-terminal-size ()
  (multiple-value-bind (rows cols) (get-terminal-size)
    (setf *term-rows* (or rows *term-rows*)
          *term-cols* (or cols *term-cols*))))

(defparameter *max-query-results* 50
  "Maximum number of results to print without an explicit 'show' command.")

(defun parse-ranges (string start max)
  "Parse comma delimited numeric ranges, returning a list of min/max
pairs as cons cells."
  ;; This function isn't very good. Curiously, using parse-integer
  ;; actually made it harder, I suspect.
  (when (or (>= start (length string))
            (char= #\- (aref string start)))
    (return-from parse-ranges nil))  
  (labels ((clamp (x) (max 0 (min x max)))
           (range (x y) (cons (clamp (min x y)) (clamp (max x y)))))
    (multiple-value-bind (min idx)
        (parse-integer string :junk-allowed t :start start)      
      (cond
        ((null min) nil)
        ((= idx (length string)) (list (range min min)))
        ((or (char= #\, (aref string idx))
             (char= #\  (aref string idx)))
         (list* (range min min)
                (parse-ranges string (1+ idx) max)))
      ((char= #\- (aref string idx))
       (multiple-value-bind (parsed-max idx)
           (parse-integer string :junk-allowed t :start (1+ idx))
         (list* (range min (or parsed-max max))
                (parse-ranges string (1+ idx) max))))
      (t (format t "???~%") nil)))))

(defun expand-ranges (ranges)
  (loop for (min . max) in ranges
        nconcing (loop for i from min upto max collect i)))

(defun extract-ranges (vector rangespec-string)
  (map 'vector (lambda (i) (aref vector i))
       (expand-ranges (parse-ranges rangespec-string 0 (1- (length vector))))))

(defun spooky-init ()
  (let ((stream #+sbcl (sb-sys:make-fd-stream 1 :external-format :latin1 :output t :input nil)
                #+ccl (ccl::make-fd-stream 1 :direction :io :sharing :lock :encoding :iso-8859-1)))
    (setf *standard-output* stream)
    (setf *error-output*    stream)
    #+sbcl
    (sb-sys:enable-interrupt sb-unix:sigint 
      (lambda (&rest args) (declare (ignore args)) (sb-ext:quit)))))

(defun quit ()
  (format t "Bye.~%")
  (finish-output)  
  #+sbcl (sb-ext:quit)
  #+ccl (ccl:quit))

(defun getline ()  
  (finish-output *standard-output*)
  (or (read-line *standard-input* nil) (quit)))

(defun compute-filtered-library ()
  (setf *filtered-library* (remove-if (lambda (song) (find "ignore" (song-tags song) :test #'string=)) *library*)))

(defun parse-command-line-args ()
  (let ((args *argv*))
    (loop with arg1 = nil 
          while args
          as arg = (pop args) do
          (flet ((bin (name &key (argname "parameter"))
                   (when (equal arg name)
                     (unless args
                       (format t "Argument ~W expects a ~A." name argname)                       
                       (quit))
                     (setf arg1 (pop args))
                     t)))
            (cond
              ((bin "--profile")
               (setf *profile* arg1)
               (format t "~&Using profile ~W~%" *profile*))
              (t (format t "~&Unrecognized argument ~W.~%" arg)))))))

(defun init ()
  (parse-command-line-args)
  (setf *random-state* (make-random-state t))
  (loop do
        (init-library)
        (unless *library-base*
          (format t "~&Enter library path: ")
          (setf *library-base* (dfn (getline) "")))
        (when (not (library-scan *library-base*))
          (format t "Unable to scan \"~A\"~%" *library-base*)
          (setf *library-base* nil))
        (when (emptyp *library*)
          (format t "No playable files found in \"~A\"~%" *library-base*)
          (setf *library-base* nil))
        until *library-base*)
  (format t "~CLibrary contains ~:D files.        ~%"
          (code-char 13) (length *library*))
  (load-tags)
  (compute-filtered-library)
  (load-id3-cache)
  (reset-query)
  ;; Scan tags of new files automatically, unless there's a ton of them.
  (let ((need (songs-needing-id3-scan)))
    (cond
      ((zerop need))
      ((> need 1000)
       (format t "~:D new songs need to be scanned for ID3 tags. To do this now, 
type \"scanid3\". It may take a moment.~%"
               (songs-needing-id3-scan)))      
      (t (scan-id3-tags :verbose t :adjective "new ")))))

(defun sgr (modes) (format t "~C[~{~D~^;~}m" #\Esc modes))

(defun print-decorated (style-0 style-1 string markings style)
  (loop for char across string
        for new-style across markings
        with current-style = nil
        do
        (unless (eql new-style current-style)
          (sgr (list* (if (zerop new-style) style-0 style-1)
                      style))
          (setf current-style new-style))
        (write-char char)
        finally (sgr '(0))))

(defun maybe-underlined (string markings-or-nil style)
  (cond
    ((null markings-or-nil)
     (when style (sgr style))
     (write-string string)
     (when style (sgr '(0))))
    (t (print-decorated 0 4 string markings-or-nil style))))

(defun spacing (n) 
  (when (> n 0) (loop repeat n do (write-char #\.))))

(defparameter *color-prefs* '(:artist (31) :album (33) :title (37) :elided (90)))

(defun show-song-matches (items &key (mode :query) (highlight-queue nil))
  (loop with hash = (and highlight-queue
                         (build-sequence-table (playqueue-and-current)))
        with last-artist = nil
        with last-album = nil
        for item across items
        as id3 = (song-id3 item)
        as mp = (song-matchprops item)
        as artist = (getf id3 :artist)
        as album  = (getf id3 :album)
        as title  = (getf id3 :title)
        as track  = (getf id3 :track)
        for n upfrom 0 do 

        (ecase mode
          (:query
           (loop for (keyword marker) in '((:filename #\f) (:artist   #\a)
                                           (:album    #\b) (:title    #\t))
                 do (write-char (if (getf mp keyword) marker #\Space)))
           (when hash (when (gethash item hash) (sgr '(1 97))))
           (format t "~5D " n)
           (when highlight-queue (sgr '(0))))
          (:list
           (format t " ~7<(~D)~>  " n)))

        (labels ((field (name key)
                   (maybe-underlined name (getf mp key) (getf *color-prefs* key)))
                 (sep () (format t ", "))
                 (post-number () 
                   #|(sgr '(90))
                   (format t " (~D)" n)
                   (sgr '(0))|#)
                 (track-and-title ()
                   (when track
                     (format t "~2D: " track))
                   (field title :title)
                   (post-number)))
          (cond 
            ((and artist title)
             (cond
               ((equalp artist last-artist)
                (sgr (getf *color-prefs* :elided))
                (write-char #\")
                (spacing (- (length artist) 1))
                (cond
                  ((and album (equalp album last-album))
                   (spacing (+ 2 (length album)))
                   (format t "\" ")
                   (sgr '(0))
                   (track-and-title))
                  (album
                   (format t "\" ")
                   (sgr '(0))
                   (field album :album)
                   (sep)
                   (track-and-title))
                  (t (format t "\" ")
                     (sgr '(0))
                     (track-and-title))))
               (t (field artist :artist)
                  (sep)
                  (when album 
                    (field album :album)
                    (sep))
                  (track-and-title)))
             (setf last-artist artist
                   last-album  album))
            (t (field (song-local-path item) :filename)
               ;; Occasionally we may have an artist but not the
               ;; title.  Clear these, so we don't elide fields that
               ;; we didn't actually print.
               (setf last-artist nil
                     last-album nil)
               (post-number))))

        (terpri)))

(defun show-current-query ()
  (if (emptyp *selection*)
      (format t "  Nothing matches the current query.~%")
      (show-song-matches *selection* :mode :query :highlight-queue t)))

(defun vector-select-ranges (vector rangespec)
  (if (emptyp vector)
      (vector)
      (extract-ranges vector rangespec)))

(defun selection-songs (rangespec)
  (vector-select-ranges *selection* rangespec))

(defun time->string (seconds)
  (setf seconds (round seconds))
  (if (>= seconds 3600)      
      (format nil "~D:~2,'0D:~2,'0D" (truncate seconds 3600) (mod (truncate seconds 60) 60) (mod seconds 60))
      (format nil "~D:~2,'0D" (truncate seconds 60) (mod seconds 60))))

(defun print-id3-properties (stream props)
  (when (getf props :title)
    (format stream "  Title: ~A" (getf props :title)))
  (let* ((line-length 76)
         (remaining 0))    
    (labels ((show (fmt &rest args)
               (let ((string (apply #'format nil fmt args)))
                 (when (< remaining (length string))
                   (fresh-line)                                      
                   (write-string "  " stream)
                   (setf remaining line-length))
                 (write-string string stream)
                 (write-string "        " stream)
                 (decf remaining (1+ (length string)))))
             (field (name indicator)
               (when (getf props indicator)
                 (show "~A: ~A" name (getf props indicator)))))
      (field "Artist"  :artist)
      (field "Album"   :album)
      (field "Track"   :track)
      (field "Genre"   :genre)
      (field "Comment" :comment)
      (terpri))))

(defun show-current-song (&optional delimit)
  (let* ((current *current-stream*)
         (song (and current (song-of current)))
         (start-time (and song (song-start-time song))))
    (when current
      (when delimit (terpri))
      (let ((pos (streamer-position current *mixer*))
            (len (streamer-length   current *mixer*)))
        (format t "[~A/~A] ~A ~A~%"
                (time->string (round pos (mixer-rate *mixer*)))
                (time->string (round len (mixer-rate *mixer*)))
                (if (streamer-paused-p current *mixer*)
                    "Paused:"
                    "Playing:")
                (song-local-path song)))
      (print-id3-properties *standard-output* (song-id3 song))
      (when start-time
        (format t "Start time is set to ~A~%" (time->string start-time)))
      (show-song-tags song)      
      (when delimit (terpri)))))

(defun show-playqueue ()
  (with-playqueue ()
    (cond
      ((emptyp *playqueue*)
       (format t "The queue is empty.~%"))
      (t (show-song-matches (coerce *playqueue* 'vector)
                            :mode :list :highlight-queue nil)))
    (when *loop-mode* (format t "Loop mode enabled.~%"))
    (show-current-song t)))

(defun stop-command ()
  (with-playqueue ()
    (with-stream-control ()
      (when *current-stream*
        (push (song-of *current-stream*) *playqueue*)
        (end-stream *current-stream*)))))

(defun play-command ()
  (unless (unpause)
    (or (current-song-playing) (play-next-song))))

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

;;; Time parsers:

(defun parse-timespec (string)
  "Parse a time/duration, in one of three formats (seconds, m:ss, h:mm:ss)"
  (disjunction (string)
    ;; Seconds format (a single integer):
    (prog1 (num in) (eof in))
    ;; mm:ss format (seconds must be modulo 60):
    (+ (* 60 (prog1 (num in)   (colon in)))
       (*  1 (prog1 (mod60 in) (eof in))))
    ;; h:mm:ss format (minutes and seconds must be modulo 60):
    (+ (* 3600 (prog1  (num in)  (colon in)))
       (*   60 (prog1 (mod60 in) (colon in)))
       (*    1 (prog1 (mod60 in) (eof in))))))

(defun do-seek (args)
  (let ((current *current-stream*)
        (time (and args (parse-timespec args))))
    (cond 
      ((null current) (format t "No song is playing.~%"))
      ((null time) (format t "Seek to where?~%"))
      (time (streamer-seek current *mixer* (* (mixer-rate *mixer*) time)))
      (t nil))))

(defun 12hour (in) (let ((n (num in))) (val (and (< 0 n 13) (mod n 12)))))

(defun parse-12-hour-format (in)
  "Parse numeric portions (hour or h:mm) of 12-hour time format, returning a count in minutes."
  (val
   (disjunction ()
     ;; h:mm format
     (+ (prog1 (* 60 (12hour in)) (colon in))
        (mod60 in))
     ;; Bare time in hours:
     (* 60 (12hour in)))))

(defun parse-daytime (in)
  "Parse string as a time of day (AM/PM), for the alarm
clock. Returns time in minutes from midnight."
  (disjunction ()
    ;; If there's no time, default to AM
    (prog1 (parse-12-hour-format in) (eof in))
    ;; AM time
    (prog1 (parse-12-hour-format in)
      (whitespace in)
      (disjunction () (match in "a.m.") (match in "am.") (match in "am"))
      (eof in))
    ;; PM time
    (+ (prog1 (parse-12-hour-format in)
         (whitespace in)
         (disjunction () (match in "p.m.") (match in "pm.") (match in "pm"))
         (eof in))
       720)))

(defun utime->string (utime)
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time utime)
    (declare (ignore second))
    (print month)
    (format nil "~A ~A ~D ~D:~2,'0D ~A ~D"
            (nth day '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
            (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
            date
            (1+ (mod (1- hour) 12))
             minute
             (if (>= hour 12) "PM" "AM")
             year)))

(defun print-time ()
  (format t "~A~%" (utime->string (get-universal-time))))

(defun daytime->alarm-time (daytime)
  "Translate a daytime (in minutes) to a universal time for the
alarm. Since the daytime doesn't specify a date, we choose tomorrow
rather than today if the date would be less than the current time."
  (let* ((current-time (get-universal-time))
         (decoded (multiple-value-list (decode-universal-time current-time)))
         (minutes (second decoded))
         (hours   (third decoded))
         (current (+ minutes (* hours 60))))
    (cond 
      ((< current daytime)
       (encode-universal-time 0 (mod daytime 60) (truncate daytime 60)
                              (fourth decoded) (fifth decoded) (sixth decoded)))
      (t (multiple-value-bind (s m h date month year) ; Get tomorrow's date.
             (decode-universal-time (+ current-time 86400))
           (declare (ignore s m h))
           (encode-universal-time 0 (mod daytime 60) (truncate daytime 60)
                                  date month year))))))

(defun parse-relative-time (in)
  (disjunction ()
    ;; Time in minutes:
    (prog1 (* 60 (num in))
      (whitespace in)
      (val (disjunction ()
             (match in "minutes") (match in "minute")
             (match in "mins") (match in "min") (match in "m")))
      (eof in))
    ;; Time in hours:
    (prog1 (* 3600 (num in))
      (whitespace in)
      (val (disjunction ()
             (match in "hours") (match in "hour")
             (match in "hr") (match in "h"))))
    ;; Time in h:mm format:
    (+ (* 3600 (prog1 (num in) (colon in)))
       (*   60 (mod60 in)))))

(defun parse-alarm-args (args)
  "Parse the arguments to the alarm command, returning NIL or a universal time."
  (disjunction (args)
    ;; State the time directly:
    (daytime->alarm-time (val (parse-daytime in)))
    ;; Syntactic sugar for stated time:
    (and (match in "at ") 
         (whitespace in)
         (daytime->alarm-time (val (parse-daytime in))))
    ;; Relative time offset:
    (and (match in "in ") 
         (whitespace in)
         (+ (get-universal-time) (val (parse-relative-time in))))))

(defvar *alarm-thread* nil)

(defun trigger-alarm ()
  ;; When the alarm goes off, unpause the player if it's paused. If it
  ;; isn't paused but there are songs in the queue, play the next
  ;; song. If the queue is empty, queue up ten random songs and play
  ;; one.
  (setf *wakeup-time* nil)
  (unless (unpause)
    (with-playqueue ()
      (unless (or *playqueue* (emptyp *filtered-library*))
        (loop repeat 10 do (push (alexandria:random-elt *filtered-library*) *playqueue*))))
    (play-next-song)))

(defun alarm-thread-toplevel ()
  (unwind-protect
       (loop with interval = 60
             as wakeup = *wakeup-time*
             as remaining = (and wakeup (- wakeup (get-universal-time))) 
             do
             (cond
               ((null wakeup) (sleep interval))
               ((<= remaining 0) (trigger-alarm))
               (t (sleep (min remaining interval))
                  (update-status-bar))))
    (setf *alarm-thread* nil)))

(defun set-alarm (utime)
  (setf *wakeup-time* utime)
  (unless *alarm-thread*
    (setf *alarm-thread* (bordeaux-threads:make-thread #'alarm-thread-toplevel))))

(defun do-set-alarm (args) 
  (cond
    ((null args)
     (let ((wakeup *wakeup-time*))
       (if wakeup
           (format t "Alarm set for ~A~%" (utime->string wakeup))
           (format t "The alarm is not set.~%"))))
    ((member args '("off" "never" "delete" "disable" "cancel" "clear" "reset") :test #'string-equal)
     (setf *wakeup-time* nil)
     (format t "Disabled alarm.~%"))
    (t (let ((time (parse-alarm-args args)))
         (cond 
           ((null time) (format t "Unable to parse as time: ~W~%" args))
           (t (set-alarm time)
              (format t "Alarm set for ~A~%" (utime->string time))))))))

(defun print-help ()
  (format t "
Shuffletron  is a text-mode  music player  oriented around  search and
tagging. Its principle of operation  is simple: search for songs, then
play them. Searches are performed by typing a / followed by the search
string:

library> /chromeo
fa      0 Chromeo, She's In Control, 10: Ah Oui Comme Ca
fa      1 \"........................\"  1: My And My Man
fa      2 \"........................\"  2: Needy Girl
fa      3 \"........................\"  3: You're So Gangsta
fa      4 \"........................\"  4: Woman Friend
fa      5 \"........................\"  7: Since You Were Gone
fa      6 \"........................\"  8: Way Too Much
fa      7 \"........................\"  9: Mercury Tears
f b     8 DJ Mehdi, I Am Somebody featuring Chromeo,  2: I Am Somebody (Montreal Version)
9 matches>

If ID3 tags are present, songs are presented in the following form:

   Artist, [Album,] [Track:] Title

Although  not shown  above, artist  names are  color coded  red, album
names yellow,  and song  titles white.  In  successive lines  with the
same artist or  album and artist, the redundant  fields are elided. If
ID3 information on the artist and title is not available, the filename
is printed instead.

In the  leftmost column is some  subset of the letters  'f', 'a', 'b',
and 't'.   These indicate  which fields matched  the query  string, as
follows:

   f: Filename
   a: Artist
   b: Album
   t: Title

Following this is a column of numbers, starting from zero. These allow
you to choose  songs to play as comma (or  space) delimited numbers or
ranges of numbers. If the song  is already in the queue, the number is
highlighted in bold white text. Here, I decide to play song 8 then 0-3
by entering this at the prompt:

9 matches> 8, 0-3

The currently  playing song is  interrupted, and the chosen  songs are
added to  the head of the playback  queue. To see the  contents of the
queue, use the 'queue' command:

9 matches> queue
     (0)  Chromeo, She's In Control, 10: Ah Oui Comme Ca
     (1)  \"........................\"  1: My And My Man
     (2)  \"........................\"  2: Needy Girl
     (3)  \"........................\"  3: You're So Gangsta

Notice that  the prompt changed  from \"library>\" to  \"\9 matches>\"
after  our initial search.  Successive searches  refine the  result of
previous searches, and the prompt indicates the number of items you're
currently searching  within. If there  had been more than  50 matches,
they would  not be printed  by default, but  you could use  the 'show'
command at any time to print  them. Also note that the 'queue' command
doesn't disrupt the  current search results (this is  why numbering in
the  queue listing is  surrounded with  parentheses, to  indicate that
entering numbers for  playback does not refer to  them). The queue can
be cleared with the 'clear'  command, and the 'skip' command skips the
current song and advances to the next song in the queue.

To  add songs  to the  queue  without interrupting  the current  song,
prefix the song list with \"+\" (to append) or \"pre\" (to prepend).

When you've  completed a  search, a single  blank line  moves backward
through the search history, allowing you to return to the \"library>\"
prompt.

If you've  imported a large  library, the ID3  tags may not  have been
scanned.   In this case,  the program  will suggest  that you  run the
scanid3 command.   Scanning ID3  tags can be  very time  consuming, as
each file must be opened  and read from. Once scanned, ID3 information
is remembered by caching  it in the ~~/.shuffletron/id3-cache file, so
you only need to do this the first time you run the program.  ID3 tags
of new files are scanned  automatically when the program starts unless
there are more than 1,000 new files.

Additional help topics:
   help commands
   help examples
   help looping
   help alarms

"))

(defun print-commands ()
  (format t "
Command list:

  /[query]       Search library for [query].
  show           Print search matches, highlighting songs in queue.
  back           Undo last search.
  [songs]        Play list of songs.
  +[songs]       Append list of songs to queue.
  pre[songs]     Prepend list of songs to queue.
  random         Play a random song from the current selection.
  random QUERY   Play a random song matching QUERY

  queue          Print queue contents and current song playing.
  shuffle        Randomize order of songs in queue.
  clear          Clear the queue (current song continues playing)
  loop           Toggle loop mode (loop through songs in queue)
  qdrop          Remove last song from queue
  qdrop RANGES   Remove songs from queue
  qtag TAGS      Apply tags to all songs in queue
  fromqueue      Transfer queue to selection
  toqueue        Replace queue with selection

  now            Print name of song currently playing.
  play           Resume playing
  stop           Stop playing (current song pushed to head of queue)
  pause          Toggle paused/unpaused.
  skip           Skip currently playing song.
  repeat N       Add N repetitions of currently playing song to head of queue.
  seek TIME      Seek to time (in [h:]m:ss format, or a number in seconds)
  startat TIME   Always start playback at a given time (to skip long intros)

  tag            List tags of currently playing song.
  tag TAGS       Add one or more textual tags to the current song.
  untag TAGS     Remove the given tags from the currently playing song.
  tagged TAGS    Search for files having any of specified tags.
  tags           List all tags (and # occurrences) within current query.
  killtag TAGS   Remove all occurances of the given tags
  tagall TAGS    Apply tags to all selected songs
  untagall TAGS  Remove given tags from all selected songs

  time           Print current time
  alarm          Set alarm.
  
  scanid3        Scan new files for ID3 tags
  prescan        Toggle file prescanning (useful if file IO is slow)
  exit           Exit the program.
  
  help [topic]   Help
"))

(defun print-examples ()
  (format t "
How to find and play a song, then return to library mode:

library> /vampire sushi
f  t    0 Old Time Relijun, Witchcraft Rebellion,  3: Vampire Sushi

1 matches> 0
1 matches>
library>

How to refine search results:

library> /beatles
223 matches> /window
fa t    0 Beatles, The, Abbey Road, 13: She Came In Through The Bathroom Window
1 matches> 

How to play your entire library in shuffle mode:

  library> 0-            # Add open interval starting from 0 to queue
  library> shuffle       # Shuffle the queue
  library> skip          # It started playing before you shuffled, so skip..

"))

(defun print-loop-help ()
  (format t "
The \"loop\"  command toggles  looping mode. In  looping mode,  a song
taken from the head of  the queue for playback is simultaneously added
at the tail of the queue.

Choosing a  single song to  play (interrupting the current  song) does
not  affect  the  contents of  the  queue,  and  there's no  issue  in
interrupting a song which you'd like to continue looping in the queue,
because it  has already  been rotated  to the end  of the  queue. This
behavior  allows you  to  audition  songs for  addition  to the  queue
without disturbing its contents.  When  you've found a song you'd like
to add the queue, you can do  it in the usual fashion, using the \"+\"
or \"pre\" commands,  or by abusing the \"repeat\"  command (this will
add the current song to the head of the queue).

In  looping  mode, a  song  which plays  to  completion  (and was  not
originally  started  from  the queue)  is  added  to  the end  of  the
queue. This provides another way to extend the queue while auditioning
songs -  if you allow the  song to play to  completion, presumably you
want to add it to the queue.
"))

(defun print-alarm-help ()
  (format t "
The \"alarm\" command provides an  alarm clock feature which will play
music when  the scheduled  wakeup time is  reached. There is  a single
wakeup time, and  when it is reached the wakeup  time is cleared. When
the alarm is triggered, the music player will do one of the following:

  1) If playback is paused, unpause the player.
  2) Otherwise, prepend ten random songs to queue and play them.

With  no argument,  the \"alarm\"  command prints  the  current wakeup
time. An argument  to the command specifies the  wakeup time. This can
be done in a variety of formats:

  alarm at 7:45 am      # \"at\" is optional and doesn't change the meaning
  alarm 7:45 am
  alarm 9 pm
  alarm 7               # If AM/PM not specified, assumes AM
  alarm in 5 minutes    # Relative alarm times, in minutes or hours
  alarm in 10m          # minutes, minute, mins, min, , m are synonyms
  alarm in 7 hours      # hours, hour, hr, h are synonyms
  alarm in 8h
  alarm in 7:29         # h:mm format - seven hours, twenty-nine minutes
  alarm reset           # off/never/delete/disable/cancel/clear/reset

If the player is already playing when the alarm goes off, the song
already playing will be interrupted by the next song in the queue.
"))

;;; TODO: Somewhere, the help should mention the 'ignore' tag, but
;;; there's currently no prose dedicated to tagging where it would
;;; make sense to mention it.

(defun eval* (string)
  "Read a form from STRING, evaluate it in the Shuffletron
  package and print the result."
     (print (eval (let ((*package* (find-package :shuffletron)))
                    (read-from-string string))))
     (terpri))

(defun parse-and-execute (line) 
 (let* ((sepidx (position #\Space line))
        (command (subseq line 0 sepidx))
        (args    (and sepidx (string-trim " " (subseq line sepidx)))))
  (cond

    ;; Back: restore previous selection.
    ;; A blank input line is a synonym for the "back" command.
    ((or (emptyp line) (string= line "back"))
     (cond
       (*selection-history* (setf *selection* (pop *selection-history*)))
       (t (reset-query))))

    ;; Lisp evaluation
    ((and *eval-support* (string= command "eval"))
     (eval* args))

    ((and (eq *eval-support* 'smart) (equal (subseq line 0 1) "("))
     (eval* line))

    ;; Input starting with a forward slash refines the current query.
    ((char= (aref line 0) #\/) (refine-query (subseq line 1)))

    ;; Show all matches
    ((or (string= line "show") (string= line "ls")) 
     (show-current-query))

    ;; Quit
    ((or (string= line "quit") (string= line "exit")) (quit))     

    ;; Play songs now (first is played, subsequent are added to queue
    ((digit-char-p (aref line 0))
     (play-songs (selection-songs line)))

    ;; Play all songs now
    ((string= line "all")
     (play-songs *selection*))

    ;; Append songs and end of playqueue
    ((and (> (length line) 1) (char= #\+ (aref line 0)))
     (with-playqueue ()
       (setf *playqueue* (concatenate 'list 
                                      *playqueue*
                                      (selection-songs (subseq line 1)))))
     (unless (current-song-playing) (play-next-song)))

    ;; Prepend songs to playqueue
    ((and (>= (length line) 4)
          (string= "pre" (subseq line 0 3))
          (or (digit-char-p (aref line 3))
              (char= #\Space (aref line 3))))
     (with-playqueue () 
       (setf *playqueue* (concatenate 'list 
                                      (selection-songs (subseq line 3))
                                      *playqueue*)))
     (unless (current-song-playing) (play-next-song)))

    ;; Skip current song
    ((or (string= line "next") (string= line "skip"))
     (play-next-song)
     (show-current-song))

    ;; Pause playback
    ((string= line "pause") 
     (toggle-pause)
     (update-status-bar))

    ;; Stop
    ((string= line "stop")
     (stop-command))

    ;; Play
    ((string= line "play")
     (play-command))

    ;; Seek
    ((string= command "seek") (do-seek args))

    ;; Start at
    ((string= command "startat")
     (let* ((time (and args (parse-timespec args)))
            (playing (current-song-playing))
            (cur-start (and playing (song-start-time playing))))
       (cond
         ((and cur-start (null time))
          (format t "Start time for the current song is ~A~%" 
                  (time->string cur-start)))
         ((and playing (null time))
          (format t "No start time for this song is set.~%"))
         ((not playing) (format t "No song is playing.~%"))
         ((null time) (format t "Set start time to when?~%"))         
         (t (setf (song-start-time playing) time)))))

    ;; Random
    ((string= line "random")
     (cond
       ((emptyp *filtered-library*) (format t "The library is empty.~%"))
       ((emptyp *library*) (format t "All songs in the library are ignored.~%"))
       (t (play-song (alexandria:random-elt (if (emptyp *selection*) *filtered-library* *selection*)))))
     (show-current-song))

    ;; Random from query
    ((string= command "random")
     (let ((matches (query args)))
       (cond
         ((emptyp matches) 
          (format t "No songs match query.~%"))
         (t (play-song (alexandria:random-elt matches))
            (show-current-song)))))

    ;; Show playqueue
    ((string= line "queue") (show-playqueue))

    ;; Show current song
    ((string= line "now") (show-current-song))

    ;; Add tags to current file
    ((and (string= command "tag") args)
     (tag-current-song args))

    ;; Show tags
    ((and (string= command "tag") (null args))
     (show-current-song-tags))

    ;; Add tag to all songs in selection
    ((string= command "tagall")
     (let ((tags (parse-tag-list args)))
       (cond
         ((null tags) (format t "~&Apply which tags to selected songs?~%"))
         (t (dolist (tag tags) (tag-songs *selection* tag))))))

    ;; Remove tag from all songs in selection
    ((string= command "untagall")
     (let ((tags (parse-tag-list args)))
       (cond
         ((null tags) (format t "~&Remove which tags from selected songs?~%"))
         (t (dolist (tag tags) (untag-songs *selection* tag))))))

    ;; Remove tags from current file
    ((string= command "untag")
     (untag-current-song args))

    ;; Show songs matching tag(s)
    ((string= command "tagged")
     (if args 
         (set-selection (songs-matching-tags (parse-tag-list args)))
         (format t "Search for which tags?~%")))

    ;; List all tags. Also check for dirty tag files, in case another
    ;; process modified them.
    ((string= line "tags")
     (load-tags)
     (show-all-tags))

    ;; Remove all occurances of tag(s)
    ((string= command "killtag")
     (load-tags)
     (map nil #'kill-tag (parse-tag-list args)))

    ;; Clear the queue
    ((string= line "clear")
     (with-playqueue ()
       (setf *playqueue* nil)))

    ;; Remove songs from the queue
    ((string= line "qdrop")
     (queue-remove-indices (list (1- (length *playqueue*)))))

    ((string= command "qdrop")
     (queue-remove-indices
      (expand-ranges (parse-ranges args 0 (1- (length *playqueue*))))))

    ;; Tag all songs in queue
    ((string= command "qtag")
     (with-playqueue ()
       (dolist (tag (parse-tag-list args))
         (dolist (song *playqueue*)
           (tag-song song tag)))
       (format t "Tagged ~:D songs in queue: ~{~A~^, ~}~%" 
               (length *playqueue*) (mapcar #'decode-as-filename (parse-tag-list args)))))

    ;; Queue to selection
    ((string= line "fromqueue")
     (set-selection (coerce *playqueue* 'vector)))

    ;; Selection to queue
    ((string= line "toqueue")
     (with-playqueue ()
       (setf *playqueue* (coerce *selection* 'list))))


    ;; Randomize queue
    ((string= line "shuffle")
     (with-playqueue ()
       (setf *playqueue* (alexandria:shuffle *playqueue*))))

    ;; Repeat the current song
    ((= 6 (or (mismatch "repeat" line) 6))
     (let ((song (current-song-playing))
           (num (or (parse-integer (subseq line 6) :junk-allowed t) 1)))
       (cond
         ((< num 0) (format t "  A negative repeat count doesn't make sense.~%"))
         ((not song) (format t "  No song is playing!~%"))
         (song
          (format t "  Repeating ~D time~:P: ~A~%" num (song-local-path song))
          (with-playqueue ()
            (setf *playqueue* (nconc (make-list num :initial-element song)
                                     *playqueue*)))))))
    ;; Toggle loop mode
    ((string= line "loop")
     (setf *loop-mode* (not *loop-mode*))
     (format t "~&Loop mode ~A~%" (if *loop-mode* "enabled" "disabled")))

    ;; Print current time
    ((string= line "time") (print-time))

    ;; Set alarm clock
    ((string= command "alarm")
     (do-set-alarm args))

    ;; Help
    ((string= line "help") (print-help))

    ;; Help: Commands
    ((and (string= command "help")
          (equalp args "commands")) (print-commands))

    ;; Help: Examples
    ((and (string= command "help") 
          (equalp args "examples")) (print-examples))

    ;; Help: Looping
    ((and (string= command "help") 
          (equalp args "looping")) (print-loop-help))

    ;; Help: Alarms
    ((and (string= command "help") 
          (equalp args "alarms")) (print-alarm-help))

    ((string= command "help")
     (format t "Unknown help topic ~W~%" args))

    ;; Scan new ID3 tags
    ((string= line "scanid3")
     (scan-id3-tags :verbose t))

    ;; Clear and rescan ID3 tags
    ((string= line "rescanid3")
     (loop for song across *library* do (setf (song-id3-p song) nil))
     (scan-id3-tags :verbose t))

    ;; Attempt to start swank server, for development.
    ((string= line "swankme") 
     ;; Work around an SBCL feature(?) in embedded cores:
     #+SBCL (cffi:foreign-funcall "setenv" :string "SBCL_HOME" :string "/usr/local/lib/sbcl/" :int 0 :int)
     (asdf:oos 'asdf:load-op :swank)     
     (eval (read-from-string "(swank:create-server :port 0)")))

    ;; Toggle file prescanning
    ((string= line "prescan")
     (setf (pref "prescan") (not (pref "prescan" t)))
     (if (pref "prescan")
         (format t "~&Prescanning enabled. This ensures track lengths and seeks are accurate.~%")
         (format t "~&Prescanning disabled. This eliminates the delay when initially starting
playback, and is useful for slow disks or network file systems.~%")))

    ;; ???
    (t (format t "Unknown command ~W. Try 'help'.~%" line)))))

(defun mainloop ()
  (loop
   ;; Show the current query, if there aren't too many items:
   (when (and *selection-changed* (<= (length *selection*) *max-query-results*))
     (show-current-query))
   (setf *selection-changed* nil)
   ;;(update-status-bar)
   ;; Prompt
   (with-output ()
     (format t "~A> " (if (querying-library-p) 
                          "library" 
                          (format nil "~:D matches" (length *selection*))))
     (force-output))
   ;; Input
   (let ((line (getline)))
     (flet ((cmd () 
              (with-output ()
                (update-terminal-size)
                (parse-and-execute (string-trim " " line)))))
       (if *debug-mode*
           (cmd)
           (handler-case (cmd)
             (error (c) 
               (with-output ()
                 (format t "~&Oops! ~A~%" c)))))))))

(defun run ()
  (spooky-init)
  ;; (Don't) Clear the screen first:
  #+ONSECONDTHOUGHT (format t "~C[2J~C[1;1H" #\Esc #\Esc)
  (format t "~&This is Shuffletron ~A~%" *shuffletron-version*)
  #+SBCL (setf *argv* (rest sb-ext:*posix-argv*))
  #-SBCL (warn "*argv* not implemented for this CL implementation.")
  (init)
  (audio-init)
  (mainloop))

;;;; Experimental status bar code:

(defun save-cursor ()    (format t "~C[s" #\Esc))
(defun restore-cursor () (format t "~C[u" #\Esc))

(defun move-cursor (col row)
  (format t "~C[~D;~DH" #\Esc row col))

;;; Debugging cruft. There's a strange issue where the program freezes
;;; in the call to finish-output for a number of seconds (10?  20?) if
;;; the alarm thread attempts to redraw the status bar while the other
;;; thread is doing output. I tried to use the output lock to protect
;;; access to the terminal, but perhaps I missed something (or that
;;; isn't the problem).

(defvar *funcounter* 0)                 
(defvar *waitcounter* 0)
(defvar *donecounter* 0)

(defun update-status-bar ()
  ;; The status bar is now disabled.
  ;; To enable it, call DISPLAY-STATUS-BAR here.
  (values))

(defun display-status-bar ()
  (with-output ()
    (save-cursor)
    (update-terminal-size)
    (format t "~C[~D;~Dr" #\Esc 2 *term-rows*) ; Set scrolling window
    (format t "~C[6p" #\Esc)                   ; Hide cursor
    (move-cursor 1 1)
    (sgr '(44 97))
    (format t "~C[0K" #\Esc)              ; Clear line (sort of, not really)
    (let* ((count 1)
           (wakeup *wakeup-time*)
           (remaining (and wakeup (- wakeup (get-universal-time))))
           (need-seperator wakeup))

      (flet ((output (fmt &rest args)
               (let* ((string (apply #'format nil fmt args))
                      (nwrite (min (max 0 (- *term-cols* count))
                                   (length string))))
                 (incf count (length string))
                 (write-string (subseq string 0 nwrite)))))
        (cond
          ((and wakeup (< remaining 3600))
           (output "Alarm in ~D m" (round remaining 60)))
          (wakeup
           (output "Alarm in ~,1F hrs" (/ remaining 3600.0))))
        
        (let* ((stream *current-stream*)
               (song (and stream (song-of stream))))
          (when song
            (when need-seperator (output " | "))
            (let* ((id3 (song-id3 song))
                   (artist (getf id3 :artist))
                   (album (getf id3 :album))
                   (title (getf id3 :title)))
              (if (streamer-paused-p stream *mixer*)
                  (output "Paused: ")
                  (output "Playing: "))
              (cond
                ((and artist title)
                 (output "\"~A\" by " title)
                 (output "~A" artist)
                 (when (and album (< (+ 4 (length album))
                                     (- *term-cols* count)))
                   (output " (~A)" album)))
                (t (output "~A" (song-local-path song)))))))

        (when (= 1 count)
          (output "Shuffletron ~A" *shuffletron-version*))

        (loop while (< count *term-cols*) do (output " "))))

    (sgr '(0))
    (restore-cursor)
    (format t "~C[7p" #\Esc)                   ; Show cursor    
    (incf *funcounter*)
    (finish-output)
    (incf *waitcounter*))
  (incf *donecounter*))

;;; How the hell does a little status bar take that much code to paint?
