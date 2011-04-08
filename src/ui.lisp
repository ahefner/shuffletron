(in-package :shuffletron)

;;; Temporal coupling ==> Recursive lock as workaround ==> Interesting.
(defvar *output-lock* (bordeaux-threads:make-recursive-lock "Output Lock"))
(defmacro with-output (() &body body)
  `(bordeaux-threads:with-recursive-lock-held (*output-lock*)
     ,@body))

(defvar *term-rows* 80)
(defvar *term-cols* 25)
 
(defun get-terminal-size ()
  #-linux (values 80 25)                ; =)
  #+linux
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

(defun getline ()
  (finish-output *standard-output*)
  (or (read-line *standard-input* nil) (quit)))

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
              ((string= arg "--help")
               (print-usage-message)
               (quit))
              ((string= arg "--version")
               (format t "Shuffletron ~A" *shuffletron-version*)
               (quit))
              ((string= arg "--list")
               (let ((profiles (all-profiles)))
                 (cond
                   ((null profiles)
                    (format t "~&There are no profiles.~%"))
                   (t
                    (format t "~&  ~20A Path~%" "Name")
                    (format t "~&  ----------------------------------------------------------------------")
                    (loop for name in profiles
                          as path = (get-profile-base name) do
                          (format t "~&  ~20A ~A" name path)))))
               (quit))
              (t (format t "~&Unrecognized argument ~W.~%" arg)
                 (quit)))))))

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
        ;; It's possible these can be NIL if we're racing against the startup
        ;; of a stream with an error
        (when (and pos len)
          (format t "[~A/~A] ~A ~A~%"
                  (time->string (round pos (mixer-rate *mixer*)))
                  (time->string (round len (mixer-rate *mixer*)))
                  (if (streamer-paused-p current *mixer*)
                      "Paused:"
                      "Playing:")
                  (song-local-path song))))
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
