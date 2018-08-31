(in-package :shuffletron)

(defun configure-library-path ()
  "Prompt user and verify library path"
  (init-library)
  (block nil
    (let ((path *library-base*))
     (unless (and (stringp path)
                  (> (length path) 0))
       (format t "~&Enter library path: ")
       (setf path (join-paths (getline) "")))
     (when (and path (null (probe-file path)))
       (format t "~&Can't open directory ~A~%" path)
       (return))
     (setf path (osicat-sys:native-namestring (probe-file path)))
     (when (and path (not (library-scan path)))
       (format t "Unable to scan \"~A\"~%" path)
       (return))
     (when (emptyp *library*)
       (format t "No playable files found in \"~A\"~%" path))
     ;; Success!
     path)))

(defun init ()
  (setf *package* (find-package :shuffletron))
  (parse-command-line-args)
  (format t "~&This is Shuffletron ~A~%" *shuffletron-version*)
  (setf *random-state* (make-random-state t))
  (loop
     do (setf *library-base* (configure-library-path))
     until *library-base*)
  (format t "~CLibrary contains ~:D files.        ~%"
          (code-char 13) (length *library*))
  (load-tags)
  (compute-filtered-library)
  (load-metadata-cache)
  (reset-query)
  ;; Scan tags of new files automatically, unless there's a ton of them.
  (let ((need (songs-needing-id3-scan)))
    (cond
      ((zerop need))
      ((> need 1000)
       (format t "~:D new songs need to be scanned for ID3 tags. To do this now,
type \"scanid3\". It may take a moment.~%"
               (songs-needing-id3-scan)))
      (t (scan-file-metadata :verbose t :adjective "new ")))))

(defun spooky-init ()
  (let ((stream #+sbcl (sb-sys:make-fd-stream 1 :external-format :latin1 :output t :input nil)
                #+ccl (ccl::make-fd-stream 1 :direction :io :sharing :lock :encoding :iso-8859-1)))
    (setf *standard-output* stream)
    (setf *error-output*    stream)
    #+sbcl
    (sb-sys:enable-interrupt
     sb-unix:sigint
     (lambda (&rest args)
       (declare (ignore args))
       (sb-ext:exit :abort t)))))

(defun quit ()
;;  (format t "Bye.~%")
  (finish-output)
  #+sbcl (sb-ext:exit :abort t)
  #+ccl (ccl:quit))

(defun eval* (string)
  "Read a form from STRING, evaluate it in the Shuffletron
  package and print the result."
  (print (eval (read-from-string string)))
  (terpri))

(defun show-current-query ()
  (if (emptyp *selection*)
      (format t "  Nothing matches the current query.~%")
      (show-song-matches *selection* :mode :query :highlight-queue t)))

(defun do-seek (args)
  (let* ((current *current-stream*)
         (mode-char (and args (find (elt args 0) "+-")))
         (time-arg (if mode-char
                       (string-trim " " (subseq args 1))
                       args))
         (seconds (and args (parse-timespec time-arg)))
         (samples (and seconds (* (mixer-rate *mixer*) seconds)))
         (base (if (not mode-char)
                   samples
                   (and current (streamer-position current *mixer*))))
         (offset (cond ((eql mode-char #\+) samples)
                       ((eql mode-char #\-) (- samples))
                       (t 0)))
         (time (and base offset (+ base offset))))
    (cond
      ((null current) (format t "No song is playing.~%"))
      ((null time) (format t "Seek to where?~%"))
      (time (streamer-seek current *mixer* (max time 0)))
      (t nil))))

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
    ((string= command "eval")
     (eval* args))

    ((equal (subseq line 0 1) "(")
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
     (add-songs (selection-songs (subseq line 1))))

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

    ;; Skip current song. If looping, don't play this again.
    ((string= line "skip")
     (skip-song)
     (show-current-song))

    ;; Advance to next song in queue. Differs from 'skip' only if
    ;; looping is enabled: whereas 'skip' drops the song from the
    ;; queue, 'next' puts it at the end of the queue.
    ((string= line "next")
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

    ;; Add/play in random order
    ((and (string= command "shuffle") args)
     (add-songs (alexandria:shuffle (copy-seq (selection-songs args)))))

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
     (format t "Loop mode ~A~%" (if *loop-mode* "enabled" "disabled")))

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

    ;; Help: Alarms
    ((and (string= command "help")
          (equalp args "alarms")) (print-alarm-help))

    ((string= command "help")
     (format t "Unknown help topic ~W~%" args))

    ;; Scan new ID3 tags
    ((string= line "scanid3")
     (scan-file-metadata :verbose t))

    ;; Clear and rescan ID3 tags
    ((string= line "rescanid3")
     (loop for song across *library* do (setf (song-id3-p song) nil))
     (scan-file-metadata :verbose t))

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
  (mixalot:main-thread-init)
  ;; (Don't) Clear the screen first:
  #+ONSECONDTHOUGHT (format t "~C[2J~C[1;1H" #\Esc #\Esc)
  #+SBCL (setf *argv* (rest sb-ext:*posix-argv*))
  #-SBCL (warn "*argv* not implemented for this CL implementation.")
  (init)
  (audio-init)
  (mainloop))
