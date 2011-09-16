(in-package :shuffletron-www)

(defvar *server* (hunchentoot:start-server :port 4242))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *jqui-theme-name* "dark-hive"))

(defmacro with-html ((title &key refresh) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     ,(and refresh '(:head (:meta :http-equiv "refresh" :content "2")))
     (:html
      (:head (:title ,title)
             (:link :type "text/css" :href "static/style.css" :rel "Stylesheet")
             (:link :type "text/css"
                    ;; It's really stupid that jQuery UI puts the version number in the
                    ;; CSS file name. I've renamed mine. Probably a bad idea.
                    :href (format nil "static/~A/jquery-ui.custom.css" *jqui-theme-name*)
                    :rel "Stylesheet")
             (:script :type "text/javascript" :src "static/jquery.js")
             (:script :type "text/javascript" :src "static/jquery-ui.js")
             (:script :type "text/javascript" :src "static/ui.js")
             (:script :type "text/javascript"
"
"))
      (:body ,@body))))

(defun present-song (song &key streamer)
  (let* ((start-time (song-start-time song))
         (tags (song-tags song)))
    (with-html-output (*standard-output*)
      (when song
        (htm
         (:h1 "Now Playing")
         (:table
          (:tr (:td "File")
               (:td (esc (format nil "~A" (song-local-path song)))))
          (:tr (:td "Position")
               (:td (format t "~A of ~A"
                            (time->string
                             (round
                              (mixalot:streamer-position streamer *mixer*)
                              (mixalot:mixer-rate *mixer*)))
                            (time->string
                             (round
                              (mixalot:streamer-length streamer *mixer*)
                              (mixalot:mixer-rate *mixer*))))))
          (when start-time
            (htm
             (:tr (:td "Skip Intro")
                  (:td (esc (time->string start-time))))))
          (flet ((field (name key)
                   (if (getf (song-id3 song) key)
                     (htm
                      (:tr (:td (esc name))
                           (:td (esc (princ-to-string
                                      (getf (song-id3 song) key)))))))))
            (field "Artist"  :artist)
            (field "Album"   :album)
            (field "Title"   :title)
            (field "Track"   :track)
            (field "Genre"   :genre)
            (field "Comment" :comment))
          (when tags
            (htm
             (:tr
              (:td "Tagged")
              (:td (esc (format t "~{~A~^, ~}"
                                (mapcar #'decode-as-filename
                                        tags)))))))))))))

(defun present-songs (songs &key controls controller)
  (declare (optimize (debug 3)))
  (with-html-output (*standard-output*)
    (:table
     (:tr
      (:td :class "colhead" "Artist")
      (:td :class "colhead" "Album")
      (when controls
        (htm (:td :class "DUMMY" "")))
      (:td :class "colhead" "Track")
      (:td :class "colhead" "Title")
      (:td :class "colhead" "Genre"))
     (loop with keys = '(:artist :album :track :title :genre)
           with artist-group-len = nil
           with album-group-len = nil
           for (song . rest) on songs
           for n upfrom 0
           as id3 = (song-id3 song)
           as prev = (vector nil nil nil nil)
           as fields = (loop for key in keys collect (getf id3 key))
           as title = (elt fields 3)
           as artist = (elt fields 0)
           as album = (elt fields 1)
           as sufficient = (and title artist)
           do
           (htm
            (:tr
             ;; Compute artist and album groupings
             (flet ((count-group (this-prop property)
                      (loop for other in rest
                         as other-prop = (getf (song-id3 other) property)
                         as matching = (and other-prop (string-equal other-prop this-prop))
                         summing (if matching 1 0) into num-matches
                         until (not matching)
                         finally
                         (htm (:td :class (symbol-name property) :rowspan (1+ num-matches)
                                   (esc this-prop)))
                         (return num-matches)))
                    (update-count (count)
                      (cond
                        ((eql 0 count) nil)
                        ((null count) nil)
                        (t (1- count))))
                    (draw-playback-control ()
                      (when controls
                        (htm
                         (:td
                          (:a :href (format nil "~Aplay=~D" controller n)
                              :class "PlayLink"
                              (:img :src "static/play.png" :alt "Play")))))))

               (when (or artist album)
                 (when (not artist-group-len)
                   (setf artist-group-len (count-group artist :artist)))

                 (when (not album-group-len)
                   (setf album-group-len (count-group album :album))))

               (setf artist-group-len (update-count artist-group-len))
               (setf album-group-len (update-count album-group-len))

               (draw-playback-control)

               (cond
                 ((not sufficient)

                  (htm (:td :class "FILENAME" :colspan 5 (esc (song-local-path song)))))
                 (t
                  (flet ((col (class prop)
                           (htm (:td :class class (esc (princ-to-string (or prop "")))))))

                    (col "TRACK" (elt fields 2))
                    (col "TITLE" (elt fields 3))
                    (col "GENRE" (elt fields 4))))))))))))

(defun present-playqueue ()
  (with-playqueue ()
    (let ((queue (copy-list *playqueue*)))
      (with-html-output (*standard-output*)
        (unless (emptyp queue)
          (htm (:h1 "Queue"))
          (present-songs queue))))))

(define-easy-handler (page/now :uri "/now")
    ()
  (let* ((current *current-stream*)
         (song (and current (song-of current))))
    (with-html ("Shuffletron" :refresh t)
      (cond
        (song
         (present-song song :streamer current))
        (t (htm (:p "No song playing."))))
      (present-playqueue))))

(define-easy-handler (page/status :uri "/status")
  ()
  (let* ((current *current-stream*)
         ;;(queue-size (with-playqueue () (length *playqueue*)))
         (song (and current (song-of current)))
         (id3 (and song (song-id3 song))))
    (with-html ("Shuffletron Status" :refresh t)
      (cond
        ((not song) (htm (:div "No song playing.")))
        (song
         (htm
          (:table :class "nowplaying"
           (htm (:tr (:td "Playing") (:td (esc (or (getf id3 :title) (song-local-path song))))))
           (when (getf id3 :artist)
             (htm (:tr (:td "Artist") (:td (esc (getf id3 :artist))))))
           (when (getf id3 :album)
             (htm (:tr (:td "Album") (:td (esc (getf id3 :album)))))))))))))

#+NIL
(define-easy-handler (page/tagged :uri "/tagged")
    (tag-name))

(defvar *session-map* (make-hash-table))
(defvar *next-session* 1)
(defvar *slock* (bordeaux-threads:make-lock "web session lock"))

(defun new-session-id ()
  (bordeaux-threads:with-lock-held (*slock*)
    (incf *next-session*)))

(defun find-session (id)
  (bordeaux-threads:with-lock-held (*slock*)
    (gethash id *session-map*)))

(defclass session ()
  ((songs :reader songs-of :initarg :songs)
   (id    :accessor session-id :initarg :id)))

(defun register-session (id session)
  (unless (numberp id) (setf id (new-session-id)))
  (bordeaux-threads:with-lock-held (*slock*)
    (setf (session-id session) id
          (gethash id *session-map*) session))
  (values session id))

(defun library-session ()
  (bordeaux-threads:with-lock-held (*slock*)
    (or (gethash 0 *session-map*)
        (setf (gethash 0 *session-map*)
              (make-instance 'session :id 0 :songs *filtered-library*)))))

(defun ensure-session (id?)
  (or (find-session id?) (library-session)))

(defun new-session (songs)
  (register-session nil (make-instance 'session :songs songs)))

(defvar *session* nil)

(defun safe-vref (vector index &optional default)
  (if (and (integerp index)
           (>= index 0)
           (< index (length vector)))
      (aref vector index)
      default))

(define-easy-handler (page/search :uri "/search")
    (term ;;tagged
     (id   :parameter-type 'integer)
     (play :parameter-type 'integer))

  (let* ((*session*   (ensure-session id))
         (*selection* (songs-of *session*))
         (old-selection *selection*)
         (play-song (safe-vref *selection* play)))
    (when play-song (play-song play-song))
    (format *trace-output* "----- SEARCH ~A ~A" term play)

    ;; Establish new session, applying search term.
    (unless (emptyp term)
      (refine-query term))

    (with-html ("Shuffletron")
      (:h1 (write-string (if (querying-library-p) "Search Library" "Search Results")))
      (unless (emptyp term)
        (when (emptyp *selection*)
          (htm (:p "No matches searching for \"" (esc term) "\""))))
      (unless (eq *selection* old-selection)
        (setf *session* (new-session *selection*)))

      ;; Present UI
      (:p (:form :method :get
                 (write-string (if (querying-library-p) "Find: " "Refine: "))
                 (:input :type :text :name "term")
                 (:input :type :hidden
                         :name "id"
                         :value (session-id *session*)))
          (:form :method :get
                 (:input :type :submit :name "reset" :value "Reset")
                 (:input :type :hidden
                         :name "id"
                         :value 0)))

      (if (< (length *selection*) 500)
          (present-songs (coerce *selection* 'list) :controls t
                         :controller (format nil "/search?id=~D&"
                                             (session-id *session*)))
          (htm
           (:p "Songs matching query: "
               (write (length *selection*))))))))

(define-easy-handler (page/tags :uri "/tags")
    ()
  (with-html ("Shuffletron")
    (:h1 "Tags")
    (:p
     (loop for (tag . count) in (tag-count-pairs *filtered-library*) do
           (htm (:b (esc (decode-as-filename tag)))
                (unless (eql count 1)
                  (htm (format t "(~:D)" count)))
                "  ")))))

;;;; Grand unified view

(define-easy-handler (page/master :uri "/")
  ()
  (with-html-output-to-string (*standard-output* nil :prologue t)
   (:html
    (:head (:title "Shuffletron"))
    (:frameset :rows "96,*"
     (:frame :name "header" :src "status" :noresize "noresize" :scrolling "no")
     (:frame :body "main"   :src "search" :scrolling "yes")))))



;;;; Testing Only

(define-easy-handler (page/foo :uri "/foo")
  ()
  (format *trace-output* "~&FOO v3 invoked.~%")
  (setf (content-type) "text/plain")
  "This is a test.")

(define-easy-handler (test/json1 :uri "/jsontest1")
  ()
  (let ((random (random 1000000)))
   (format *trace-output* "~&jsontest1 has a random number, ~A~%" random)
   (setf (content-type) "text/plain")
   (format nil "{ \"name\": \"JSON test\", \"value\":~D }" random)))





;;;; Server setup

(defvar *here*
  (load-time-value
   (make-pathname :name nil :type nil :version nil
                  :defaults (or #.*compile-file-pathname* *load-pathname*))))


(setf *dispatch-table*
      (list 'dispatch-easy-handlers
            (create-folder-dispatcher-and-handler "/static/" (merge-pathnames #p"static/" *here*))))

(setf hunchentoot:*show-lisp-backtraces-p* t)
(setf hunchentoot:*show-lisp-errors-p* t)

;;(hunchentoot:start-server :port 4242)
