(in-package :shuffletron-www)

(defvar *server* (hunchentoot:start-server :port 4242))

(defmacro with-html ((title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (:html
      (:head (:title ,title))
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
          (:tr (:td "/home/hefner/cl/shuffletron/extra/web/Position")
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
                   (when (getf (song-id3 song) key)
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
  (with-html-output (*standard-output*)
    (:table
     (:tr
      (when controls
        (htm (:td "")))      
      (:td "Artist") (:td "Album") (:td "Track") (:td "Title") (:td "Genre"))
     (loop for song in songs
           for n upfrom 0
           as id3 = (song-id3 song)
           as prev = (vector nil nil nil nil)
           as fields = (loop for key in '(:artist :album :track :title :genre)
                             collect (getf id3 key))
           as title = (elt fields 3)
           as artist = (elt fields 0)
           as sufficient = (and title artist)
           do
           (htm
            (:tr
             (when controls
               (htm
                (:td (:a :href (format nil "~Aplay=~D" controller n)
                         "Play"))))
             (cond 
               ((not sufficient)
                (htm (:td :colspan 5 (esc (song-local-path song)))))
               (t
                (loop for field in fields do
                      (htm
                       (:td (esc (princ-to-string (or field ""))))))))))
           ))))

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
    (with-html ("Shuffletron")
      (cond
        (song
         (present-song song :streamer current))
        (t (htm (:p "No song playing."))))
      (present-playqueue))))

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
    (term tagged
     (id   :parameter-type 'integer)
     (play :parameter-type 'integer))
  (let* ((*session*   (ensure-session id))
         (*selection* (songs-of *session*))
         (old-selection *selection*)
         (play-song (safe-vref *selection* play)))
    (when play-song (play-song play-song))
    (with-html ("Shuffletron")
      (:h1 "Search Library")
      ;; Establish new session, applying search term.
      (unless (emptyp term)
        (refine-query term)
        (when (emptyp *selection*)          
          (htm (:p "No matches searching for \"" (esc term) "\""))))
      (unless (eq *selection* old-selection)
        (setf *session* (new-session *selection*)))

      ;; Present UI
      (:p (:form :method :get
                 "Find"
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

(setf *dispatch-table*
      (list 'dispatch-easy-handlers))

(setf hunchentoot:*show-lisp-backtraces-p* t)
(setf hunchentoot:*show-lisp-errors-p* t)
