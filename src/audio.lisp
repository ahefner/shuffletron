(in-package :shuffletron)

(defvar *mixer* nil)

(defclass shuffletron-stream-mixin ()
  ((song :accessor song-of :initarg :song)
   (stopped :accessor stopped :initform nil)))

;;; Should use a proxy approach, so we don't have to define N
;;; subclasses.  Won't work without some small changes to Mixalot -
;;; presently, a proxying stream can't tell when the proxied stream is
;;; finished. Fix later.

(defclass shuffletron-mp3-stream  (mixalot-mp3:mp3-streamer shuffletron-stream-mixin) ())
(defclass shuffletron-ogg-stream  (mixalot-vorbis:vorbis-streamer shuffletron-stream-mixin) ())
(defclass shuffletron-flac-stream (mixalot-flac:flac-streamer shuffletron-stream-mixin) ())

(defun audio-init ()
  (setf *mixer* (create-mixer :rate 44100)))

;;; Lock discipline: If you need both locks, take the playqueue lock first.
;;; This locking business is very dodgy.

(defvar *cslock* (bordeaux-threads:make-lock "current stream"))
(defvar *i-took-the-cs-look* nil)
(defvar *current-stream* nil)

(defmacro with-stream-control (() &body body)
  `(let ((*i-took-the-cs-look* t))
     (bordeaux-threads:with-lock-held (*cslock*) ,@body)))

(defmacro when-playing ((name) &body body)
  `(let ((,name *current-stream*))
     (when ,name ,@body)))

(defvar *pqlock* (bordeaux-threads:make-lock "play queue"))
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

(defun-extensible end-stream (stream)
  ;; TODO: Fade out nicely.
  (setf (stopped stream) t)
  (mixer-remove-streamer *mixer* stream)
  (setf *current-stream* nil))

(defun finish-stream (stream)
  (when *loop-mode*
    (with-playqueue ()
      (setf *playqueue* (append *playqueue* (list (song-of stream))))))
  (end-stream stream))

(defun make-streamer (song)
  (ecase (music-file-type (song-full-path song))
    (:mp3  (mixalot-mp3:make-mp3-streamer
            (song-full-path song)
            :class 'shuffletron-mp3-stream
            :song song
            :prescan (pref "prescan" t)))
    (:ogg  (mixalot-vorbis:make-vorbis-streamer
            (song-full-path song)
            :class 'shuffletron-ogg-stream
            :song song))
    (:flac (mixalot-flac:make-flac-streamer
            (song-full-path song)
            :class 'shuffletron-flac-stream
            :song song))))

(defun-extensible play-song (song)
  "Start a song playing, overriding the existing song. Returns the new
stream if successful, or NIL if the song could not be played."
  (when-playing (stream) (end-stream stream))
  (prog1
      (handler-case
          (with-stream-control ()
            (when *current-stream* (finish-stream *current-stream*))
            (let ((new (make-streamer song))
                  (start-at (song-start-time song)))
              (setf *current-stream* new)
              (mixer-add-streamer *mixer* *current-stream*)
              (when start-at
                ;; Race conditions are fun.
                (streamer-seek new *mixer* (* start-at (mixer-rate *mixer*))))
              ;; Success: Return the streamer.
              new))
        (error (err)
          (princ err)
          ;; Failure: Return NIL.
          nil))))

(defun play-songs (songs)
  "Prepend songs to the queue and play the first one immediately."
  (when-playing (stream) (end-stream stream))
  (with-playqueue ()
    (setf *playqueue* (concatenate 'list songs *playqueue*)))
  (play-next-song))

(defun add-songs (songs)
  "Append songs to queue, beginning playback if stopped."
  (with-playqueue ()
       (setf *playqueue* (concatenate 'list *playqueue* songs)))
     (unless (current-song-playing) (play-next-song)))

(defun skip-song ()
  (when-playing (stream) (end-stream stream))
  (play-next-song))

(defun play-next-song ()
  ;; If a song is playing, finish it first. This will ensure that a
  ;; looping song is put back on the (potentially empty) queue before
  ;; we try to pop the next song from the queue.
  (when-playing (stream) (finish-stream stream))
  (with-playqueue ()
    (cond
      (*playqueue*
       ;; Try songs from the queue until one starts successfully.
       ;; In loop mode, songs that fail to start are dropped from the queue.
       (loop as next = (pop *playqueue*)
             until (or (null next) (play-song next))))
      ;; If there's no song in the queue, finish the current song.
      (t (with-stream-control ()
           (when *current-stream* (finish-stream *current-stream*)))))))


(defmethod streamer-cleanup :after ((stream shuffletron-stream-mixin) mixer)
  (declare (ignore mixer))
  ;; The STOPPED flag distinguishes whether playback was interrupted
  ;; by the user, versus having reached the end of the song. If we're
  ;; supposed to loop, this determines who is responsible for making
  ;; that happen.
  (when (and *loop-mode* (not (stopped stream)))
    (with-playqueue ()
      (setf *playqueue* (append *playqueue* (list (song-of stream))))))
  ;; If stopped is set, someone else can be expected to start up the
  ;; next song. Otherwise, we have to do it ourselves.
  (unless (stopped stream)
    ;; If the song completed:
    (with-stream-control ()
      (when (eq stream *current-stream*)
        (setf *current-stream* nil)))
    ;; We do this call in new thread, because we are in the mixer
    ;; thread here, and scanning the next file could take long enough
    ;; to stall it.
    (bordeaux-threads:make-thread
     (lambda () (play-next-song)))))

(defun-extensible toggle-pause ()
  (with-stream-control ()
    (when *current-stream*
      (if (streamer-paused-p *current-stream* *mixer*)
          (streamer-unpause *current-stream* *mixer*)
          (streamer-pause *current-stream* *mixer*)))))

(defun-extensible unpause ()
  (with-stream-control ()
    (when *current-stream*
      (when (streamer-paused-p *current-stream* *mixer*)
        (streamer-unpause *current-stream* *mixer*)
        t))))

(defun current-song-playing ()
  (when-playing (stream) (song-of stream)))

(defun playqueue-and-current ()
  (let ((song (current-song-playing)))
    (if song
        (cons song *playqueue*)
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

(defun stop-command ()
  (with-playqueue ()
    (with-stream-control ()
      (when *current-stream*
        ;; Put a stopped song on the head of the queue, so that a
        ;; subsequent 'play' will start it from the beginning.
        (push (song-of *current-stream*) *playqueue*)
        (end-stream *current-stream*)))))

(defun play-command ()
  (unless (unpause)
    (or (current-song-playing) (play-next-song))))
