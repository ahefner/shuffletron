;;; I'd intended to use the standard stream classes behind a proxy stream, but it
;;; turns out that there isn't a good way to know when the underlying stream has
;;; finished playing. Making this work requires some slight design changes to Mixalot
;;; which I'm not going to undertake presently. As a workaround, we can just define
;;; a subclass of each Mixalot stream type we're using.

;;; This is how it would look. This almost works, except streamer-cleanup never runs.

(defclass song-streamer ()
  ((song :accessor song-of :initarg :song)
   (stopped :accessor stopped :initform nil)
   (underlying-streamer :accessor underlying-streamer :initarg :underlying-streamer)))

(defun make-streamer (song)
  (make-instance 'song-streamer
                 :song song
                 :underlying-streamer
                 (ecase (music-file-type (song-full-path song))
                   (:mp3 (make-mp3-streamer (song-full-path song) :prescan (pref "prescan" t)))
                   (:ogg (mixalot-vorbis:make-vorbis-streamer (song-full-path song)))
                   (:flac (mixalot-flac:make-flac-streamer (song-full-path song))))))

(defmethod streamer-mix-into ((stream song-streamer) mixer buffer offset length time)
  (streamer-mix-into (underlying-streamer stream) mixer buffer offset length time))

(defmethod streamer-cleanup ((stream song-streamer) mixer)
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
    ;; Seems prudent to do the cleanup of the underlying streamer in
    ;; the new thread as well.
    (bordeaux-threads:make-thread
     (lambda ()
       (streamer-cleanup (underlying-streamer stream) mixer)
       (play-next-song)))))

(defmethod streamer-seekable-p ((stream song-streamer) mixer)
  (streamer-seekable-p (underlying-streamer stream) mixer))

(defmethod streamer-length ((stream song-streamer) mixer)
  (streamer-length (underlying-streamer stream) mixer))

(defmethod streamer-seek ((stream song-streamer) mixer position &key)
  (streamer-seek (underlying-streamer stream) mixer position))

(defmethod streamer-position ((stream song-streamer) mixer)
  (streamer-position (underlying-streamer stream) mixer))
