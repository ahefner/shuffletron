(in-package :shuffletron)

(defvar *mixer* nil)

(defclass mp3-jukebox-streamer (mp3-streamer)
  ((song :accessor song-of :initarg :song)
   (stopped :accessor stopped :initform nil)
   (enqueue-on-completion :accessor enqueue-on-completion
                          :initform nil
                          :initarg :enqueue-on-completion)))

(defun audio-init ()
  (setf *mixer* (create-mixer :rate 44100)))

(defun stop-command ()
  (with-playqueue ()
    (with-stream-control ()
      (when *current-stream*
        (push (song-of *current-stream*) *playqueue*)
        (end-stream *current-stream*)))))

(defun play-command ()
  (unless (unpause)
    (or (current-song-playing) (play-next-song))))

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
