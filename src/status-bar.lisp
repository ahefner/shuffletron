(in-package :shuffletron)

;;;; Experimental status bar plugin:

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

(defclass status-bar () ())

(defmethod extending-display-prompt :after ((app status-bar))
  (display-status-bar))

(defmethod extending-wait-for-alarm :after ((app status-bar) interval)
  (declare (ignore interval))
  (display-status-bar))

(defmethod extending-toggle-pause :after ((app status-bar))
  (display-status-bar))

(defmethod extending-unpause :after ((app status-bar))
  (display-status-bar))

(defmethod extending-play-song :after ((app status-bar) song)
  (declare (ignore song))
  (display-status-bar))

(defmethod extending-end-stream :after ((app status-bar) stream)
  (declare (ignore stream))
  (display-status-bar))




