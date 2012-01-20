;;;; Based on Leslie Polzer's change-term-title plugin.

(in-package :shuffletron)

(defclass term-title () ())

(defun set-term-title (title)
  (format t "~C~C~C~C~A~C~%"
          (code-char #x1B)
          (code-char #x5D)
          (code-char #x30)
          (code-char #x3B)
          title
          (code-char #x07)))

(defmethod extending-play-song :after ((app term-title) song)
  (let* ((id3 (song-id3 song))
         (artist (getf id3 :artist))
         (title  (getf id3 :title)))
    (set-term-title
     (if (and artist title)
         (format nil "Playing \"~A\" by ~A" title artist)
         (format nil "Playing \"~A\"" (song-local-path song))))))

(defmethod extending-end-stream :after ((app term-title) stream)
  (declare (ignore stream))
  ;; FIXME: This doesn't work as planned, and end-stream probably
  ;; shouldn't be part of the plugin interface at all.  We need a
  ;; notification that a song has ended, and it shouldn't be running in
  ;; the mixer thread either.
  (set-term-title "Shuffletron"))
