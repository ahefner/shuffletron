(defpackage :shuffletron
  (:use :common-lisp :mixalot)
  (:export #:run #:*shuffletron-version*
           #:emptyp
           #:walk #:relative-to #:join-paths
           #:*profile* #:pref #:prefpath
           #:*library* #:*filtered-library* #:*library-base*
           #:song #:song-full-path #:song-local-path #:song-tags
           #:song-properties #:song-id3 #:song-id3-p
           #:song-start-time
           #:songs-matching-tags #:songs-matching-tag
           #:tag-song #:untag-song
           #:decode-as-filename #:encode-as-filename
           #:*selection* #:selection-history*
           #:querying-library-p #:set-selection
           #:reset-query #:refine-query #:query
           #:with-stream-control #:with-playqueue #:when-playing
           #:*mixer* #:*current-stream* #:*playqueue*
           #:song-of #:stopped
           #:*loop-mode* #:*wakeup-time*
           #:finish-stream
           #:play-song #:play-songs #:add-songs #:play-next-song #:skip-song
           #:play-command #:stop-command
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
           #:parse-and-execute

           ;; Extensible by plug-ins:
           #:tag-songs #:untag-songs
           #:extending-tag-songs #:extending-untag-songs

           #:redisplay #:read-command #:display-prompt
           #:extending-redisplay #:extending-read-command #:extending-display-prompt

           #:toggle-pause #:extending-toggle-pause
           #:unpause #:extending-unpause

           #:trigger-alarm #:extending-trigger-alarm
           #:wait-for-alarm #:extending-wait-for-alarm

           #:play-song #:extending-play-song
           #:end-stream #:extending-end-stream

           #:note-audio-written #:extending-note-audio-written

           ;; Help functions (for extension by plugins)
           #:print-commands
           #:print-help

           ;; Plugin framework:
           #:plugin-enabled #:plugin-disabled
           #:enabled-plugins
           #:apply-configuration
           #:enable-plugin
           #:disable-plugin
           #:*application*
           #:defun-extensible))
