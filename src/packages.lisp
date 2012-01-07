(defpackage :shuffletron
  (:use :common-lisp :mixalot)
  (:nicknames :shuf)
  (:export #:run #:*shuffletron-version*
           #:emptyp
           #:walk #:rel #:dfn
           #:*profile* #:pref #:prefpath
           #:*library* #:*filtered-library* #:*library-base*
           #:song #:song-full-path #:song-local-path #:song-tags
           #:song-properties #:song-id3 #:song-id3-p
           #:song-start-time
           #:songs-matching-tags #:songs-matching-tag
           #:tag-songs #:tag-song #:untag-songs #:untag-song
           #:decode-as-filename #:encode-as-filename
           #:*selection* #:selection-history*
           #:querying-library-p #:set-selection
           #:reset-query #:refine-query #:query
           #:with-stream-control #:with-playqueue #:when-playing
           #:*mixer* #:*current-stream* #:*playqueue*
           #:song-of #:stopped
           #:*loop-mode* #:*wakeup-time*
           #:end-stream #:finish-stream
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
           #:parse-and-execute))
