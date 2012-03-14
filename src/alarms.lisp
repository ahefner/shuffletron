(in-package :shuffletron)

;;; Time parsers:

(defun parse-timespec (string)
  "Parse a time/duration, in one of three formats (seconds, m:ss, h:mm:ss)"
  (disjunction (string)
    ;; Seconds format (a single integer):
    (prog1 (num in) (eof in))
    ;; mm:ss format (seconds must be modulo 60):
    (+ (* 60 (prog1 (num in)   (colon in)))
       (*  1 (prog1 (mod60 in) (eof in))))
    ;; h:mm:ss format (minutes and seconds must be modulo 60):
    (+ (* 3600 (prog1  (num in)  (colon in)))
       (*   60 (prog1 (mod60 in) (colon in)))
       (*    1 (prog1 (mod60 in) (eof in))))))

(defun 12hour (in) (let ((n (num in))) (val (and (< 0 n 13) (mod n 12)))))

(defun parse-12-hour-format (in)
  "Parse numeric portions (hour or h:mm) of 12-hour time format, returning a count in minutes."
  (val
   (disjunction ()
     ;; h:mm format
     (+ (prog1 (* 60 (12hour in)) (colon in))
        (mod60 in))
     ;; Bare time in hours:
     (* 60 (12hour in)))))

(defun parse-daytime (in)
  "Parse string as a time of day (AM/PM), for the alarm
clock. Returns time in minutes from midnight."
  (disjunction ()
    ;; If there's no time, default to AM
    (prog1 (parse-12-hour-format in) (eof in))
    ;; AM time
    (prog1 (parse-12-hour-format in)
      (whitespace in)
      (disjunction () (match in "a.m.") (match in "am.") (match in "am"))
      (eof in))
    ;; PM time
    (+ (prog1 (parse-12-hour-format in)
         (whitespace in)
         (disjunction () (match in "p.m.") (match in "pm.") (match in "pm"))
         (eof in))
       720)))

(defun utime->string (utime)
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time utime)
    (declare (ignore second))
    (format nil "~A ~A ~D ~D:~2,'0D ~A ~D"
            (nth day '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
            (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
            date
            (1+ (mod (1- hour) 12))
             minute
             (if (>= hour 12) "PM" "AM")
             year)))

(defun print-time ()
  (format t "~A~%" (utime->string (get-universal-time))))

(defun daytime->alarm-time (daytime)
  "Translate a daytime (in minutes) to a universal time for the
alarm. Since the daytime doesn't specify a date, we choose tomorrow
rather than today if the date would be less than the current time."
  (let* ((current-time (get-universal-time))
         (decoded (multiple-value-list (decode-universal-time current-time)))
         (minutes (second decoded))
         (hours   (third decoded))
         (current (+ minutes (* hours 60))))
    (cond
      ((< current daytime)
       (encode-universal-time 0 (mod daytime 60) (truncate daytime 60)
                              (fourth decoded) (fifth decoded) (sixth decoded)))
      (t (multiple-value-bind (s m h date month year) ; Get tomorrow's date.
             (decode-universal-time (+ current-time 86400))
           (declare (ignore s m h))
           (encode-universal-time 0 (mod daytime 60) (truncate daytime 60)
                                  date month year))))))

(defun parse-relative-time (in)
  (disjunction ()
    ;; Time in minutes:
    (prog1 (* 60 (num in))
      (whitespace in)
      (val (disjunction ()
             (match in "minutes") (match in "minute")
             (match in "mins") (match in "min") (match in "m")))
      (eof in))
    ;; Time in hours:
    (prog1 (* 3600 (num in))
      (whitespace in)
      (val (disjunction ()
             (match in "hours") (match in "hour")
             (match in "hr") (match in "h"))))
    ;; Time in h:mm format:
    (+ (* 3600 (prog1 (num in) (colon in)))
       (*   60 (mod60 in)))))

(defun parse-alarm-args (args)
  "Parse the arguments to the alarm command, returning NIL or a universal time."
  (disjunction (args)
    ;; State the time directly:
    (daytime->alarm-time (val (parse-daytime in)))
    ;; Syntactic sugar for stated time:
    (and (match in "at ")
         (whitespace in)
         (daytime->alarm-time (val (parse-daytime in))))
    ;; Relative time offset:
    (and (match in "in ")
         (whitespace in)
         (+ (get-universal-time) (val (parse-relative-time in))))))

(defvar *alarm-thread* nil)

(defvar *wakeup-time* nil
  "Time to wake up if alarm clock is enabled.")

(defun-extensible trigger-alarm ()
  ;; When the alarm goes off, unpause the player if it's paused. If it
  ;; isn't paused but there are songs in the queue, play the next
  ;; song. If the queue is empty, queue up ten random songs and play
  ;; one.
  (setf *wakeup-time* nil)
  (unless (unpause)
    (with-playqueue ()
      (unless (or *playqueue* (emptyp *filtered-library*))
        (loop repeat 10 do (push (alexandria:random-elt *filtered-library*) *playqueue*))))
    (play-next-song)))

(defun-extensible wait-for-alarm (interval)
  (sleep interval))

(defun alarm-thread-toplevel ()
  (unwind-protect
       (loop with interval = 60
             as wakeup = *wakeup-time*
             as remaining = (and wakeup (- wakeup (get-universal-time)))
             do
             (cond
               ((null wakeup) (wait-for-alarm interval))
               ((<= remaining 0) (trigger-alarm))
               (t (wait-for-alarm (min remaining interval)))))
    (setf *alarm-thread* nil)))

(defun set-alarm (utime)
  (setf *wakeup-time* utime)
  (unless *alarm-thread*
    (setf *alarm-thread* (bordeaux-threads:make-thread #'alarm-thread-toplevel))))

(defun do-set-alarm (args)
  (cond
    ((null args)
     (let ((wakeup *wakeup-time*))
       (if wakeup
           (format t "Alarm set for ~A~%" (utime->string wakeup))
           (format t "The alarm is not set.~%"))))
    ((member args '("off" "never" "delete" "disable" "cancel" "clear" "reset") :test #'string-equal)
     (setf *wakeup-time* nil)
     (format t "Disabled alarm.~%"))
    (t (let ((time (parse-alarm-args args)))
         (cond
           ((null time) (format t "Unable to parse as time: ~W~%" args))
           (t (set-alarm time)
              (format t "Alarm set for ~A~%" (utime->string time))))))))
