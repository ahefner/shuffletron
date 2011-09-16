(in-package :shuffletron)

(defvar *selection* nil)
(defvar *selection-changed* nil)
(defvar *selection-history* nil)
(defvar *history-depth* 16)

(defun querying-library-p () (= (length *selection*) (length *filtered-library*)))

(defun set-selection (new-selection &key (record t))
  (when record
   (push *selection* *selection-history*)
   (when (> (length *selection-history*) *history-depth*)
     (setf *selection-history* (subseq *selection-history* 0 *history-depth*))))
  (setf *selection* new-selection
        *selection-changed* t)
  (values))

(defun reset-query ()
  (set-selection (copy-seq *filtered-library*) :record nil)
  (loop for x across *library* do (setf (song-matchprops x) nil)))

(defmacro any (&body forms)
  "Similar to the OR macro, but doesn't short-circuit."
  (let ((syms (loop repeat (length forms) collect (gensym "ANY"))))
    `((lambda ,syms (or ,@syms)) ,@forms)))

(defun do-query (substring update-highlighting)
  (declare (optimize (speed 3)))
  ;; Query and update highlighting:
  (loop for song across *selection*
        with query = (coerce (string-downcase substring) 'simple-string)
        with new-selection = (make-array 0 :adjustable t :fill-pointer 0)
        do
        (labels
            ((field (keyword)
               (let* ((string (if (eql keyword :filename)
                                  (song-local-path song)
                                  (getf (song-id3 song) keyword)))
                      (searchable (if (eql keyword :filename)
                                      (song-smashed song)
                                      string))
                      (found nil))
                 (when searchable
                   ;; There's no guarantee that this really holds...
                   (check-type searchable simple-string)
                   ;; The case insensitive search is painfully slow, so shortcut
                   ;; around it for filenames, which are already downcased.
                   (if (eql keyword :filename)
                       (setf found (search query searchable))
                       (setf found (search query searchable :key #'char-downcase))))
                 (when (and found update-highlighting)
                   (setf (getf (song-matchprops song) keyword)
                         (fill (the (simple-array bit 1)
                                 (or (getf (song-matchprops song) keyword)
                                     (make-array (length string) :element-type 'bit)))
                               1 :start found :end (+ found (length query)))))
                 found)))
          (when (any (field :filename)
                     (field :artist)
                     (field :album)
                     (field :title))
            (vector-push-extend song new-selection)))
        finally (return new-selection)))

(defun refine-query (substring)
  (set-selection (do-query substring t)))

(defun query (substring) (do-query substring nil))
