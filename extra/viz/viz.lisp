(defpackage :shuffletron-viz
  (:use :common-lisp
        :shuffletron
        :putil
        :playpen)
  (:export #:viz))

(in-package :shuffletron-viz)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Not sure if this eval-when makes sense. Need this available to
  ;; substitute into typespecs via #. (I'll know when I rebuild..)
  (defconstant +cb-length+ (* 4 4096))
  (defconstant +fft-size+ 4096)
  (defconstant +sample-rate+ 44100.0d0))

;;; Modular arithmetic on circular-buffer indices.

(deftype circular-index ()
  `(integer 0 #.(1- +cb-length+)))

(declaim (inline circular+))

(defun circular+ (x y)
  (declare (type circular-index x y))
  (logand (1- +cb-length+) (+ x y)))

(defvar *viz-window* nil)

(defclass viz-wakeup (window-event) ())

;;; Separate this state from the application object, to lessen the
;;; odds that we burn off the Earth's atmosphere if disable-plugin
;;; fires at the wrong instant. Wouldn't want that.
(defclass viz-state ()
  ((circular-buffer
    :reader circular-buffer
    :initform (make-array +cb-length+
                          :element-type 'mixalot:stereo-sample
                          :adjustable nil
                          :fill-pointer nil))

   (high-water-mark :initform 0 :accessor high-water-mark)
   (current-time :initform 0 :accessor current-time)

   (fft-input
    :accessor fft-input
    :initform (make-array +fft-size+
                          :element-type 'napa-fft:real-sample
                          :adjustable nil
                          :fill-pointer nil))
   (spectrum
    :accessor spectrum
    :initform (make-array +fft-size+
                          :element-type 'napa-fft:complex-sample
                          :adjustable nil
                          :fill-pointer nil))
   (displayed-spectrum
    :accessor displayed-spectrum
    :initform (make-array +fft-size+
                          :element-type 'napa-fft:complex-sample
                          :adjustable nil
                          :fill-pointer nil))

   (displayed-spectrums
    :accessor displayed-spectrums
    :initform (make-array (list 4 +fft-size+)
                          :adjustable nil
                          :fill-pointer nil))

   (texture :accessor texture :initform nil)

   (s-texture :accessor s-texture :initform nil)

   (dirty :accessor dirty :initform t)
   (shutdown-flag :initform nil :accessor shutdown-flag)))

(defclass viz ()
  ((viz-state :reader viz-state :initform (make-instance 'viz-state))))

(defmethod extending-note-audio-written ((plugin viz) buffer offset size)
  (declare (optimize (speed 2) (safety 2))
           (type mixalot:sample-vector buffer))
  ;; Partial(?) guard against race during initialization.
  (cond
    ((not (slot-boundp plugin 'viz-state))
     (format t "~&Ooh, initialization race condition detected.~%"))
    (t
     ;; Ignore the case of the circular buffer being full, since it
     ;; won't hurt anyone if the visualizer output is "wrong".
     (with-slots (circular-buffer high-water-mark current-time) (viz-state plugin)
       (loop with mask = (1- +cb-length+)
             with cbuffer = (the mixalot:sample-vector circular-buffer)
             for in-dex from 0 below size
             for out-dex = (the circular-index high-water-mark) then (circular+ out-dex 1)
             do
             (setf (aref cbuffer out-dex) (aref buffer in-dex))
             finally
             (setf high-water-mark (logand mask (+ high-water-mark size)))
             (incf current-time size)
             (when *viz-window*
               (send *viz-window* (make-instance 'viz-wakeup) :fail 'identity)))))))

(defvar *vs*)

(declaim (inline a-weight))

(defun a-weight (freq)
  (declare (optimize (speed 2))
           (type double-float freq))
  ;; 7.39705*10**9*f**4/ ( (f+129.4)**2 * (f+676.7) * (f+4636) * (f+76655)**2)
  (/ (* 7.39705d9 (* freq freq freq freq))
     (* (expt (+ freq 129.4) 2)
        (+ freq 676.7)
        (+ freq 4636)
        (expt (+ freq 76655) 2))))

;; Don't need this: FFTs don't overlap, so they're always in phase with each other.
#+NIL
(defun absolutize-phases (vs)
  (with-slots (spectrum current-time vs) vs
    (loop for bin from 0 below +fft-size+
          as phase-corrector = ;;(cis (- (* bin 2 pi ... )))
          do (setf (aref spectrum bin)
                   (* phase-corrector (aref spectrum bin))))))

(defun compute-spectrum (&optional (vs *vs*))
  (declare (optimize (speed 2)))
  (let ((fft-input (the napa-fft:real-sample-array (fft-input vs)))
        (spectrum  (the napa-fft:complex-sample-array (spectrum vs)))
        (audio     (the mixalot:sample-vector (circular-buffer vs))))

    (multiple-value-bind (scale scale-mode)
        #+NIL (values 32768.0d0 nil)
        (values 32.0d0 :inv)

      ;; Fill FFT input buffer.
      (loop for idx-in = (the circular-index
                           (circular+ (high-water-mark vs)
                                      (- +cb-length+ +fft-size+)))
            then (circular+ idx-in 1)
            for idx-out from 0 below +fft-size+
            do (setf (aref fft-input idx-out)
                     (float (/ (mixalot:stereo->mono (aref audio idx-in))
                               scale)
                            0.0d0)))
      ;; Do FFT.
      #+NIL
      (napa-fft:rfft fft-input
                     :dst spectrum
                     :size +fft-size+
                     :scale scale-mode)

      (napa-fft:windowed-rfft fft-input
                              (/ (length fft-input) 2)
                              +fft-size+
                              :dst spectrum
                              :scale scale-mode))

    ;;(absolutize-phases vs)

    ;; Apply fucked up A-weighting curve. I'm 99% certain the curve up
    ;; there is wrong, but I use it anyway becauss it attenuates the
    ;; low frequency clusterfuck.
    (loop for n from 0 below +fft-size+
          as freq = (* n (/ +sample-rate+ +fft-size+))
          do (setf (aref spectrum n)
                   (* (aref spectrum n) 2.0d0
                      ;; Kludge it. Eyeballed to balance lower frequencies better.
                      (+ (a-weight freq) 0.05))))

    ;; A miracle occurs.
    (values)))

#+NIL
(defun draw-spectrum (vs)
  (let ((spectrum  (the napa-fft:complex-sample-array (spectrum vs)))
        (dspectrum (displayed-spectrum vs)))
    (gl:color 1.0 1.0 1.0)
    (gl:begin :line-strip)
    (loop with width = (/ +fft-size+ 2)
          for i from 0 below width
          for x = *x0* then (1+ x)
          ;; Linear
          ;;as y = (+ -0.9d0 (* 0.1d0 (abs (aref dspectrum i))))
          ;; Logarithmic
          as y = (- (height *window*)
                    20
                    (* 0.333 (height *window*) (log (+ 1.0 (abs (aref spectrum i))))))

          do
          (setf (aref dspectrum i)
                (expt-approach (aref dspectrum i)
                               y
                               :rate 1d-7
                               :threshold 0.001))
          (gl:vertex x (realpart (aref dspectrum i)) 0.0d0))
    (gl:end)
    (gl:check-error)))

;;; This version draws the ghost trails with different elasticities..
(defun draw-spectrum (vs)
  (let ((spectrum  (the napa-fft:complex-sample-array (spectrum vs)))
        (displayed (displayed-spectrums vs)))
    (loop for j below 4
          for rate in '(1d-9 1d-6 1d-3 1d-2)
          for alpha in '(1.0 0.6 0.3 0.2)
          do
          (gl:color 1.0 1.0 1.0 alpha)
          (gl:begin :line-strip)
          (loop with width = (/ +fft-size+ 2)
                for i from 0 below width
                ;;for x = -0.98d0 then (+ x (/ 1.96d0 width))
                for x = *x0* then (1+ x)
                ;; Linear
                ;;as y = (+ -0.9d0 (* 0.1d0 (abs (aref dspectrum i))))
                ;; Logarithmic
                ;;as y = (+ -0.9d0 (* 0.7 (log (+ 1.0 (abs (aref spectrum i))))))
                as y = (- (height *window*)
                          20
                          (* 0.333 (height *window*) (log (+ 1.0 (abs (aref spectrum i))))))
                do
                (setf (aref displayed j i)
                      (expt-approach (aref displayed j i)
                                     y
                                     :rate rate
                                     :threshold 0.001))
                (gl:vertex x (realpart (aref displayed j i)) 0.0d0))
          (gl:end))
    (gl:check-error)))

(defclass viz-window (window time-consumer)
  ((vs :initarg :vs))
  (:default-initargs
   :application-name "Shuffletron"
   :title "Visualizer"))

(defparameter *x0* 0)

(defvar *y* 0)

(defun fill-spectrum-texture (spectrum texture)
  (check-type spectrum napa-fft:complex-sample-array)
  (let ((matrix (data-array texture)))
    (setf *y* (mod (1+ *y*) (height texture)))
    (loop with y = *y*
          with scale = 620.0d0
          for x from 0 below 2048
          for i upfrom 0
          as tmp = (* scale (log (1+ (abs (aref spectrum i)))))
          as c1 = (clamp 0 (round tmp) 255)
          as c2 = (clamp 0 (round (- tmp 256)) 255)
          as c3 = (clamp 0 (round (- tmp 512)) 255)
          do (setf (aref matrix y x)
                   (logior #xFF000000
                           c1
                           (ash c2 8)
                           (ash c3 16))))))
#+NIL
(defun fill-spectrum-texture (spectrum texture)
  (check-type spectrum napa-fft:complex-sample-array)
  (let ((matrix (data-array texture)))
    (setf *y* (mod (1+ *y*) (height texture)))
    (loop with y = *y*
          with scale = 160.0
          with b0 = (cis 0.0)
          with b1 = (cis (* 2 pi -1/3))
          with b2 = (cis (* 2 pi -2/3))
          for x from 0 below (width texture)
          for i from 0 below +fft-size+
          as c0 = (clamp 0 (round (* scale (max 0 (realpart (* b0 (aref spectrum i)))))) 255)
          as c1 = (clamp 0 (round (* scale (max 0 (realpart (* b1 (aref spectrum i)))))) 255)
          as c2 = (clamp 0 (round (* scale (max 0 (realpart (* b2 (aref spectrum i)))))) 255)

          do (setf (aref matrix y x)
                   (logior #xFF000000
                           c0
                           (ash c1 8)
                           (ash c2 16))))))

#+NIL
(defun fill-spectrum-texture (spectrum texture)
  (check-type spectrum napa-fft:complex-sample-array)
  (let ((matrix (data-array texture)))
    (setf *y* (mod (1+ *y*) (height texture)))
    (loop with y = *y*
          with scale = 460.0
          for x from 0 below (width texture)
          for i from 0 below +fft-size+
          as c0 = (clamp 0 (round (* scale (abs (realpart (aref spectrum i))))) 255)
          as c1 = (clamp 0 (round (* scale (abs (imagpart (aref spectrum i))))) 255)

          do (setf (aref matrix y x)
                   (logior #xFF000000
                           c0
                           (ash c1 8))))))

(declaim (inline ivref cvref lerp))

(defun lerp (x y alpha)
  (+ (* x (- 1.0f0 alpha))
     (* y alpha)))

(defun cvref (vector index default)
  (if (and (>= index 0)
           (< index (length vector)))
      (aref vector index)
      default))

(defun ivref (vector index)
  (let* ((i (truncate index))
         (a (mod index 1.0f0)))
    (lerp (cvref vector i 0.0f0)
          (cvref vector (1+ i) 0.0f0)
          a)))

(defun fill-2d-texture (spectrum texture)
  (declare (optimize (speed 2) (safety 2))
           (type napa-fft:complex-sample-array spectrum)
           (type image-matrix texture))
  (let ((width  (the (unsigned-byte 16) (width texture)))
        (height (the (unsigned-byte 16) (height texture)))
        (data (the (simple-array (unsigned-byte 32) (* *))
                (image-matrix-data texture)))
        (nbins (truncate +fft-size+ 2)))
    (fill (sb-kernel:%array-data-vector data) 0)
    (loop for y below height
          for j = 1.0f0 then (* j (expt 6/5 1))
          for row-cidx from 0 by 3
          while (< j nbins)
          do
     (loop for x below 64 ;width
           with step = (* j (expt 5/4 1))
           for cidx from row-cidx by 4
           as i = j then (+ i step) #+NIL (* i (expt 4/3 1/16))
           while (< i nbins)
           as color = (aref #(#xffffff
                              #x00ffff
                              #xff00ff
                              #x0000ff

                              #x00ffff
                              #x00ff00
                              #xff0000
                              #x000000

                              #xffff00
                              #x00ff00
                              #xff00ff
                              #x0000ff)
                            (mod cidx 12))
           as s = (round
                   (* 70.0 (abs (aref spectrum (round i)))
                      ;; Don't need the interpolation, already aliasing.
                      #+NIL (ivref spectrum i)))
           as p = (abs (clamp 0 s 255))
           do
           (setf (aref data y x)
                 (logior (logand (logior (ash p 0)
                                         (ash p 8)
                                         (ash p 16))
                                 #+NIL color
                                 #x00FF00)
                         #+NIL #xFF000000
                         (ash p 24)
                         ))))))

(defun display-mountaingram ()
  ;;(reset-transforms)
  (use-pixel-projection)
  (gl:disable :texture-2d)
  (gl:enable :line-smooth)              ; Enable antialiased lines.
  (gl:line-width 1.0)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :blend)
  (gl:hint :line-smooth-hint :nicest)
  (gl:check-error)
  (draw-spectrum *vs*))

(defun display-waterfall ()
  (gl:color 1 1 1 1)
  (use-pixel-projection)
  (with-slots (texture) *vs*
    (when texture
      (use-texture texture)
      (texture-update-rectangle texture 0 *y* (width texture) (1+ *y*))
      (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
      (gl:begin :quads)
      (playpen::%draw-rect+ (complex *x0* 0) (scalec #c(1 1) (- (dimensions texture) #c(0 2))) 0
                           (complex 0.0 (/ *y* 256.0))
                           (complex 1.0 (+ -1.0 (/ (+ 2 *y*) 256.0))))
      (gl:end)))

  (gl:check-error))

(defun display-2d-thing ()
  (gl:color 1 1 1 1)
  (use-pixel-projection)
  (with-slots (s-texture) *vs*
    (when s-texture
      (use-texture s-texture)
      (texture-update-rectangle s-texture 0 0 (width s-texture) (height s-texture))

      (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
      (gl:begin :quads)
      (playpen::%draw-rect+ (complex 0 260) (* 20 (dimensions s-texture)))
      (gl:end))))

(defmethod handle-event ((window viz-window) (event expose))
  (setf *viz-window* window)
  (let ((*vs* (slot-value window 'vs)))
    (with-graphics-context (window)
      ;(clear-screen #(0.4 0.17 0.17 0.9))
      ;(clear-screen #(0.3 0.3 0.3 1.0))
      (clear-screen #(0 0 0 1))
      (display-waterfall)
      (display-2d-thing)
      (display-mountaingram))))

(defmethod handle-event ((window viz-window) (event viz-wakeup))
  (with-slots (vs) window
    (compute-spectrum vs)
    (orf (texture vs)
         (image-matrix +rgba+ :width 2048 :height 256))
    (fill-spectrum-texture (spectrum vs) (texture vs))
    (orf (s-texture vs)
         (image-matrix +rgba+ :width 512 :height 512))
    (fill-2d-texture (spectrum vs) (s-texture vs)))
  (animate))

(defun launch-viz (vs)
  (run-app 'viz-window :vs vs))

(defmethod plugin-enabled
    (shuffletron (plugin (eql 'viz)) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (print "viz enabled")
  (let ((vs (viz-state shuffletron)))
    (bordeaux-threads:make-thread
     (lambda ()
       (launch-viz vs)))))

(defmethod plugin-disabled (shuffletron (plugin (eql 'viz)))
  (print "viz disabled")
  (setf (shutdown-flag (viz-state shuffletron)) t))

