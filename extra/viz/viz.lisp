(defpackage :shuffletron-viz
  (:use :common-lisp
        :shuffletron
        :playpen)
  (:export #:viz))

(in-package :shuffletron-viz)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Not sure if this eval-when makes sense. Need this available to
  ;; substitute into typespecs via #. (I'll know when I rebuild..)
  (defconstant +cb-length+ (* 4 4096))
  (defconstant +fft-size+ 1024)
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

   (texture :accessor texture :initform nil)
   (dirty :accessor dirty :initform t)
   (shutdown-flag :initform nil :accessor shutdown-flag)))

(defclass viz ()
  ((viz-state :reader viz-state :initform (make-instance 'viz-state))))

(defmethod extending-note-audio-written ((plugin viz) buffer offset size)
  (declare (optimize (speed 2) (safety 2))
           (type mixalot:sample-vector buffer))
  ;; Don't bother handling the full buffer case, since we're just a visualizer.
  (with-slots (circular-buffer high-water-mark dirty) (viz-state plugin)
    (loop with mask = (1- +cb-length+)
          with cbuffer = (the mixalot:sample-vector circular-buffer)
          for in-dex from 0 below size
          for out-dex = (the circular-index high-water-mark) then (circular+ out-dex 1)
          do
          (setf (aref cbuffer out-dex) (aref buffer in-dex)
                dirty t)
          finally
          (setf high-water-mark (logand mask (+ high-water-mark size)))
          (when *viz-window*
            (send *viz-window* (make-instance 'viz-wakeup) :fail 'identity)))))

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
      (napa-fft:rfft fft-input
                     :dst spectrum
                     :size +fft-size+
                     :scale scale-mode)

      (napa-fft:windowed-rfft fft-input
                              (/ (length fft-input) 2)
                              +fft-size+
                              :dst spectrum
                              :scale scale-mode))

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

(defun draw-spectrum (vs)
  (let ((spectrum  (the napa-fft:complex-sample-array (spectrum vs)))
        (dspectrum (displayed-spectrum vs)))
    (gl:color 1.0 1.0 1.0)
    (gl:begin :line-strip)
    (loop with width = (/ +fft-size+ 2)
          for i from 0 below width
          for x = -0.98d0 then (+ x (/ 1.96d0 width))
          ;; Linear
          ;;as y = (+ -0.9d0 (* 0.1d0 (abs (aref dspectrum i))))
          ;; Logarithmic
          as y = (+ -0.9d0 (* 0.7 (log (+ 1.0 (abs (aref spectrum i))))))
          do
          (setf (aref dspectrum i)
                (expt-approach (aref dspectrum i)
                               y
                               :rate 1d-7
                               :threshold 0.001))
          (gl:vertex x (realpart (aref dspectrum i)) 0.0d0))
    (gl:end)
    (gl:check-error)))

(defclass viz-window (window time-consumer)
  ((vs :initarg :vs))
  (:default-initargs
   :application-name "Shuffletron"
   :title "Visualizer"))

(defmethod handle-event ((window viz-window) (event expose))
  (setf *viz-window* window)
  (let ((*vs* (slot-value window 'vs)))
   (with-graphics-context (window)
     ;;(use-graphic-projection)
     (reset-transforms)
     (clear-screen #(0.4 0.17 0.17 0.8))

     (gl:enable :line-smooth)           ; Enable antialiased lines.
     (gl:line-width 1.0)
     (gl:blend-func :src-alpha :one-minus-src-alpha)
     (gl:enable :blend)
     (gl:hint :line-smooth-hint :nicest)
     (gl:check-error)

     (gl:color 1.0 1.0 0.5)
     ;;(pwin-tests::draw-circle 0 0 0.5)

     (draw-spectrum *vs*)

     (gl:check-error))))

(defmethod handle-event ((window viz-window) (event viz-wakeup))
  (with-slots (vs) window
    (compute-spectrum vs))
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

