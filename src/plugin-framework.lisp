(in-package :shuffletron)

;;;; ----------------------------------------
;;;; Dynamic class machinery
;;;; ----------------------------------------

(defun derived-name (base mixins)
  (if mixins
      (let* ((*package* (symbol-package base))
             (name (intern (format nil "~W/~{~W~^/~}" base mixins) (symbol-package base))))
        (setf (get name 'application-class-name) base
              (get name 'plugin-mixins) mixins)
        name)
      base))

(defun ensure-configuration-class (base mixins)
  (or (find-class (derived-name base mixins) nil)
      (eval `(defclass ,(derived-name base mixins) (,base ,@mixins) ()))))

(defun conf-base (derived-name) (or (get derived-name 'application-class-name) derived-name))
(defun conf-plugins (derived-name) (get derived-name 'plugin-mixins))
(defun conf-add-plugin (derived-name plugin-name) (remove-duplicates (append (conf-plugins derived-name) (list plugin-name))))
(defun conf-remove-plugin (derived-name plugin-name) (remove plugin-name (conf-plugins derived-name)))


(defgeneric plugin-enabled (application plugin-name &rest initargs &key &allow-other-keys)
  (:documentation "Notification that a plugin has just been enabled. Plugin initialization methods must EQL-specialize on the PLUGIN-NAME argument. ")
  (:method-combination progn)
  (:method progn (application plugin-name &rest initargs)
    (declare (ignore plugin-name initargs))))

(defgeneric plugin-disabled (application plugin-name)
  (:documentation "Notification that a plugin is about to be disabled. Plugin initialization methods must EQL-specialize on the PLUGIN-NAME argument. ")
  (:method-combination progn)
  (:method progn (application plugin-name)
    (declare (ignore application plugin-name))))

(defun enabled-plugins (application)
  "List of plugins currently enabled by an application."
  (conf-plugins (type-of application)))

(defgeneric apply-configuration (application plugins &rest initargs)
  (:documentation "Reconfigure an application to enable the specified
  plugins. Currently enabled plugins outside this set are disabled.")
  (:method (application plugins &rest initargs)
    (let* ((name (type-of application))
           (current-plugins (conf-plugins name))
           (enabled-plugins (set-difference plugins current-plugins))
           (disabled-plugins (set-difference current-plugins plugins)))
      (dolist (plugin disabled-plugins) (plugin-disabled application plugin))
      (apply #'change-class
             application
             (ensure-configuration-class (conf-base name) plugins)
             initargs)
      (dolist (plugin enabled-plugins) (apply #'plugin-enabled application plugin initargs))
      application)))

(defun application-enable-plugin (application plugin-name &rest initargs)
  "Enable a plugin within an application."
  (apply #'apply-configuration application (conf-add-plugin (type-of application) plugin-name) initargs))

(defun application-disable-plugin (application plugin-name)
  "Disable a plugin within an application."
  (apply-configuration application (conf-remove-plugin (type-of application) plugin-name)))

;;;; ----------------------------------------
;;;; Sugar for extensible functions:
;;;; ----------------------------------------

;;; Extensible functions wrap a defun-style definition in a generic
;;; function (prefixed with EXTENDING-) with an additional first
;;; argument, binding the application instance to *application*.
;;; Extensions use this argument to specialize on their mixin. The
;;; body is defined in an unspecialized primary method. A wrapper is
;;; defined using DEFUN which calls the generic function with the
;;; value of *application* as the first argument. Therefore, you must
;;; bind *application* to the application object before calling any
;;; such functions.

;;; (This macro is exactly the sort of hare-brained misdirection of
;;; effort that will never repay the time diverted to it, and
;;; threatens to turn one off CL completely. I just couldn't resist
;;; indulging the foolish urge to automate an already trivial idiom by
;;; inflicting more half-arsed magic macrology on the world.  Why,
;;; imagine the hundreds of characters this could spare me over the
;;; remainder of my programming lifetime! I like it anyway.)

(defvar *application*)

;; Massage arglist so we can put it in a method, wrapped by a regular
;; function, without completely obscuring the arglist. At least the
;; required args will still be visible in SLIME..

(defun wrapper-arguments (arglist)
  (multiple-value-bind (required optional rest keys aok aux)
      (alexandria:parse-ordinary-lambda-list arglist)
    (when aux (error "You can't use &aux in an extensible function."))
    (let ((rest-arg (and (or optional rest keys aok) (gensym))))
      (values
       (if rest-arg
           `(,@required &rest ,rest-arg)
           required)
       required
       rest-arg))))

(defmacro defun-extensible (name arglist &body body)
  (let ((gf-name (intern (format nil "EXTENDING-~A" (symbol-name name)) (symbol-package name))))
    (multiple-value-bind (wrapper-args required-args rest-arg) (wrapper-arguments arglist)
      `(progn
         ;; An exercise for the interested reader: Produce a
         ;; DEFGENERIC here, mangling the arglist such that
         ;; optional/keyword args won't break it, to avoid SBCL
         ;; bitching as it implicitly creates the function.
         (defmethod ,gf-name (*application* ,@arglist) ,@body)
         (defun ,name ,wrapper-args (apply #',gf-name *application* ,@required-args ,rest-arg))))))

;;; Simple interface to use within the application.

(defun enable-plugin (plugin-name &rest initargs)
  (apply 'application-enable-plugin *application* plugin-name initargs))

(defun disable-plugin (plugin-name)
  (application-disable-plugin *application* plugin-name))
