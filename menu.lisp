(in-package #:stumpwm-files)

(defmethod navigate ((object null)))

(defun nav (entries prompt &optional (selected 0))
  (navigate
   (cadr (stumpwm:select-from-menu
	  (stumpwm:current-screen)
	  (loop for e in entries collect (list (display e) e))
	  prompt
	  selected))))

(defclass action-entry (entry)
  ((context :initarg :context :reader context)))

(defmacro action (class &optional context)
  `(make-instance ',class :context ,context))

(defmacro defaction (class display (entry-var-name) &body body)
  `(progn (defclass ,class (action-entry) ((display :initform ,display)))
	  (defmethod navigate ((,entry-var-name ,class)) ,@body)))

(defaction show-hidden "Show hidden files" (entry)
  (setf *show-hidden-p* t)
  (navigate (context entry)))

(defaction hide-hidden "Hide hidden files" (entry)
  (setf *show-hidden-p* nil)
  (navigate (context entry)))

(defaction listing-settings "SETTINGS" (entry)
  (nav (list (make-file-entry (get-pathname (context entry)) "<<<")
	     (if *show-hidden-p*
		 (action hide-hidden (context entry))
		 (action show-hidden (context entry))))
       "SETTINGS"))

(defaction copy-file-action "Copy" (entry))
(defaction move-file-action "Move" (entry))
(defaction rename-file-action "Rename" (entry))
(defaction delete-file-action "Delete" (entry))

(defaction dir-actions "ACTIONS" (entry)
  (let ((pathname (get-pathname (context entry))))
    (nav (list (make-file-entry pathname "<<<")
	       (action copy-file-action (context entry))
	       (action move-file-action (context entry))
	       (action rename-file-action (context entry))
	       (action delete-file-action (context entry)))
	 (format nil "~a" pathname))))

(defmethod navigate ((entry dir))
  (let* ((pathname (get-pathname entry))
         (files (sorted-file-entries pathname))
         (parent (parent entry "../")))
    (nav (cons (action listing-settings entry)
	       (cons (action dir-actions entry)
		     (if parent (cons parent files) files)))
         (format nil "NAVIGATION ~a" pathname)
	 (+ (initial-selection-position entry files) (if parent 3 2)))))

(defun menu ()
  (let ((stumpwm::*menu-maximum-height* 30))
    (navigate (make-file-entry (user-homedir-pathname)))))

(stumpwm:defcommand files () () (menu))

;; (stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "j") "files")
