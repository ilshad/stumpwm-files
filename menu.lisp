(in-package #:stumpwm-files)

(defvar *scopes* nil)
(defvar *positions* nil)

(defgeneric navigate (object))

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

(defmacro defaction (class display (&optional scope position) (arg) &body body)
  `(progn
     (defclass ,class (action-entry)
       ((display :initform ,display)))
     ,(when scope
	`(setf (getf *scopes* ',class) ',scope))
     ,(when position
	`(setf (getf *positions* ',class) ',position))
     (defmethod navigate ((,arg ,class))
       ,@body)))

(defmacro action (class &optional context)
  `(make-instance ',class :context ,context))

(defun %actions (scope context)
  (let ((classes (loop for (key value) on *scopes* by #'cddr
		       when (if (keywordp scope)
				(eq scope value)
				(subtypep scope value))
                       	 collect key)))
    (flet ((sort-key (class) (or (getf *positions* class) 99)))
      (loop for class in (sort classes #'< :key #'sort-key)
	    collect (make-instance class :context context)))))

(defmacro actions (scope &optional context)
  `(%actions ',scope ,context))

(defaction show-hidden "Show hidden files" () (entry)
  (setf *show-hidden-p* t)
  (navigate (context entry)))

(defaction hide-hidden "Hide hidden files" () (entry)
  (setf *show-hidden-p* nil)
  (navigate (context entry)))

(defaction listing-settings "SETTINGS" () (entry)
  (nav (list (make-file-entry (get-pathname (context entry)) "<<<")
	     (if *show-hidden-p*
		 (action hide-hidden (context entry))
		 (action show-hidden (context entry))))
       "SETTINGS"))

(defaction copy-file-action "Copy" (file 10) (entry))
(defaction move-file-action "Move" (file 20) (entry))
(defaction rename-file-action "Rename" (file 30) (entry))
(defaction delete-file-action "Delete" (file 40) (entry))

(defaction dir-actions "ACTIONS" () (entry)
  (let ((pathname (get-pathname (context entry))))
    (nav (cons (make-file-entry pathname "<<<")
	       (actions dir (context entry)))
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
