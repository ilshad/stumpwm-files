(in-package #:stumpwm-files)

(defvar *scopes* nil)
(defvar *positions* nil)

(defgeneric navigate (object))

(defmethod navigate ((object null)))

(defun nav (entries prompt &optional (selected 0))
  (navigate
   (cadr (stumpwm:select-from-menu
	  (stumpwm:current-screen)
	  (loop for e in entries when e collect (list (display e) e))
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

(defun actions (context &optional scope)
  (let ((classes (loop for (key value) on *scopes* by #'cddr
		       when (if scope
				(if (keywordp scope)
				    (eq scope value)
				    (subtypep scope value))
				(subtypep (type-of context) value))
                         collect key)))
    (flet ((sort-key (class) (or (getf *positions* class) 99)))
      (loop for class in (sort classes #'< :key #'sort-key)
	    collect (make-instance class :context context)))))

(defaction show-hidden "Show hidden files" () (action)
  (setf *show-hidden-p* t)
  (navigate (context action)))

(defaction hide-hidden "Hide hidden files" () (action)
  (setf *show-hidden-p* nil)
  (navigate (context action)))

(defaction settings "SETTINGS" () (action)
  (nav (list (make-node (get-pathname (context action)) "<<<")
	     (if *show-hidden-p*
		 (action hide-hidden (context action))
		 (action show-hidden (context action))))
       "SETTINGS"))

(defaction copy-action "Copy" (node 10) (action))
(defaction move-action "Move" (node 20) (action))
(defaction rename-action "Rename" (node 30) (action))
(defaction delete-action "Delete" (node 40) (action))

(defaction dir-actions "ACTIONS" () (action)
  (let* ((dir (context action))
	 (pathname (get-pathname dir)))
    (nav (cons (make-node pathname "<<<")
	       (actions dir))
	 (princ-to-string pathname))))

(defmethod navigate ((dir dir))
  (let* ((pathname (get-pathname dir))
         (nodes (sorted-nodes pathname))
	 (menu (remove nil (list (action settings dir)
				 (action dir-actions dir)
				 (parent dir "../")))))
    (nav (append menu nodes)
	 (princ-to-string pathname)
	 (+ (or (initial-selection-position dir nodes) -1)
	    (length menu)))))

(defun menu ()
  (let ((stumpwm::*menu-maximum-height* 30))
    (navigate (make-node (user-homedir-pathname)))))

(stumpwm:defcommand files () () (menu))

;; (stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "j") "files")
