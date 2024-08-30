(in-package #:stumpwm-files)

(defvar *scopes* nil)
(defvar *positions* nil)
(defvar *operation* nil)

(defgeneric navigate (object))

(defmethod navigate ((object null)))

(defgeneric operation (action &key &allow-other-keys))

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

(defmacro defoperation (class display (scope position) (arg &rest args) &body body)
  (let ((op-args (cdr args)))
    `(progn
       (defaction ,class ,display (,scope ,position) (action)
	 (setf *operation* action)
	 (navigate (parent (context action))))
       (defmethod operation ((,arg ,class) &key ,@op-args)
	 ,@body
	 (setf *operation* nil)))))

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

(defun clean-actions ()
  (setf *scopes* nil)
  (setf *positions* nil)
  (setf *operation* nil))

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

(defoperation copy-file-action "Copy file" (file 10) (action &key dir)
  (setf *debug-value* (list :copy action dir)))

(defoperation move-file-action "Move file" (file 20) (action &key dir)
  (setf *debug-value* (list :copy action dir)))

(defaction copy-dir-action "Copy directory" (dir 10) (action))
(defaction move-dir-action "Move directory" (dir 20) (action))

(defaction rename-file-action "Rename file" (file 30) (action))
(defaction delete-file-action "Delete file" (file 40) (action))
(defaction rename-dir-action "Rename directory" (dir 30) (action))
(defaction delete-dir-action "Delete directory" (dir 40) (action))
(defaction create-dir-action "Create nested directory" (dir 50) (action))

(defaction dir-actions "ACTIONS" () (action)
  (let* ((dir (context action))
	 (pathname (get-pathname dir)))
    (nav (cons (make-node pathname "<<<")
	       (actions dir))
	 (princ-to-string pathname))))

(defparameter *debug-value* nil)

(defaction paste "PASTE" () (action)
  (operation *operation* :dir (context action))
  (navigate (context action)))

(defaction cancel "CANCEL" () (action)
  (setf *operation* nil)
  (navigate (context action)))

(defun dir-menu (dir)
  (if *operation*
      (list (action cancel dir)
	    (action paste dir)
	    (parent dir "../"))
      (list (action settings dir)
	    (action dir-actions dir)
	    (parent dir "../"))))

(defmethod navigate ((dir dir))
  (let* ((pathname (get-pathname dir))
         (nodes (sorted-nodes pathname))
	 (menu (remove nil (dir-menu dir))))
    (nav (append menu nodes)
	 (princ-to-string pathname)
	 (+ (or (initial-selection-position dir nodes) -1)
	    (length menu)))))

(defmethod navigate ((file file))
  (nav (cons (parent file "<<<")
	     (actions file))
       (princ-to-string (get-pathname file))))

(defun menu ()
  (let ((stumpwm::*menu-maximum-height* 30))
    (navigate (make-node (user-homedir-pathname)))))

(stumpwm:defcommand files () () (menu))

;; (stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "j") "files")
