(in-package #:stumpwm-files)

(defvar *scopes* nil)
(defvar *positions* nil)
(defvar *operation* nil)

(defparameter *debug-value* nil)

(defgeneric navigate (entry))
(defgeneric operation (action &key &allow-other-keys))
(defgeneric enabled-p (action))

(defmethod navigate ((object null)))
(defmethod enabled-p (action) t)

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

(defun make-action (class context)
  (make-instance class :context context))

(defun make-actions (context &optional scope)
  (let ((classes (loop for (key value) on *scopes* by #'cddr
		       when (if scope
				(if (keywordp scope)
				    (eq scope value)
				    (subtypep scope value))
				(subtypep (type-of context) value))
                         collect key)))
    (flet ((sort-key (class) (or (getf *positions* class) 99)))
      (loop for class in (sort classes #'< :key #'sort-key)
	    for action = (make-action class context)
	    when (enabled-p action)
            collect action))))

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
  (let ((dir (context action)))
    (nav (list (make-node (get-pathname dir) "<<<")
	       (make-action (if *show-hidden-p* 'hide-hidden 'show-hidden) dir))
	 "SETTINGS")))

(defun forbidden-pathname-p (pathname)
  (or (eq pathname (user-homedir-pathname))
      (>= 3 (length (pathname-directory pathname)))))

(defun enabled-action-p (action)
  (not (forbidden-pathname-p (get-pathname (context action)))))

(defun cli-from-to (command from to)
  (uiop:run-program
   (append command
	   (list (namestring (get-pathname from))
		 (namestring (get-pathname to))))))

(defoperation copy-file-action "Copy" (file 10) (action &key dir)
  (cli-from-to '("cp") (context action) dir))

(defoperation move-file-action "Move" (file 20) (action &key dir)
  (cli-from-to '("mv") (context action) dir))

(defaction rename-file-action "Rename" (file 30) (action)
  (let* ((file (context action))
	 (old-pathname (get-pathname file))
	 (dir-pathname (uiop:pathname-directory-pathname old-pathname)))
    (when-let (new-name (stumpwm:read-one-line
			 (stumpwm:current-screen)
			 (format nil "Rename ~a to: " (display file))))
      (let ((new-pathname (merge-pathnames new-name dir-pathname)))
	(if (forbidden-pathname-p new-pathname)
	    (stumpwm::message-no-timeout
	     (format nil "Forbidden pathname:~%~a" new-pathname))
	    (progn
	      (uiop:run-program (list "mv"
				      (namestring old-pathname)
				      (namestring new-pathname)))
	      (navigate (parent file))))))))

(defaction delete-file-action "Delete" (file 40) (action)
  (let* ((file (context action))
	 (pathname (get-pathname file)))
    (when (stumpwm::yes-or-no-p (format nil "Delete file '~a'? " pathname))
      (uiop:run-program (list "rm" (namestring pathname))))
    (navigate (parent file))))

(defoperation copy-dir-action "Copy directory" (dir 10) (action &key dir)
  (cli-from-to '("cp" "-r") (context action) dir))

(defoperation move-dir-action "Move directory" (dir 20) (action &key dir)
  (cli-from-to '("mv") (context action) dir))

(defaction rename-dir-action "Rename directory" (dir 30) (action)
  (let* ((dir (context action))
	 (old-pathname (get-pathname dir))
	 (old-name (lastcar (pathname-directory old-pathname))))
    (when-let (new-name (stumpwm:read-one-line
			 (stumpwm:current-screen)
			 (format nil "Rename directory '~a' to: " old-name)))
      (let ((new-pathname (uiop:ensure-directory-pathname
			   (merge-pathnames new-name
					    (get-pathname (parent dir))))))
	(if (forbidden-pathname-p new-pathname)
	    (stumpwm::message-no-timeout
	     (format nil "Forbidden pathname:~%~a" new-pathname))
            (progn
	      (uiop:run-program (list "mv"
				      (namestring old-pathname)
				      (namestring new-pathname)))
	      (navigate (parent dir))))))))

(defun validate-delete-1 (pathname)
  (stumpwm::yes-or-no-p
   (format nil "Directory '~a' is not empty.~%DELETE RECURSIVELY?~%" pathname)))

(defun validate-delete-2 (pathname)
  (or (forbidden-pathname-p pathname)
      (stumpwm::yes-or-no-p
       (format nil "~a~%CONFIRM RECURSIVE DELETION AGAIN.~%" pathname))))

(defaction delete-dir-action "Delete directory" (dir 40) (action)
  (let* ((dir (context action))
	 (pathname (get-pathname dir)))
    (when (stumpwm::yes-or-no-p (format nil "Delete directory '~a'?~%" pathname))
      (if (emptyp (list-directory-pathnames pathname))
	  (uiop:delete-empty-directory pathname)
          (when (validate-delete-1 pathname)
	    (uiop:delete-directory-tree pathname :validate #'validate-delete-2))))
    (navigate (parent dir))))

(defaction create-dir-action "Create directory" (dir 50) (action)
  (let* ((dir (context action))
	 (pathname (get-pathname dir)))
    (when-let (name (stumpwm:read-one-line
		     (stumpwm:current-screen)
		     (format nil "~a~%Create directory: " pathname)))
      (uiop:run-program (list "mkdir" (namestring (merge-pathnames name pathname))))
      (navigate dir))))

(defaction dir-actions "ACTIONS" () (action)
  (let* ((dir (context action))
	 (pathname (get-pathname dir)))
    (nav (cons (make-node pathname "<<<")
	       (make-actions dir))
	 (namestring pathname))))

(defaction cancel "CANCEL" () (action)
  (setf *operation* nil)
  (navigate (context action)))

(defaction paste nil () (action)
  (operation *operation* :dir (context action))
  (navigate (context action)))

(defmethod enabled-p ((action rename-dir-action)) (enabled-action-p action))
(defmethod enabled-p ((action move-dir-action)) (enabled-action-p action))
(defmethod enabled-p ((action delete-dir-action)) (enabled-action-p action))

(defmethod display ((action paste))
  (format nil "PASTE THIS: ~a" (display (context *operation*))))

(defun dir-menu (dir)
  (if *operation*
      (list (make-action 'cancel dir)
	    (make-action 'paste dir)
            (parent dir "../"))
      (list (make-action 'settings dir)
	    (make-action 'dir-actions dir)
            (parent dir "../"))))

(defmethod navigate ((dir dir))
  (let* ((pathname (get-pathname dir))
         (nodes (sorted-nodes pathname))
	 (menu (remove nil (dir-menu dir))))
    (nav (append menu nodes)
	 (namestring pathname)
	 (+ (or (initial-selection-position dir nodes) -1)
	    (length menu)))))

(defmethod navigate ((file file))
  (nav (cons (parent file "<<<")
	     (make-actions file))
       (namestring (get-pathname file))))

(defun menu ()
  (let ((stumpwm::*menu-maximum-height* 30))
    (navigate (make-node (user-homedir-pathname)))))

(stumpwm:defcommand files () () (menu))

;; (stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "j") "files")
