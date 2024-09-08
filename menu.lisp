(in-package #:stumpwm-files)

(defparameter *selection-marker* #\s)

(defvar *scopes* nil)
(defvar *positions* nil)
(defvar *continue* nil)

(defparameter *debug-value* nil)

(defgeneric navigate (entry))
(defgeneric enabled-p (action))
(defgeneric display-continue (action))
(defgeneric perform-continue (action &key &allow-other-keys))

(defmethod navigate ((object null)))
(defmethod enabled-p (action) t)

(defun input-line (prompt-fmt-string prompt-fmt-args)
  (stumpwm:read-one-line
   (stumpwm:current-screen)
   (apply #'format nil prompt-fmt-string prompt-fmt-args)))

(defun yes-p (fmt-string &rest fmt-args)
  (stumpwm::yes-or-no-p (apply #'format nil fmt-string fmt-args)))

(defun select-key (&rest keyvals)
  (cadr (stumpwm:select-from-menu
	 (stumpwm:current-screen)
	 (loop while keyvals
	       for key = (pop keyvals)
	       for label = (pop keyvals)
	       collect (list label key)))))

(defun select-entry (entries prompt &optional (selected 0))
  (cadr (stumpwm:select-from-menu
	 (stumpwm:current-screen)
	 (loop for e in entries when e collect (list (display e) e))
	 prompt
	 selected)))

(defun select-entries (entries)
  (let ((input (stumpwm:select-from-batch-menu
		(stumpwm:current-screen)
		(loop for entry in entries
		      collect (make-instance 'stumpwm:menu-entry
					     :label (display entry)
					     :data entry))
		:prompt (format nil "Select[~c] [Enter/Esc]" *selection-marker*)
		:allowed-markers (list *selection-marker*))))
    (when-let (selected (cdr (assoc *selection-marker* input)))
      (mapcar #'stumpwm::menu-entry-data selected))))

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

(defun set-continue (action &optional (parent-p t))
  (setf *continue* action)
  (navigate (if parent-p (parent (context action)) (context action))))

(defun del-continue ()
  (setf *continue* nil))

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
  (setf *continue* nil)
  (setf *debug-value* nil))

(defun navigate-action (class pathname)
  (navigate (make-action class (make-node pathname))))

(defaction show-hidden "Show hidden files" () (action)
  (setf *show-hidden-p* t)
  (navigate (context action)))

(defaction hide-hidden "Hide hidden files" () (action)
  (setf *show-hidden-p* nil)
  (navigate (context action)))

(defaction settings "SETTINGS" () (action)
  (let ((dir (context action)))
    (navigate
     (select-entry
      (list (make-node (get-pathname dir) "<<<")
	    (make-action (if *show-hidden-p* 'hide-hidden 'show-hidden) dir))
      "SETTINGS"))))

(defun forbidden-pathname-p (pathname)
  (or (eq pathname (user-homedir-pathname))
      (>= 3 (length (pathname-directory pathname)))))

(defun enabled-action-p (action)
  (not (forbidden-pathname-p (get-pathname (context action)))))

(defaction xdg-open-file-action "Open" (file 10) (action)
  (uiop:launch-program
   (list "xdg-open" (namestring (get-pathname (context action))))))

(defaction copy-file-action "Copy" (file 20) (action)
  (set-continue action))

(defmethod display-continue ((action copy-file-action))
  (format nil "PASTE: ~a" (display (context action))))

(defmethod perform-continue ((action copy-file-action) &key dir)
  (copy-node (context action) dir)
  (del-continue))

(defaction move-file-action "Move" (file 30) (action)
  (set-continue action))

(defmethod display-continue ((action move-file-action))
  (format nil "PASTE: ~a" (display (context action))))

(defmethod perform-continue ((action move-file-action) &key dir)
  (move-node (context action) dir)
  (del-continue))

(defaction rename-file-action "Rename" (file 40) (action)
  (let* ((file (context action))
	 (old-pathname (get-pathname file))
	 (dir-pathname (uiop:pathname-directory-pathname old-pathname)))
    (when-let (new-name (input-line "Rename ~a to: " (display file)))
      (let ((new-pathname (merge-pathnames new-name dir-pathname)))
	(if (forbidden-pathname-p new-pathname)
	    (stumpwm::message-no-timeout "Forbidden pathname:~%~a" new-pathname)
	    (progn
	      (uiop:run-program (list "mv"
				      (namestring old-pathname)
				      (namestring new-pathname)))
	      (navigate (parent file))))))))

(defaction delete-file-action "Delete" (file 50) (action)
  (let* ((file (context action))
	 (pathname (get-pathname file)))
    (when (yes-p "Delete file '~a'? " pathname)
      (uiop:run-program (list "rm" (namestring pathname))))
    (navigate (parent file))))

(defaction xdg-open-dir-action "Open" (dir 10) (action)
  (uiop:launch-program
   (list "xdg-open" (namestring (get-pathname (context action))))))

(defaction copy-dir-action "Copy" (dir 20) (action)
  (set-continue action))

(defmethod display-continue ((action copy-dir-action))
  (format nil "PASTE: ~a" (display (context action))))

(defmethod perform-continue ((action copy-dir-action) &key dir)
  (copy-node (context action) dir)
  (del-continue))

(defaction move-dir-action "Move" (dir 30) (action)
  (set-continue action))

(defmethod display-continue ((action move-dir-action))
  (format nil "PASTE: ~a" (display (context action))))

(defmethod perform-continue ((action move-dir-action) &key dir)
  (move-node (context action) dir)
  (del-continue))

(defmethod enabled-p ((action move-dir-action))
  (enabled-action-p action))

(defaction rename-dir-action "Rename" (dir 40) (action)
  (let* ((dir (context action))
	 (old-pathname (get-pathname dir))
	 (old-name (lastcar (pathname-directory old-pathname))))
    (when-let (new-name (input-line "Rename directory '~a' to: " old-name))
      (let ((new-pathname (uiop:ensure-directory-pathname
			   (merge-pathnames new-name
					    (get-pathname (parent dir))))))
	(if (forbidden-pathname-p new-pathname)
	    (stumpwm::message-no-timeout "Forbidden pathname:~%~a" new-pathname)
            (progn
	      (uiop:run-program (list "mv"
				      (namestring old-pathname)
				      (namestring new-pathname)))
	      (navigate (parent dir))))))))

(defmethod enabled-p ((action rename-dir-action))
  (enabled-action-p action))

(defclass nodes-selection (dir)
  ((selected :initarg :selected :reader selected)))

(defaction select-nodes "Select files" (dir 50) (action)
  (let ((dir (context action)))
    (navigate
     (if-let (nodes (select-entries (sorted-nodes (get-pathname dir))))
       (select-entry (make-actions (make-instance 'nodes-selection
                                                  :pathname (get-pathname dir)
						  :selected nodes)
				   :nodes-selection)
		     (format nil "~d selected" (length nodes)))
       dir))))

(defaction copy-selected-nodes "Copy" (:nodes-selection 10) (action)
  (set-continue action nil))

(defmethod display-continue ((action copy-selected-nodes))
  (format nil "PASTE (~d selected)" (length (selected (context action)))))

(defmethod perform-continue ((action copy-selected-nodes) &key dir)
  (dolist (node (selected (context action)))
    (copy-node node dir))
  (del-continue))

(defaction move-selected-nodes "Move" (:nodes-selection 20) (action)
  (set-continue action nil))

(defmethod display-continue ((action move-selected-nodes))
  (format nil "PASTE (~d selected)" (length (selected (context action)))))

(defmethod perform-continue ((action move-selected-nodes) &key dir)
  (setf *debug-value* (list action dir))
  (del-continue))

(defaction delete-selected-nodes "Delete" (:nodes-selection 30) (action)
  (setf *debug-value* action))

(defaction delete-dir-action "Delete" (dir 60) (action)
  (let* ((dir (context action))
	 (pathname (get-pathname dir)))
    (when (yes-p "Delete directory '~a'?~%" pathname)
      (if (emptyp (list-directory-pathnames pathname))
	  (uiop:delete-empty-directory pathname)
          (when (yes-p "Directory '~a' is not empty.~%DELETE RECURSIVELY?~%" pathname)
	    (uiop:delete-directory-tree
	     pathname
	     :validate #'(lambda (%)
			   (or (forbidden-pathname-p %)
			       (yes-p "~a~%CONFIRM RECURSIVE DELETION.~%" %)))))))
    (navigate (parent dir))))

(defmethod enabled-p ((action delete-dir-action))
  (enabled-action-p action))

(defaction create-dir-action "Create directory" (dir 70) (action)
  (let* ((dir (context action))
	 (pathname (get-pathname dir)))
    (when-let (name (input-line "~a~%Create directory: " pathname))
      (uiop:run-program (list "mkdir" (namestring (merge-pathnames name pathname))))
      (navigate dir))))

(defaction dir-actions "ACTIONS" () (action)
  (let* ((dir (context action))
	 (pathname (get-pathname dir)))
    (navigate
     (select-entry (cons (make-node pathname "<<<")
			 (make-actions dir))
		   (namestring pathname)))))

(defaction cancel-continue "CANCEL" () (action)
  (del-continue)
  (navigate (context action)))

(defaction finish-continue nil () (action)
  (let ((dir (context action)))
    (perform-continue *continue* :dir dir)
    (navigate dir)))

(defmethod display ((action finish-continue))
  (display-continue *continue*))

(defun dir-menu (dir)
  (if *continue*
      (list (make-action 'cancel-continue dir)
	    (make-action 'finish-continue dir)
            (parent dir "../"))
      (list (make-action 'settings dir)
	    (make-action 'dir-actions dir)
            (parent dir "../"))))

(defmethod navigate ((dir dir))
  (let* ((menu (remove nil (dir-menu dir)))
	 (pathname (get-pathname dir))
         (nodes (sorted-nodes pathname)))
    (navigate
     (select-entry (append menu nodes)
		   (namestring pathname)
		   (+ (or (initial-selection-position dir nodes) -1)
		      (length menu))))))

(defmethod navigate ((file file))
  (navigate
   (select-entry (cons (parent file "<<<")
		       (make-actions file))
		 (namestring (get-pathname file))
		 1)))

(defun menu ()
  (let ((stumpwm::*menu-maximum-height* 30))
    (navigate (make-node (user-homedir-pathname)))))

(stumpwm:defcommand files () () (menu))

;; (stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "j") "files")
