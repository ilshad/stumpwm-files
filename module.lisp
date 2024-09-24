(in-package #:stumpwm-files)

(defparameter *show-hidden-p* nil)
(defparameter *hidden-types* '("fasl"))
(defparameter *selection-marker* #\s)
(defparameter *debug-value* nil)

(defvar *scopes* nil)
(defvar *positions* nil)
(defvar *continue* nil)

(defclass entry ()
  ((display :initarg :display :initform nil :reader display)))

(defclass node (entry)
  ((pathname :initarg :pathname :reader get-pathname)
   (modified :accessor modified)
   (hidden-p :initform nil)))

(defclass dir (node)
  ((initial-selection-node :initform nil :accessor initial-selection-node)))

(defclass file (node) ())

(defmethod initialize-instance :after ((node node) &key)
  (setf (modified node) (file-write-date (get-pathname node))))

(defun dirname (node)
  (lastcar (pathname-directory (get-pathname node))))

(defmethod display ((node dir))
  (or (slot-value node 'display)
      (format nil "~a/" (dirname node))))

(defmethod display ((node file))
  (or (slot-value node 'display)
      (file-namestring (get-pathname node))))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t)
    (princ (display node) stream)))

(defmethod hidden-p ((node node))
  (or (slot-value node 'hidden-p)
      (equal (uiop:first-char (display node)) #\.)
      (when-let (type (pathname-type (get-pathname node)))
	(member type *hidden-types* :test #'string-equal))))

(defgeneric dir-p (node))
(defmethod dir-p ((node file)) nil)
(defmethod dir-p ((node dir)) t)

(defun node-class (pathname)
  (cond
    ((uiop:directory-pathname-p pathname) 'dir)
    (t 'file)))

(defun make-node (pathname &optional display)
  (make-instance (node-class pathname) :pathname pathname :display display))

(defun make-nodes (pathnames)
  (loop for pathname in pathnames
	for node = (make-node pathname)
	when (or *show-hidden-p* (not (hidden-p node)))
	  collect node))

(defun sort-by-modified (nodes)
  (sort nodes #'> :key 'modified))

(defun sort-by-type (nodes)
  (stable-sort nodes #'(lambda (a b) (and (dir-p a) (not (dir-p b))))))

(defun sort-nodes (nodes)
  (sort-by-type (sort-by-modified nodes)))

(defun list-directory-pathnames (pathname)
  (directory (make-pathname :defaults pathname :name :wild :type :wild)
	     :resolve-symlinks nil))

(defun sorted-nodes (pathname)
  (sort-nodes (make-nodes (list-directory-pathnames pathname))))

(defgeneric parent (node &optional display))

(defmethod parent ((dir dir) &optional display)
  (let ((pathname (get-pathname dir)))
    (when (cdr (pathname-directory pathname))
      (let* ((parent-pathname (uiop:pathname-parent-directory-pathname pathname))
	     (parent-node (make-node parent-pathname display)))
	(setf (initial-selection-node parent-node) dir)
	parent-node))))

(defmethod parent ((file file) &optional display)
  (let* ((pathname (get-pathname file))
	 (dir-pathname (uiop:pathname-directory-pathname pathname))
	 (dir (make-node dir-pathname display)))
    (setf (initial-selection-node dir) file)
    dir))

(defun initial-selection-position (dir nodes)
  (when-let (node (initial-selection-node dir))
    (position (get-pathname node) nodes :key #'get-pathname)))

(defun %from-to (command from to)
  (uiop:run-program
   (append command (list (namestring (get-pathname from))
			 (namestring (get-pathname to))))))

(defgeneric copy-node (node destination))

(defmethod copy-node ((node file) (destination dir))
  (%from-to '("cp") node destination))

(defmethod copy-node ((node dir) (destination dir))
  (%from-to '("cp" "-r") node destination))

(defgeneric move-node (node destination))

(defmethod move-node ((node node) (destination dir))
  (%from-to '("mv") node destination))

(defgeneric delete-node (node))

(defmethod delete-node ((node file))
  (delete-file (get-pathname node)))

(defmethod delete-node ((node dir))
  (let ((pathname (get-pathname node)))
    (if (emptyp (list-directory-pathnames pathname))
	(uiop:delete-empty-directory pathname)
	(when (yes-p "Directory '~a' is not empty.~%DELETE RECURSIVELY?~%" pathname)
	  (uiop:delete-directory-tree
	   pathname
	   :validate #'(lambda (%)
			 (or (forbidden-pathname-p %)
			     (yes-p "~a~%CONFIRM RECURSIVE DELETION.~%" %))))))))

(defmethod make-subdir-pathname (subdir-name dir-node)
  (uiop:ensure-directory-pathname
   (merge-pathnames subdir-name (get-pathname dir-node))))

(defgeneric navigate (entry))
(defgeneric enabled-p (action))
(defgeneric display-continue (action node))
(defgeneric perform-continue (action node))

(defmethod navigate ((object null)))
(defmethod enabled-p (action) t)

(defun input-line (prompt-fmt-string &rest prompt-fmt-args)
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

(defun register-action (class scope position)
  (when scope (setf (getf *scopes* class) scope))
  (when position (setf (getf *positions* class) position)))

(defclass action-entry (entry)
  ((context :initarg :context :reader context)))

(defmacro defaction (class display
		     (&optional scope position)
		     (action-var &optional node-var)
		     &body body)
  (let ((continue-on (find (caar body) '(&context &parent))))
    `(progn
       (defclass ,class (action-entry)
	 ((display :initform ,display)))
       (register-action ',class ',scope ',position)
       ,(if continue-on
	    `(defmethod navigate ((action ,class))
	       (setf *continue* action)
	       (navigate ,(case continue-on
			    (&context `(context action))
			    (&parent `(parent (context action))))))
	    `(defmethod navigate ((,action-var ,class))
	       ,@body))
       ,(when continue-on
          `(defmethod display-continue ((,action-var ,class) ,node-var)
	     ,(cadar body)))
       ,(when continue-on
	  `(defmethod perform-continue ((,action-var ,class) ,node-var)
	     ,@(cdr body)
	     (setf *continue* nil))))))

(defun sorted-actions (context-type &optional direct-only-p)
  (loop for (key value) on *scopes* by #'cddr
	when (if direct-only-p
		 (eq context-type value)
		 (subtypep context-type value))
	  collect (cons key (getf *positions* key)) into types
	finally (return (sort types #'< :key #'(lambda (cons) (or (cdr cons) 999))))))

(defun make-action (class context)
  (make-instance class :context context))

(defun make-actions (context &optional direct-only-p)
  (loop for (class . nil) in (sorted-actions (type-of context) direct-only-p)
	for action = (make-action class context)
	when (enabled-p action)
	  collect action))

(defun clean-actions ()
  (setf *scopes* nil)
  (setf *positions* nil)
  (setf *continue* nil)
  (setf *debug-value* nil))

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

(defun context-not-forbidden (action)
  (not (forbidden-pathname-p (get-pathname (context action)))))

(defun xdg-open (node)
  (uiop:launch-program (list "xdg-open" (namestring (get-pathname node)))))

(defaction xdg-open-file-action "Open" (file 10) (action)
  (xdg-open (context action)))

(defaction copy-file-action "Copy" (file 20) (action dir)
  (&parent (format nil "PASTE: ~a" (display (context action))))
  (copy-node (context action) dir))

(defaction move-file-action "Move" (file 30) (action dir)
  (&parent (format nil "PASTE: ~a" (display (context action))))
  (move-node (context action) dir))

(defaction rename-file-action "Rename" (file 40) (action)
  (let ((file (context action)))
    (when-let (name (input-line "Rename ~a to: " (display file)))
      (let ((new-pathname (merge-pathnames name (get-pathname (parent file)))))
	(if (forbidden-pathname-p new-pathname)
	    (stumpwm::message-no-timeout "Forbidden pathname:~%~a" new-pathname)
	    (progn
	      (sb-unix:unix-rename (namestring (get-pathname file))
				   (namestring new-pathname))
	      (navigate (parent file))))))))

(defaction delete-file-action "Delete" (file 50) (action)
  (let ((file (context action)))
    (when (yes-p "Delete file '~a'? " (get-pathname file))
      (delete-node file))
    (navigate (parent file))))

(defaction xdg-open-dir-action "Open" (dir 10) (action)
  (xdg-open (context action)))

(defaction copy-dir-action "Copy" (dir 20) (action dir)
  (&parent (format nil "PASTE: ~a" (display (context action))))
  (copy-node (context action) dir))

(defaction move-dir-action "Move" (dir 30) (action dir)
  (&parent (format nil "PASTE: ~a" (display (context action))))
  (move-node (context action) dir))

(defmethod enabled-p ((action move-dir-action))
  (context-not-forbidden action))

(defaction rename-dir-action "Rename" (dir 40) (action)
  (let ((dir (context action)))
    (when-let (new-name (input-line "Rename directory '~a' to: " (dirname dir)))
      (let ((new-pathname (make-subdir-pathname new-name (parent dir))))
	(if (forbidden-pathname-p new-pathname)
	    (stumpwm::message-no-timeout "Forbidden pathname:~%~a" new-pathname)
            (progn
	      (sb-unix:unix-rename (namestring (get-pathname dir))
				   (namestring new-pathname))
              (navigate (parent dir))))))))

(defmethod enabled-p ((action rename-dir-action))
  (context-not-forbidden action))

(defaction delete-dir-action "Delete" (dir 60) (action)
  (let ((dir (context action)))
    (when (yes-p "Delete directory '~a'?~%" (get-pathname dir))
      (delete-node dir))
    (navigate (parent dir))))

(defmethod enabled-p ((action delete-dir-action))
  (context-not-forbidden action))

(defaction create-dir-action "Create directory" (dir 70) (action)
  (let ((dir (context action)))
    (when-let (name (input-line "~a~%Create directory: " (get-pathname dir)))
      (ensure-directories-exist (make-subdir-pathname name dir))
      (navigate dir))))

(defclass selected-nodes (dir)
  ((selected :initarg :selected :reader selected)))

(defaction select-nodes "Select multiple" (dir 50) (action)
  (let ((dir (context action)))
    (navigate
     (if-let (nodes (select-entries (sorted-nodes (get-pathname dir))))
       (select-entry (make-actions (make-instance 'selected-nodes
						  :pathname (get-pathname dir)
						  :selected nodes)
				   t)
		     (format nil "~d selected" (length nodes)))
       dir))))

(defaction copy-selected-nodes "Copy" (selected-nodes 10) (action dir)
  (&context (format nil "PASTE (~d selected)" (length (selected (context action)))))
  (dolist (node (selected (context action)))
    (copy-node node dir)))

(defaction move-selected-nodes "Move" (selected-nodes 20) (action dir)
  (&context (format nil "PASTE (~d selected)" (length (selected (context action)))))
  (dolist (node (selected (context action)))
    (move-node node dir)))

(defaction delete-selected-nodes "Delete" (selected-nodes 30) (action)
  (let ((nodes (selected (context action)))
	forbidden)
    (when (yes-p "Delete ~d files? " (length nodes))
      (dolist (node nodes)
	(if (forbidden-pathname-p (get-pathname node))
	    (push node forbidden)
	    (delete-node node))))
    (if forbidden
	(stumpwm::message-no-timeout "Forbidden pathnames: ~{~a~^, ~}" forbidden)
	(navigate (context action)))))

(defaction dir-actions "ACTIONS" () (action)
  (let* ((dir (context action))
	 (pathname (get-pathname dir)))
    (navigate
     (select-entry (cons (make-node pathname "<<<")
			 (make-actions dir))
		   (namestring pathname)))))

(defaction cancel-continue "CANCEL" () (action)
  (setf *continue* nil)
  (navigate (context action)))

(defaction finish-continue nil () (action)
  (let ((dir (context action)))
    (perform-continue *continue* dir)
    (navigate dir)))

(defmethod display ((action finish-continue))
  (display-continue *continue* (context action)))

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
