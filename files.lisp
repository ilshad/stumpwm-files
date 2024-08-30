(in-package #:stumpwm-files)

(defparameter *show-hidden-p* nil)

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

(defmethod display ((node dir))
  (or (slot-value node 'display)
      (format nil "~a/" (lastcar (pathname-directory (get-pathname node))))))

(defmethod display ((node file))
  (or (slot-value node 'display)
      (let ((p (get-pathname node)))
	(format nil "~a~@[.~a~]" (pathname-name p) (pathname-type p)))))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~a" (display node))))

(defmethod hidden-p ((node node))
  (or (slot-value node 'hidden-p)
      (equal (uiop:first-char (display node)) #\.)))

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
