(in-package #:stumpwm-files)

(defparameter *show-hidden-p* nil)

(defclass entry ()
  ((display :initarg :display :initform nil :reader display)))

(defclass file (entry)
  ((pathname :initarg :pathname :reader get-pathname)
   (modified :accessor modified)
   (hidden-p :initform nil)))

(defclass dir (file)
  ((initial-selection-entry :initform nil :accessor initial-selection-entry)))

(defmethod initialize-instance :after ((entry file) &key)
  (setf (modified entry) (file-write-date (get-pathname entry))))

(defmethod display ((entry file))
  (or (slot-value entry 'display)
      (let ((p (get-pathname entry)))
	(format nil "~a~@[.~a~]" (pathname-name p) (pathname-type p)))))

(defmethod display ((entry dir))
  (or (slot-value entry 'display)
      (format nil "~a/" (lastcar (pathname-directory (get-pathname entry))))))

(defmethod print-object ((object file) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (display object))))

(defmethod hidden-p ((entry file))
  (or (slot-value entry 'hidden-p)
      (equal (uiop:first-char (display entry)) #\.)))

(defmethod dir-p ((entry file)) nil)
(defmethod dir-p ((entry dir)) t)

(defun file-class (pathname)
  (cond
    ((uiop:directory-pathname-p pathname) 'dir)
    (t 'file)))

(defun make-file-entry (pathname &optional display)
  (make-instance (file-class pathname) :pathname pathname :display display))

(defun file-entries (pathnames)
  (loop for pathname in pathnames
	for file = (make-file-entry pathname)
	when (or *show-hidden-p* (not (hidden-p file)))
	  collect file))

(defun sort-by-modified (files)
  (sort files #'> :key 'modified))

(defun sort-by-type (files)
  (stable-sort files #'(lambda (a b) (and (dir-p a) (not (dir-p b))))))

(defun sort-files (files)
  (sort-by-type (sort-by-modified files)))

(defun list-directory-pathnames (pathname)
  (directory (make-pathname :defaults pathname :name :wild :type :wild)
	     :resolve-symlinks nil))

(defun sorted-file-entries (pathname)
  (sort-files (file-entries (list-directory-pathnames pathname))))

(defun parent (file &optional display)
  (let ((pathname (get-pathname file)))
    (when (cdr (pathname-directory pathname))
      (let* ((parent-pathname (uiop:pathname-parent-directory-pathname pathname))
	     (parent-entry (make-file-entry parent-pathname display)))
	(setf (initial-selection-entry parent-entry) file)
	parent-entry))))

(defun initial-selection-position (dir files)
  (if-let (entry (initial-selection-entry dir))
    (position (get-pathname entry) files :key #'get-pathname)
    0))
