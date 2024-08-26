(in-package #:stumpwm-files)

(defmethod navigate ((object null)))

(defun nav (entries prompt &optional (selected 0))
  (navigate (cadr (stumpwm:select-from-menu
		   (stumpwm:current-screen)
		   (loop for e in entries collect (list (display e) e))
		   prompt
		   selected))))

(defclass action (entry)
  ((context
    :initarg :context
    :reader context)))

(defmethod display ((entry action))
  (slot-value entry 'display))

(defun action-entry (class display &optional context)
  (make-instance class :display display :context context))

(defclass listing-settings (action) ())
(defclass toggle-hidden (action) ())

(defmethod navigate ((entry listing-settings))
  (nav (list (file-entry (get-pathname (context entry)) "<<<")
	     (action-entry 'toggle-hidden
			   (format nil "~:[Show~;Do not show~] hidden files"
				   *show-hidden-p*)
			   (context entry)))
       "SETTINGS"))

(defmethod navigate ((entry toggle-hidden))
  (setf *show-hidden-p* (not *show-hidden-p*))
  (navigate (context entry)))

(defmethod navigate ((entry dir))
  (let* ((pathname (get-pathname entry))
         (files (list-files pathname))
         (parent (parent entry "../"))
	 (settings (action-entry 'listing-settings "SETTINGS" entry)))
    (nav (cons settings (if parent (cons parent files) files))
         (format nil "NAVIGATION ~a" pathname)
	 (+ (initial-selection-position entry files) (if parent 2 1)))))

(defun menu ()
  (let ((stumpwm::*menu-maximum-height* 30))
    (navigate (file-entry (user-homedir-pathname)))))

(stumpwm:defcommand files () () (menu))

;; (stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "j") "files")
