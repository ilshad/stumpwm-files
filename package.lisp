(defpackage #:stumpwm-files
  (:use #:cl #:alexandria)
  (:export

   ;; entry class and its subclasses
   #:entry
   #:node #:dir #:file
   #:action

   ;; common methods
   #:display #:navigate

   ;; filesystem nodes methods
   #:parent #:get-pathname #:hidden-p

   ;; action methods
   #:context #:enabled-p

   ;; filesystem utils
   #:sorted-nodes

   ;; define and call actions
   #:defaction #:make-action #:make-actions

   ;; debug
   #:clean-actions #:sorted-actions

   ;; entry point
   #:menu))
