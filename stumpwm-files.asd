(asdf:defsystem #:stumpwm-files
  :description "File Manager for StumpWM"
  :author "Ilshad Khabibullin <astoon.net@gmail.com>"
  :license "GPL v3"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:stumpwm)
  :components ((:file "package")
               (:file "module")))
