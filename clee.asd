(in-package :cl-user)

(defpackage :clee-asd
  (:use :cl :asdf))

(in-package :clee-asd)

(defsystem clee
  :version "1.0.0-SNAPSHOT"
  :author "Eitarow Fukamachi"
  :license "MIT"
  :depends-on (:cffi)
  :components ((:file "packages")
               (:file "clee" :depends-on ("packages"))))
