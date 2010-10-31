#|
CLEE - Yet another libevent binding for Common Lisp
URL: http://github.com/fukamachi/clee
Copyright (c) 2010 Eitarow Fukamachi <e.arrows@gmail.com>

CLEE is freely distributable under the MIT License (http://www.opensource.org/licenses/mit-license).
|#

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
