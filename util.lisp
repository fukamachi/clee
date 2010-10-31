#|
CLEE - Yet another libevent binding for Common Lisp
URL: http://github.com/fukamachi/clee
Copyright (c) 2010 Eitarow Fukamachi <e.arrows@gmail.com>

CLEE is freely distributable under the MIT License (http://www.opensource.org/licenses/mit-license).
|#

(in-package :clee-util)

(defun get-file-handle (stream direction)
  #+ccl (ccl:stream-device stream direction)
  ;; TODO: support more implementations
  #-ccl (error "Your implementation seems to be not supported."))
