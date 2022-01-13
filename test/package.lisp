(mgl-pax:define-package :journal-test
  (:use #:common-lisp #:journal #:try)
  (:shadowing-import-from #:journal #:event))
