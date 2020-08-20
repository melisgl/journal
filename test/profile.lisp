(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-sprof))

(defun profile-bundle ()
  (jrn:make-file-bundle (asdf:system-relative-pathname
                         :journal
                         "test/test-bundle/")
                        :max-n-failed 1
                        :max-n-completed 1
                        :sync t))

(defun fib (n)
  (jrn:journaled (fib :version :infinity :args `(,n))
    (cond ((= n 0) 1)
          ((= n 1) 1)
          (t (+ (fib (- n 1))
                (fib (- n 2)))))))

(defun xxx (n)
  (loop for i below n do
    (jrn:journaled (xxx :version :infinity :args `(,i)))))

#+nil
(defun foo (n filep)
  (let ((bundle (if filep
                    (profile-bundle)
                    (jrn:make-in-memory-bundle
                     :max-n-failed 1
                     :max-n-completed 1))))
    (loop repeat n do
      (jrn:with-bundle (bundle)
        (fib 10)))))

(defun foo (n filep)
  (let ((bundle (if filep
                    (profile-bundle)
                    (jrn:make-in-memory-bundle
                     :max-n-failed 1
                     :max-n-completed 1))))
    (loop repeat n do
      (jrn:with-bundle (bundle)
        (xxx 177)))))

#+nil
(time (loop repeat 100 do (journal::test)))

#+nil
(time (foo 100 t))

#+nil
(sb-sprof:with-profiling (:report :graph)
  (time (foo 10000 nil)))
