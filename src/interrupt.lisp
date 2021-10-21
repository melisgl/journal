(in-package :journal)

;;;; Signal handling

;;; Only used for self-documenting.
(defmacro async-signal-safe (&body body)
  `(progn ,@body))

#+allegro
(progn
  (defmacro without-interrupts (&body body)
    `(excl:with-delayed-interrupts ,@body))
  (defmacro with-interrupts (&body body)
    `(let ((excl::*without-interrupts* nil))
       ,@body)))

#+ccl
(progn
  (defmacro without-interrupts (&body body)
    `(ccl:without-interrupts ,@body))
  (defmacro with-interrupts (&body body)
    `(ccl:with-interrupts-enabled ,@body)))

#+cmucl
(defmacro without-interrupts (&body body)
  `(sys:without-interrupts ,@body))

#+ecl
(progn
  (defmacro without-interrupts (&body body)
    `(mp:without-interrupts
       (mp:allow-with-interrupts
         ,@body)))
  (defmacro with-interrupts (&body body)
    `(mp:with-interrupts ,@body)))

#+lispworks
(defmacro without-interrupts (&body body)
  `(mp:with-interrupts-blocked ,@body))

#+sbcl
(progn
  (defmacro without-interrupts (&body body)
    `(sb-sys:without-interrupts
       (sb-sys:allow-with-interrupts
         ,@body)))
  (defmacro with-interrupts (&body body)
    `(sb-sys:with-interrupts ,@body)))

(eval-when (:load-toplevel :execute)
  ;; This is very bad. See @SAFETY.
  (unless (fboundp 'without-interrupts)
    (error "WITHOUT-INTERRUPTS not implemented."))
  ;; This is milder, but it means that UNWIND-PROTECT*'s protected
  ;; form will not be interruptible.
  (unless (fboundp 'with-interrupts)
    (warn "~@<WITH-INTERRUPTS not implemented. Some code ~
          will not be interruptible.~:@>")
    (defmacro with-interrupts (&body body)
      `(progn ,@body))))

;;; Recompile with these when doing statistical profiling that relies
;;; on signals. Or when feeling brave.
#+nil
(progn
  (defmacro without-interrupts (&body body)
    `(progn ,@body))
  (defmacro with-interrupts (&body body)
    `(progn ,@body)))

(defmacro unwind-protect* (protected &body cleanup)
  #-ccl
  `(without-interrupts
     (unwind-protect
          (with-interrupts
            ,protected)
       ,@cleanup))
  ;; CCL cleanups are already protected from interrupts.
  #+ccl
  `(unwind-protect
        ,protected
     ,@cleanup))
