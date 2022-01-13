(in-package :journal)

;;;; Signal handling

;;; Only used for self-documenting.
(defmacro async-signal-safe (&body body)
  `(progn ,@body))

(defvar *without-interrupts-available* nil)
(defvar *with-interrupts-available* nil)

#+allegro
(progn
  (setq *without-interrupts-available* t)
  (defmacro without-interrupts (&body body)
    `(excl:with-delayed-interrupts ,@body))
  (setq *with-interrupts-available* t)
  (defmacro with-interrupts (&body body)
    `(let ((excl::*without-interrupts* nil))
       ,@body)))

#+ccl
(progn
  (setq *without-interrupts-available* t)
  (defmacro without-interrupts (&body body)
    `(ccl:without-interrupts ,@body))
  (setq *with-interrupts-available* t)
  (defmacro with-interrupts (&body body)
    `(ccl:with-interrupts-enabled ,@body)))

#+cmucl
(progn
  (setq *without-interrupts-available* t)
  (defmacro without-interrupts (&body body)
    `(sys:without-interrupts ,@body))
  (setq *with-interrupts-available* t)
  (defmacro with-interrupts (&body body)
    `(let ((unix::*interrupts-enabled* t))
       (when unix::*interrupt-pending*
         (unix::do-pending-interrupt))
       ,@body)))

#+ecl
(progn
  (setq *without-interrupts-available* t)
  (defmacro without-interrupts (&body body)
    `(mp:without-interrupts
       (mp:allow-with-interrupts
         ,@body)))
  (setq *with-interrupts-available* t)
  (defmacro with-interrupts (&body body)
    `(mp:with-interrupts ,@body)))

#+lispworks
(progn
  (setq *without-interrupts-available* t)
  (defmacro without-interrupts (&body body)
    `(mp:with-interrupts-blocked ,@body))
  (setq *without-interrupts-available* nil))

#+sbcl
(progn
  (setq *without-interrupts-available* t)
  (defmacro without-interrupts (&body body)
    `(sb-sys:without-interrupts
       (sb-sys:allow-with-interrupts
         ,@body)))
  (setq *with-interrupts-available* t)
  (defmacro with-interrupts (&body body)
    `(sb-sys:with-interrupts ,@body)))

(unless *without-interrupts-available*
  (format *error-output*
          "~&~@<WITHOUT-INTERRUPTS is not implemented on this Lisp. ~
          Proceeding, but any attempt to SYNC-JOURNAL will be a ~
          runtime error. See JOURNAL:@SAFETY for more.~:@>")
  ;; KLUDGE: Quicklisp calls MUFFLE-WARNING on WARNINGs, but it fails
  ;; with no restart available on some Lisps.
  #-(or abcl clisp)
  (signal 'style-warning)
  (defmacro without-interrupts (&body body)
    `(progn ,@body)))

(unless *with-interrupts-available*
  (format *error-output*
          "~&~@<WITH-INTERRUPTS is not implemented on this Lisp. ~
          Some code will not be interruptible.~:@>")
  #-(or abcl clisp)
  (signal 'style-warning)
  (defmacro with-interrupts (&body body)
    `(progn ,@body)))

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
