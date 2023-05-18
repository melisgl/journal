;; -*- lexical-binding: t -*-

;;;; Autoloading of JOURNAL on the Common Lisp side

(defcustom mgl-jrn-autoload t
  "If true, then the JOURNAL ASDF system will be loaded as
necessary by `slime-toggle-jtrace-fdefinition'."
  :type 'boolean
  :group 'mgl-jrn)

(defvar mgl-jrn-version '(0 1 0))

(defun mgl-jrn-maybe-autoload (cont)
  (if mgl-jrn-autoload
      (slime-eval-async
          `(cl:progn
            (cl:unless (cl:find-package :journal)
                       (cl:format t ";; Autoloading JOURNAL for Emacs ~
                                     (mgl-jrn-autoload is t).~%")
                       (asdf:load-system "journal")
                       (cl:format t ";; Done autoloading JOURNAL for Emacs~%"))
            (cl:and (cl:find-package :journal)
                    (cl:funcall (cl:find-symbol
                                 (cl:string '#:check-jrn-elisp-version)
                                 :journal)
                                ',mgl-jrn-version)
                    t))
        cont)
    (slime-eval-async
        `(cl:and (cl:find-package :journal)
                 (cl:funcall (cl:find-symbol
                              (cl:string '#:check-jrn-elisp-version)
                              :journal)
                             ',mgl-jrn-version)
                 t)
      cont)))

(defun slime-toggle-jtrace-fdefinition (spec)
  "Toggle JOURNAL:JTRACE. If invoked with the empty string, then
JOURNAL:JUNTRACE all."
  (interactive (list (slime-read-from-minibuffer
                      "j(un)trace: " (slime-symbol-at-point))))
  (mgl-jrn-maybe-autoload
   (lambda (loadedp)
     (if (not loadedp)
         (message "JOURNAL is not loaded. See the variable mgl-jrn-autoload.")
       (message "%s" (slime-eval
                      ;; Silently fail if Journal is not loaded.
                      `(cl:when (cl:find-package :journal)
                                (cl:funcall
                                 (cl:find-symbol
                                  (cl:string '#:swank-toggle-jtrace)
                                  :journal)
                                 ,spec))))))))

(define-key slime-mode-map (kbd "C-c C-j")
  'slime-toggle-jtrace-fdefinition)

(define-key slime-repl-mode-map (kbd "C-c C-j")
  'slime-toggle-jtrace-fdefinition)

(provide 'mgl-jrn)
