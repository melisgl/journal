(defun slime-toggle-jtrace-fdefinition (spec)
  "Toggle JTRACE."
  (interactive (list (slime-read-from-minibuffer
                      "j(un)trace: " (slime-symbol-at-point))))
  (message "%s" (slime-eval
                 ;; Silently fail if Journal is not loaded.
                 `(cl:when (cl:find-package :journal)
                           (cl:funcall
                            (cl:find-symbol
                             (cl:symbol-name :swank-toggle-jtrace) :journal)
                            ,spec)))))

(define-key slime-mode-map (kbd "C-c C-j")
  'slime-toggle-jtrace-fdefinition)

(define-key slime-repl-mode-map (kbd "C-c C-j")
  'slime-toggle-jtrace-fdefinition)
