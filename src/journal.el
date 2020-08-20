(defun slime-toggle-jtrace-fdefinition (spec)
  "Toggle JTRACE."
  (interactive (list (slime-read-from-minibuffer
                      "j(un)trace: " (slime-symbol-at-point))))
  (message "%s" (slime-eval `(journal::swank-toggle-jtrace ,spec))))

(define-key slime-mode-map (kbd "C-j")
  'slime-toggle-jtrace-fdefinition)

(define-key slime-repl-mode-map (kbd "C-c C-j")
  'slime-toggle-jtrace-fdefinition)
