(in-package :journal)

;;;; Register in PAX World

(defun pax-sections ()
  (list @journal-manual))
(defun pax-pages ()
  `((:objects
     (, @journal-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :journal
                      "https://github.com/melisgl/journal"))))
(register-doc-in-pax-world :journal (pax-sections) (pax-pages))

;;; Regenerate documentation
#+nil
(progn
  (update-asdf-system-readmes @journal-manual :journal
                              :formats '(:markdown :plain))
  (let ((*document-downcase-uppercase-code* t))
    (update-asdf-system-html-docs @journal-manual :journal
                                  :pages (pax-pages))))
