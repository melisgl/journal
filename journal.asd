;;;; -*-mode: Lisp; coding: utf-8;-*-

;;; See JOURNAL:@JOURNAL-MANUAL for the user guide.
(asdf:defsystem :journal
  :licence "MIT, see COPYING."
  :version "0.1.0"
  :author "Gábor Melis <mega@retes.hu>"
  :homepage "http://github.com/melisgl/journal"
  :bug-tracker "http://github.com/melisgl/journal/issues"
  :source-control (:git "https://github.com/melisgl/journal.git")
  :description "A library for logging, tracing, testing and persistence."
  :depends-on (#:alexandria #:bordeaux-threads #:local-time
               #:mgl-pax #:trivial-features #:trivial-garbage
               (:feature (:not (:or :allegro :sbcl :cmucl)) :osicat))
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "interrupt")
                             (:file "journal")
                             (:file "doc"))))
  :in-order-to ((asdf:test-op (asdf:test-op "journal/test"))))

(asdf:defsystem :journal/test
  :licence "MIT, see COPYING."
  :version "0.0.1"
  :author "Gábor Melis <mega@retes.hu>"
  :description "Tests for Journal."
  :depends-on (#:alexandria #:journal #:try)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "test-journal"))))
  :perform (asdf:test-op (o s)
             (uiop:symbol-call '#:journal-test '#:test)))
