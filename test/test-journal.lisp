(in-package :journal-test)

(define-condition some-error (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "SOME-ERROR was signalled."))))

(define-condition another-error (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "ANOTHER-ERROR was signalled."))))

(defmacro assert-error ((condition-type &optional substring) &body body)
  (alexandria:once-only (substring)
    (alexandria:with-gensyms (got-it c)
      `(let ((,got-it nil))
         (unwind-protect
              (handler-case (progn ,@body)
                (,condition-type (,c)
                  (if (or (null ,substring)
                          (search ,substring
                                  (with-standard-io-syntax
                                    (princ-to-string ,c))))
                      (setq ,got-it t)
                      (setq ,got-it ,c))))
           (assert (eq ,got-it t) ()
                   "~@<Didn't get expected ~S containing ~S. Got a ~S ~
                    saying ~S.~:@>"
                   ',condition-type ,substring (type-of ,got-it)
                   (with-standard-io-syntax
                     (princ-to-string ,got-it))))))))

(defmacro assert-signalled ((condition-type &optional substring) &body body)
  (alexandria:once-only (substring)
    (alexandria:with-gensyms (got-it c)
      `(let ((,got-it nil))
         (unwind-protect
              (handler-bind
                  ((,condition-type (lambda (,c)
                                      (if (or (null ,substring)
                                              (search ,substring
                                                      (with-standard-io-syntax
                                                        (princ-to-string ,c))))
                                          (setq ,got-it t)
                                          (setq ,got-it ,c)))))
                (progn ,@body))
           (assert (eq ,got-it t) ()
                   "~@<Didn't get expected ~S containing ~S. Got ~S.~:@>"
                   ',condition-type ,substring
                   (with-standard-io-syntax
                     (princ-to-string ,got-it))))))))

(defmacro unwind-but-dont-receive (condition-type &body body)
  (alexandria:with-gensyms (c)
    `(handler-case (progn ,@body)
       (,condition-type (,c)
         (assert nil () "Oops. Received condition ~S." ,c)))))

(defun check-file-journal-state (journal state)
  (when (typep journal 'file-journal)
    (assert (probe-file (pathname-of journal)))
    ;; KLUDGE: We want a new instance pointing to the same
    ;; directory.
    (let* ((jrn::*truename-to-file-journal* (make-hash-table :test #'equal))
           (new-journal (make-file-journal (pathname-of journal))))
      (assert (eq (journal-state new-journal) state)))))

(defparameter *make-journal* 'make-in-memory-journal)


(defun test-events-to-frames ()
  (assert (equal (events-to-frames '((:in foo :args (1 2))
                                     (:leaf "l1")
                                     (:in bar :args (7))
                                     (:leaf "l2")
                                     (:out bar :values (8))
                                     (:leaf "l3")
                                     (:in bar2)
                                     (:out bar2 :values ())
                                     (:out foo :values (2))
                                     (:in foo :args (3 4))
                                     (:in bar :args (8))))
                 '(((:in foo :args (1 2))
                    (:leaf "l1")
                    ((:in bar :args (7))
                     (:leaf "l2")
                     (:out bar :values (8)))
                    (:leaf "l3")
                    ((:in bar2)
                     (:out bar2 :values ()))
                    (:out foo :values (2)))
                   ((:in foo :args (3 4))
                    ((:in bar :args (8))))))))

(defun identical-events-p (events-1 events-2)
  (and (= (length events-1) (length events-2))
       (equal events-1 events-2)))

(defun equivalent-events-p (events-1 events-2)
  (and (= (length events-1) (length events-2))
       (every #'event= events-1 events-2)))


;;;; Test recording of log events

(defun test-log-values-record ()
  (let ((journal (funcall *make-journal*)))
    (assert (eq (journal-state journal) :new))
    (with-journaling (:record journal)
      (assert (eq (journal-state journal) :recording))
      (assert (= 42 (journaled (foo) 42)))
      (assert (eq (journal-state journal) :recording))
      (assert (equal (list-events journal)
                     '((:in foo)
                       (:out foo :values (42))))))
    (assert (eq (journal-state journal) :completed))
    (check-file-journal-state journal :completed)))

(defun test-log-condition-record ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (assert (eq (journal-state journal) :recording))
      (assert-error (some-error)
        (journaled ("foo" :condition (constantly 'eh))
          (error 'some-error)
          42))
      (assert (eq (journal-state journal) :recording))
      (assert (equal (list-events journal)
                     '((:in "foo")
                       (:out "foo" :condition eh)))))
    (assert (eq (journal-state journal) :completed))
    (check-file-journal-state journal :completed)))

(defun test-log-error-record ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (assert (eq (journal-state journal) :recording))
      (assert-error (some-error)
        (journaled (foo)
          (error 'some-error)
          42))
      (assert (eq (journal-state journal) :recording))
      (assert (equivalent-events-p (list-events journal)
                                   '((:in foo)
                                     (:out foo :error this-is-not-compared)))))
    (assert (eq (journal-state journal) :completed))
    (check-file-journal-state journal :completed)))

(defun test-log-nlx-record ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (assert (eq (journal-state journal) :recording))
      (assert (= 42 (catch 'foo
                      (journaled (foo)
                        (throw 'foo 42)))))
      (assert (eq (journal-state journal) :recording))
      (assert (equal (list-events)
                     '((:in foo)
                       (:out foo :nlx nil)))))
    (assert (eq (journal-state journal) :completed))
    (check-file-journal-state journal :completed)))


;;;; Test LOG-RECORD

(defun test-log-record-without-with-journaling ()
  (let ((journal (funcall *make-journal*)))
    (assert (eq (journal-state journal) :new))
    (assert (= 42 (framed (foo :log-record journal) 42)))
    (checked (bar) 42)
    (replayed (bar) 42)
    (assert (equal (list-events journal)
                   '((:in foo)
                     (:out foo :values (42)))))
    (assert (eq (journal-state journal) :new))
    ;; TEST-LOG-RECORDING-TO-FAILED-JOURNAL is related.
    (check-file-journal-state journal :failed)))

(defun test-log-record-in-with-journaling ()
  (let ((log-journal (funcall *make-journal*))
        (journal (funcall *make-journal*)))
    (assert (eq (journal-state journal) :new))
    (with-journaling (:record journal)
      (assert (eq (journal-state journal) :recording))
      (framed (foo :log-record log-journal) 42)
      (checked (bar) 42)
      (replayed (bar) 42))
    (assert (eq (journal-state journal) :completed))
    (assert (equal (list-events journal)
                   '((:in bar :version 1)
                     (:out bar :version 1 :values (42))
                     (:in bar :version :infinity)
                     (:out bar :version :infinity :values (42)))))
    (assert (equal (list-events log-journal)
                   '((:in foo)
                     (:out foo :values (42)))))))

(defun test-log-record-to-record-journal ()
  (let ((journal (funcall *make-journal*)))
    (assert (eq (journal-state journal) :new))
    (with-journaling (:record journal)
      (assert (eq (journal-state journal) :recording))
      (framed (foo :log-record journal) 42)
      (checked (bar) 7))
    (assert (eq (journal-state journal) :completed))
    (assert (equal (list-events journal)
                   '((:in foo)
                     (:out foo :values (42))
                     (:in bar :version 1)
                     (:out bar :version 1 :values (7)))))))


;;;; Test LOGGED

(defun test-logged-without-with-journaling ()
  (let ((journal (funcall *make-journal*)))
    (assert (eq (journal-state journal) :new))
    (assert (null (logged (journal) "42")))
    (assert (equal (list-events journal)
                   '((:leaf "42"))))
    (assert (eq (journal-state journal) :new))
    (check-file-journal-state journal :failed)))

(defun test-logged-in-with-journaling ()
  (let ((log-journal (funcall *make-journal*))
        (journal (funcall *make-journal*)))
    (assert (eq (journal-state journal) :new))
    (with-journaling (:record journal)
      (assert (eq (journal-state journal) :recording))
      (logged () "7")
      (logged (log-journal) "42"))
    (assert (eq (journal-state journal) :completed))
    (assert (equal (list-events journal)
                   '((:leaf "7"))))
    (assert (equal (list-events log-journal)
                   '((:leaf "42"))))))

(defun test-logged-to-record-journal ()
  (let ((journal (funcall *make-journal*)))
    (assert (eq (journal-state journal) :new))
    (with-journaling (:record journal)
      (assert (eq (journal-state journal) :recording))
      (logged (journal) "~S" 42))
    (assert (eq (journal-state journal) :completed))
    (assert (equal (list-events journal)
                   '((:leaf "42"))))))


;;;; Test LOG-DECORATOR

(defun test-log-decorator ()
  (let ((*time* t)
        (journal (funcall *make-journal*)))
    (declare (special *time*))
    (assert (eq (journal-state journal) :new))
    (setf (journal-log-decorator journal)
          (make-log-decorator :thread t :time '*time*))
    (framed (foo :log-record journal) 42)
    (destructuring-bind (e1 e2) (list-events journal)
      (assert (getf e1 :time))
      (assert (getf e1 :thread))
      (assert (getf e2 :time))
      (assert (getf e2 :thread)))
    (let ((*time* nil))
      (declare (special *time*))
      (logged (journal) "42")
      (assert (= (length (list-events journal)) 3))
      (destructuring-bind (e3) (subseq (list-events journal) 2)
        (assert (not (getf e3 :time)))
        (assert (getf e3 :thread))))))


;;;;; Test replay corner cases

(defun test-replay-end-of-journal-p-at-in-event ()
  (let ((journal-1 (make-in-memory-journal :state :completed))
        (journal-2 (funcall *make-journal*)))
    (assert-error (end-of-journal)
      (with-journaling (:replay journal-1 :record journal-2
                                :replay-eoj-error-p t)
        (assert (not (journal-divergent-p journal-2)))
        (assert (eq (journal-state journal-2) :recording))
        (checked (foo)
          (assert (journal-divergent-p journal-2))
          42)))
    (assert (eq (journal-state journal-2) :completed))
    (check-file-journal-state journal-2 :completed)))

(defun test-replay-end-of-journal-p-at-out-event ()
  (let ((journal-1 (make-in-memory-journal))
        (journal-2 (funcall *make-journal*)))
    ;; Let's create a truncated journal by hand, where the out-event
    ;; is missing entirely.
    (write-event '(:in foo :version 1) journal-1)
    (setf (jrn::%state journal-1) :completed)
    (assert-error (end-of-journal)
      (with-journaling (:replay journal-1 :record journal-2
                                :replay-eoj-error-p t)
        (assert (eq (journal-state journal-2) :replaying))
        (checked (foo)
          (assert (not (journal-divergent-p journal-2)))
          42)
        (assert (journal-divergent-p journal-2))))
    (assert (eq (journal-state journal-2) :completed))
    (check-file-journal-state journal-2 :completed)))

(defun test-replay-incomplete ()
  (let ((journal-1 (funcall *make-journal*))
        (journal-2 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (checked (foo) 42))
    (assert-error (replay-incomplete)
      (with-journaling (:record journal-2 :replay journal-1)))
    (assert (not (journal-divergent-p journal-2)))
    (assert (eq (journal-state journal-2) :failed))
    (check-file-journal-state journal-2 :failed)))

(defun test-replay-incomplete-without-record ()
  (let ((journal-1 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (checked (foo) 42))
    (assert-error (replay-incomplete)
      (with-journaling (:replay journal-1)))))

;;; Check that REPLAY-INCOMPLETE is not signalled if
;;; WITH-JOURNALING did not finish normally.
(defun test-replay-incomplete-not-signalled-case ()
  (let ((journal-1 (funcall *make-journal*))
        (journal-2 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (checked (foo) 42))
    (assert-error (simple-error "eh")
      (with-journaling (:record journal-2 :replay journal-1)
        (error "eh")))
    (assert (not (journal-divergent-p journal-2)))))

(defun test-replay-incomplete-not-signalled-case-without-record ()
  (let ((journal-1 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (checked (foo) 42))
    (assert-error (simple-error "eh")
      (with-journaling (:replay journal-1)
        (error "eh")))))

(defun test-recording-to-completed-journal ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (= (framed (foo) 42)))
    (assert (eq (journal-state journal) :completed))
    (assert-error (journal-error "is not in state :NEW")
      (with-journaling (:record journal)))))

(defun test-log-recording-to-completed-journal ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (= (framed (foo) 42)))
    (assert (eq (journal-state journal) :completed))
    (assert-error (journal-error "Refusing to log to journal")
      (framed (foo :log-record journal)))))

(defun test-recording-to-failed-journal ()
  (let ((journal (funcall *make-journal*)))
    (setf (jrn::%state journal) :failed)
    (assert-error (journal-error "is not in state :NEW")
      (with-journaling (:record journal)))))

(defun test-recording-to-mismatched-journal ()
  (let ((journal (funcall *make-journal*)))
    (setf (jrn::%state journal) :mismatched)
    (assert-error (journal-error "is not in state :NEW")
      (with-journaling (:record journal)))))

(defun test-log-recording-to-failed-journal ()
  (let ((journal (funcall *make-journal*)))
    (setf (jrn::%state journal) :failed)
    ;; Also see TEST-LOG-RECORD-WITHOUT-WITH-JOURNALING.
    (framed (foo :log-record journal))))

(defun test-skip-log-with-no-record ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (framed (j1)))
    (with-journaling (:replay journal :replay-eoj-error-p t))))

(defun test-recording-after-replay-failure ()
  (let ((journal-1 (funcall *make-journal*))
        (journal-2 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (checked (j1))
      (checked (j1)))
    (with-journaling (:replay journal-1 :record journal-2)
      (assert-error (replay-name-mismatch)
        (checked (j2)))
      (assert (eq (journal-state journal-2) :mismatched))
      (let ((read-position (read-position jrn::*replay-streamlet*)))
        (checked (j3))
        ;; We switch to the :INSERT replay strategy in :MISMATCHED.
        (assert (= read-position (read-position jrn::*replay-streamlet*))))
      (assert (equal (list-events)
                     '((:in j2 :version 1)
                       (:in j3 :version 1)
                       (:out j3 :version 1 :values (nil))))))))


;;;; Recording and replay of versioned events

(defun test-values-replay ()
  (let ((journal-1 (funcall *make-journal*)))
    ;; Record
    (with-journaling (:record journal-1)
      (journaled (foo :version 1)
        (assert (equal (list-events)
                       '((:in foo :version 1))))
        42))
    ;; Check what was recorded.
    (assert (equal (list-events journal-1)
                   '((:in foo :version 1)
                     (:out foo :version 1 :values (42)))))
    (assert (journal-divergent-p journal-1))

    ;; Replay with the same :VALUES outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (with-journaling (:replay journal-1 :record journal-2)
        (assert (eq (journal-state journal-2) :replaying))
        (journaled (foo :version 1)
          (assert (eq (journal-state journal-2) :replaying))
          42)
        (assert (eq (journal-state journal-2) :recording)))
      (assert (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (assert (not (journal-divergent-p journal-2))))

    ;; Replay with different :VALUES outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (assert-error (replay-outcome-mismatch)
        (with-journaling (:replay journal-1 :record journal-2)
          (assert (eq (journal-state journal-2) :replaying))
          (journaled (foo :version 1) 7)
          (assert nil)))
      (assert (equal (list-events journal-2)
                     '((:in foo :version 1)
                       (:out foo :version 1 :values (7)))))
      (assert (eq (journal-state journal-2) :failed))
      (check-file-journal-state journal-2 :failed)
      (assert (journal-divergent-p journal-2)))

    ;; Replay with :CONDITION outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (unwind-but-dont-receive some-error
        (assert-error (replay-outcome-mismatch)
          (with-journaling (:replay journal-1 :record journal-2)
            (journaled (foo :version 1 :condition (constantly 'eh))
              (assert (eq (journal-state journal-2) :replaying))
              (error 'some-error)))))
      (assert (eq (journal-state journal-2) :failed))
      (check-file-journal-state journal-2 :failed)
      (assert (journal-divergent-p journal-2)))

    ;; Replay with :ERROR outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (assert-error (replay-unexpected-outcome)
        (with-journaling (:replay journal-1 :record journal-2)
          (unwind-but-dont-receive some-error
            (journaled (foo :version 1)
              (assert (eq (journal-state journal-2) :replaying))
              (error 'some-error)))))
      (assert (eq (journal-state journal-2) :failed))
      (check-file-journal-state journal-2 :failed)
      (assert (journal-divergent-p journal-2)))

    ;; Replay with :NLX outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (assert-error (replay-unexpected-outcome)
        (catch 'foo
          (with-journaling (:replay journal-1 :record journal-2)
            (journaled (foo :version 1)
              (assert (eq (journal-state journal-2) :replaying))
              (throw 'foo 7)))))
      (assert (eq (journal-state journal-2) :failed))
      (check-file-journal-state journal-2 :failed)
      (assert (journal-divergent-p journal-2)))))

(defun test-condition-replay ()
  (let ((journal-1 (funcall *make-journal*)))
    ;; Record
    (with-journaling (:record journal-1)
      (assert-error (some-error)
        (journaled (foo :version 1 :condition (constantly 'eh))
          (error 'some-error)
          42))
      ;; Check what was recorded.
      (assert (equal (list-events journal-1)
                     '((:in foo :version 1)
                       (:out foo :version 1 :condition eh)))))
    (assert (journal-divergent-p journal-1))

    ;; Replay with any :VALUES outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (assert-error (replay-outcome-mismatch)
        (with-journaling (:replay journal-1 :record journal-2)
          (journaled (foo :version 1)
            (assert (eq (journal-state journal-2) :replaying))
            42)
          (assert nil)))
      (assert (eq (journal-state journal-2) :failed))
      (check-file-journal-state journal-2 :failed)
      (assert (journal-divergent-p journal-2)))

    ;; Replay with the same :CONDITION outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (assert-error (some-error)
        (with-journaling (:replay journal-1 :record journal-2)
          (unwind-protect
               (journaled (foo :version 1 :condition (constantly 'eh))
                 (assert (eq (journal-state journal-2) :replaying))
                 (error 'some-error)
                 42)
            (assert (eq (journal-state journal-2) :recording)))))
      (assert (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (assert (not (journal-divergent-p journal-2))))

    ;; Replay with a different :CONDITION outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (unwind-but-dont-receive some-error
        (assert-error (replay-outcome-mismatch)
          (with-journaling (:replay journal-1 :record journal-2)
            (journaled (foo :version 1 :condition (constantly 'bah))
              (assert (eq (journal-state journal-2) :replaying))
              (error 'some-error)
              42))))
      (assert (eq (journal-state journal-2) :failed))
      (check-file-journal-state journal-2 :failed)
      (assert (journal-divergent-p journal-2)))

    ;; Replay with :ERROR outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (unwind-but-dont-receive some-error
        (assert-error (replay-unexpected-outcome)
          (with-journaling (:replay journal-1 :record journal-2)
            (journaled (foo :version 1)
              (assert (eq (journal-state journal-2) :replaying))
              (error 'some-error)))))
      (assert (eq (journal-state journal-2) :failed))
      (check-file-journal-state journal-2 :failed)
      (assert (journal-divergent-p journal-2)))

    ;; Replay with :NLX outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (assert-error (replay-unexpected-outcome)
        (catch 'foo
          (with-journaling (:replay journal-1 :record journal-2)
            (journaled (foo :version 1)
              (assert (eq (journal-state journal-2) :replaying))
              (throw 'foo 7)))))
      (assert (eq (journal-state journal-2) :failed))
      (check-file-journal-state journal-2 :failed)
      (assert (journal-divergent-p journal-2)))))

(defun test-error-replay ()
  (let ((journal-1 (funcall *make-journal*)))
    ;; Record
    (with-journaling (:record journal-1)
      (assert-error (some-error)
        (journaled (foo :version 1)
          (error 'some-error)
          42))
      ;; Check what was recorded.
      (assert (equivalent-events-p
               (list-events journal-1)
               '((:in foo :version 1)
                 (:out foo :version nil :error this-is-not-compared)))))
    (assert (journal-divergent-p journal-1))

    ;; Replay with any :VALUES outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (= 42 (with-journaling (:replay journal-1 :record journal-2)
              (assert (eq (journal-state journal-2) :replaying))
              (unwind-protect
                   (journaled (foo :version 1) 42)
                (assert (eq (journal-state journal-2) :recording)))))
      (assert (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (assert (journal-divergent-p journal-2)))

    ;; Replay with any :CONDITION outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (assert-error (some-error)
        (with-journaling (:replay journal-1 :record journal-2)
          (unwind-protect
               (journaled (foo :version 1 :condition (constantly 'eh))
                 (assert (eq (journal-state journal-2) :recording))
                 (error 'some-error)
                 42)
            (assert (eq (journal-state journal-2) :recording)))))
      (assert (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (assert (journal-divergent-p journal-2)))

    ;; Replay with the same :ERROR outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (assert-error (some-error)
        (with-journaling (:replay journal-1 :record journal-2)
          (unwind-protect
               (journaled (foo :version 1)
                 (assert (eq (journal-state journal-2) :recording))
                 (error 'some-error))
            (assert (eq (journal-state journal-2) :logging)))))
      (assert (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (assert (not (journal-divergent-p journal-2))))

    ;; Replay with a different :ERROR outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (assert-error (another-error)
        (with-journaling (:replay journal-1 :record journal-2)
          (unwind-protect
               (journaled (foo :version 1)
                 (assert (eq (journal-state journal-2) :recording))
                 (error 'another-error))
            (assert (eq (journal-state journal-2) :logging)))))
      (assert (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (assert (not (journal-divergent-p journal-2))))

    ;; Replay with :NLX outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (= 7 (catch 'foo
             (with-journaling (:replay journal-1 :record journal-2)
               (unwind-protect
                    (journaled (foo :version 1)
                      (assert (eq (journal-state journal-2) :recording))
                      (throw 'foo 7))
                 (assert (eq (journal-state journal-2) :logging))))))
      (assert (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (assert (not (journal-divergent-p journal-2))))))

(defun test-nlx-replay ()
  (let ((journal-1 (funcall *make-journal*)))
    ;; Record
    (with-journaling (:record journal-1)
      (assert (= 42 (catch 'foo
                      (journaled (foo :version 1)
                        (throw 'foo 42))))))
    ;; Check what was recorded.
    (assert (equal (list-events journal-1)
                   '((:in foo :version 1)
                     (:out foo :nlx nil))))
    (assert (journal-divergent-p journal-1))

    ;; Replay with any :VALUES outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (= 42 (with-journaling (:replay journal-1 :record journal-2)
              (unwind-protect
                   (journaled (foo :version 1)
                     (assert (eq (journal-state journal-2) :recording))
                     42)
                (assert (eq (journal-state journal-2) :recording)))))
      (assert (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (assert (journal-divergent-p journal-2)))

    ;; Replay with any :CONDITION outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (assert-error (some-error)
        (with-journaling (:replay journal-1 :record journal-2)
          (unwind-protect
               (journaled (foo :version 1 :condition (constantly 'eh))
                 (assert (eq (journal-state journal-2) :recording))
                 (error 'some-error)
                 42)
            (assert (eq (journal-state journal-2) :recording)))))
      (assert (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (assert (journal-divergent-p journal-2)))

    ;; Replay with any :ERROR outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (assert-error (some-error)
        (with-journaling (:replay journal-1 :record journal-2)
          (unwind-protect
               (journaled (foo :version 1)
                 (assert (eq (journal-state journal-2) :recording))
                 (error 'some-error))
            (assert (eq (journal-state journal-2) :logging)))))
      (assert (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (assert (not (journal-divergent-p journal-2))))

    ;; Replay with :NLX outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (= 7 (catch 'foo
             (with-journaling (:replay journal-1 :record journal-2)
               (unwind-protect
                    (journaled (foo :version 1)
                      (assert (eq (journal-state journal-2) :recording))
                      (throw 'foo 7))
                 (assert (eq (journal-state journal-2) :logging))))))
      (assert (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (assert (not (journal-divergent-p journal-2))))))

(defun test-various-replay-cases ()
  (let ((journal-1 (funcall *make-journal*)))
    ;; Record
    (with-journaling (:record journal-1)
      (journaled (foo :version 2)
        (assert (equal (list-events)
                       '((:in foo :version 2))))
        42))
    ;; Check what was recorded.
    (assert (equal (list-events journal-1)
                   '((:in foo :version 2)
                     (:out foo :version 2 :values (42)))))
    (assert (journal-divergent-p journal-1))

    (dolist (make-journal (list *make-journal* (constantly nil)))
      ;; Same version with mismatched name
      (let ((journal-2 (funcall make-journal)))
        (assert-error (replay-name-mismatch)
          (with-journaling (:replay journal-1 :record journal-2)
            (unwind-protect
                 (journaled (bar :version 2)
                   (assert nil)
                   42)
              (when journal-2
                (assert (eq (journal-state journal-2) :mismatched))))))
        (check-file-journal-state journal-2 :failed)
        (when journal-2
          (assert (journal-divergent-p journal-2))))

      ;; Same name and version with mismatched args
      (let ((journal-2 (funcall make-journal)))
        (assert-error (replay-args-mismatch)
          (with-journaling (:replay journal-1 :record journal-2)
            (unwind-protect
                 (journaled (foo :version 2 :args '(1 2))
                   (assert nil))
              (when journal-2
                (assert (eq (journal-state journal-2) :mismatched))))))
        (check-file-journal-state journal-2 :failed)
        (when journal-2
          (assert (journal-divergent-p journal-2))))

      ;; Higher version. Name is still checked.
      (let ((journal-2 (funcall make-journal)))
        (assert-error (replay-name-mismatch)
          (with-journaling (:replay journal-1 :record journal-2)
            (unwind-protect
                 (journaled (bar :version 3)
                   (assert nil))
              (when journal-2
                (assert (eq (journal-state journal-2) :mismatched))))))
        (check-file-journal-state journal-2 :failed)
        (when journal-2
          (assert (journal-divergent-p journal-2))))

      ;; Higher version. Args, outcome not checked.
      (let ((journal-2 (funcall make-journal)))
        (assert
         (= 7 (with-journaling (:replay journal-1 :record journal-2)
                (unwind-protect
                     (journaled (foo :version 3)
                       (when journal-2
                         (assert (eq (journal-state journal-2) :replaying)))
                       7)
                  (when journal-2
                    (assert (eq (journal-state journal-2) :recording)))))))
        (when journal-2
          (assert (eq (journal-state journal-2) :completed)))
        (check-file-journal-state journal-2 :completed)
        (when journal-2
          (assert (journal-divergent-p journal-2))))

      ;; Upgrade to :INFINITY. Args, outcome not checked.
      (let ((journal-2 (funcall make-journal)))
        (assert
         (= 42 (with-journaling (:replay journal-1 :record journal-2)
                 (unwind-protect
                      (journaled (foo :version :infinity)
                        (when journal-2
                          (assert (eq (journal-state journal-2) :replaying)))
                        (assert nil))
                   (when journal-2
                     (assert (eq (journal-state journal-2) :recording)))))))
        (when journal-2
          (assert (eq (journal-state journal-2) :completed)))
        (check-file-journal-state journal-2 :completed)
        (when journal-2
          (assert (journal-divergent-p journal-2))))

      ;; Lower version.
      (let ((journal-2 (funcall make-journal)))
        (with-journaling (:replay journal-1 :record journal-2)
          (assert-error (replay-version-downgrade)
            (journaled (foo :version 1)))
          (when journal-2
            (assert (eq (journal-state journal-2) :mismatched)))
          (check-file-journal-state journal-2 :failed))
        (check-file-journal-state journal-2 :failed)
        (when journal-2
          (assert (eq (journal-state journal-2) :failed))
          (assert (journal-divergent-p journal-2))))

      ;; Log replay event. Name, args, outcome not checked.
      (let ((journal-2 (funcall make-journal)))
        (assert-error (replay-incomplete)
          (with-journaling (:replay journal-1 :record journal-2)
            (unwind-protect
                 (journaled (bar :version nil :args '(1 2))
                   (when journal-2
                     (assert (eq (journal-state journal-2) :replaying)))
                   7)
              ;; We haven't replayed the FOO frame.
              (when journal-2
                (assert (eq (journal-state journal-2) :replaying))))))
        (when journal-2
          (assert (eq (journal-state journal-2) :failed)))
        (check-file-journal-state journal-2 :failed)
        (when journal-2
          (assert (not (journal-divergent-p journal-2)))))

      ;; Extra log replay event.
      (let ((journal-2 (funcall make-journal)))
        (with-journaling (:replay journal-1 :record journal-2)
          (journaled (log)
            (journaled (foo :version 2)
              42)))
        (when journal-2
          (assert (eq (journal-state journal-2) :completed)))
        (check-file-journal-state journal-2 :completed)
        (when journal-2
          (assert (not (journal-divergent-p journal-2))))))))


;;;; Test recording and replay of external events

(defun test-external-in-log ()
  (let ((n-foo-evals 0))
    (labels ((foo (x y)
               (journaled (foo :version :infinity :args `(,x ,y))
                 (incf n-foo-evals)
                 (+ x y)))
             (bar (x y z)
               (journaled (bar :args `(,x ,y ,z))
                 (values (* z (foo x y)) :second))))
      (assert (equal '(9 :second) (multiple-value-list (bar 1 2 3))))
      (assert (= 1 n-foo-evals))
      (let ((journal-1 (funcall *make-journal*)))
        (with-journaling (:record journal-1)
          (assert (equal (multiple-value-list (bar 1 2 3))
                         '(9 :second)))
          (assert (equal (list-events)
                         '((:in bar :args (1 2 3))
                           (:in foo :version :infinity :args (1 2))
                           (:out foo :version :infinity :values (3))
                           (:out bar :values (9 :second))))))
        (assert (= 2 n-foo-evals))
        (let ((journal-2 (funcall *make-journal*)))
          (with-journaling (:replay journal-1 :record journal-2)
            (assert (equal (multiple-value-list (bar 1 2 3))
                           '(9 :second)))
            (assert (equal (list-events journal-1)
                           (list-events journal-2))))
          (assert (= 2 n-foo-evals)))))))

(defun test-log-in-external ()
  (labels ((foo (x y)
             (journaled (l1 :args `(,x ,y))
               (journaled (v :version 1)
                 (journaled (l2)
                   (+ x y)))))
           (bar (x y z)
             (journaled (bar :version :infinity :args `(,x ,y ,z))
               (values (* z (foo x y)) :second))))
    (assert (equal '(9 :second) (multiple-value-list (bar 1 2 3))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1 :replay-eoj-error-p t)
        (assert (equal (multiple-value-list (bar 1 2 3))
                       '(9 :second)))
        (assert (equal (list-events)
                       '((:in bar :version :infinity :args (1 2 3))
                         (:in l1 :args (1 2))
                         (:in v :version 1)
                         (:in l2)
                         (:out l2 :values (3))
                         (:out v :version 1 :values (3))
                         (:out l1 :values (3))
                         (:out bar :version :infinity :values (9 :second))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (assert (equal (multiple-value-list (bar 1 2 3))
                       '(9 :second)))
        (assert (equal (list-events)
                       '((:in bar :version :infinity :args (1 2 3))
                         (:in l1 :args (1 2))
                         (:in v :version 1)
                         (:in l2)
                         (:out l2 :values (3))
                         (:out v :version 1 :values (3))
                         (:out l1 :values (3))
                         (:out bar :version :infinity
                          :values (9 :second)))))))))

(defun test-non-log-in-external ()
  (labels ((foo (x y)
             (journaled (foo :version 1 :args `(,x ,y))
               (+ x y)))
           (bar (x y z)
             (journaled (bar :version :infinity :args `(,x ,y ,z))
               (values (* z (foo x y)) :second))))
    (assert (equal '(9 :second) (multiple-value-list (bar 1 2 3))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1 :replay-eoj-error-p t)
        (assert (equal (multiple-value-list (bar 1 2 3))
                       '(9 :second)))
        (assert (equal (list-events)
                       '((:in bar :version :infinity :args (1 2 3))
                         (:in foo :version 1 :args (1 2))
                         (:out foo :version 1 :values (3))
                         (:out bar :version :infinity :values (9 :second))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (assert (equal (multiple-value-list (bar 1 2 3))
                       '(9 :second)))
        (assert (equal (list-events)
                       '((:in bar :version :infinity :args (1 2 3))
                         (:in foo :version 1 :args (1 2))
                         (:out foo :version 1 :values (3))
                         (:out bar :version :infinity
                          :values (9 :second)))))))))


;;;; Test :REPLAY-VALUES

(defclass user ()
  ((id :initarg :id :reader user-id)))

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type t)
    (format stream "~S" (slot-value user 'id))))

(defvar *users* (make-hash-table))

(defun find-user (id)
  (gethash id *users*))

(defun add-user (id)
  (setf (gethash id *users*) (make-instance 'user :id id)))

(defparameter *user7* (add-user 7))

(defun get-message ()
  (journaled (listen :version :infinity
                     :values (values-> #'user-id)
                     :replay-values (values<- #'find-user))
    (values *user7* "hello")))

(defun test-replay-values ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (get-message))
    (assert (eq *user7*
                (with-journaling (:replay journal
                                          :record (funcall *make-journal*))
                  (get-message))))))


(defun test-replay-condition ()
  (let ((journal-1 (funcall *make-journal*))
        (journal-2 (funcall *make-journal*)))
    (assert-error (simple-error "some text")
      (with-journaling (:record journal-1)
        (journaled (e :version :infinity
                      :condition (lambda (c)
                                   (when (typep c 'simple-error)
                                     `(error ,(princ-to-string c)))))
          (error "some text"))))
    (assert-error (simple-error "some text")
      (with-journaling (:replay journal-1 :record journal-2)
        (journaled (e :version :infinity :replay-condition #'eval))))
    (assert (not (journal-divergent-p journal-2)))
    (assert (eq (journal-state journal-2) :completed))
    (identical-events-p (list-events journal-2)
                        '((:in e :version :infinity)
                          (:out e :version :infinity
                           :condition (error "some text"))))))

(defun test-external-event-unexpected-outcome ()
  (let* ((journal-1 (funcall *make-journal*))
         (sync (journal-sync journal-1)))
    (assert-error (some-error)
      (with-journaling (:record journal-1)
        (unwind-protect
             (assert-signalled (record-unexpected-outcome "SOME-ERROR")
               (journaled (b :version :infinity)
                 (error 'some-error)))
          (assert (eq (journal-state journal-1) :logging))
          (journaled (l) 1)
          (journaled (v :version 1) 2)
          (assert-error (external-event-downgrade)
            (journaled (f :version :infinity))))))
    (assert (equal (list-events journal-1)
                   (if sync
                       ()
                       '((:in b :version :infinity)
                         ;; Downgraded to log
                         (:out b 
                          :error ("SOME-ERROR" "SOME-ERROR was signalled."))
                         (:in l)
                         (:out l :values (1))
                         ;; Downgraded to log
                         (:in v)
                         (:out v :values (2))))))
    ;; Replay is pretty much the same.
    (let ((journal-2 (funcall *make-journal*)))
      (assert-error (some-error)
        (with-journaling (:record journal-2 :replay journal-1)
          (unwind-protect
               (assert-signalled (record-unexpected-outcome "SOME-ERROR")
                 (journaled (b :version :infinity)
                   (error 'some-error)))
            (assert (eq (journal-state journal-2) :logging))
            (journaled (l) 1)
            (journaled (v :version 1) 2)
            (assert-error (external-event-downgrade)
              (journaled (f :version :infinity)))))))))

(defun test-unwinding-from-record-unexpected-outcome ()
  (let* ((journal-1 (funcall *make-journal*))
         (sync (journal-sync journal-1)))
    (assert-error (record-unexpected-outcome "SOME-ERROR")
      (with-journaling (:record journal-1)
        (unwind-protect
             (unwind-but-dont-receive some-error
               (journaled (b :version :infinity)
                 (error 'some-error)))
          (assert (eq (journal-state journal-1) :logging))
          (journaled (l) 1)
          (journaled (v :version 1) 2)
          (assert-error (external-event-downgrade)
            (journaled (f :version :infinity))))))
    (assert (equal (list-events journal-1)
                   (if sync
                       ()
                       '((:in b :version :infinity)
                         ;; Downgraded to log
                         (:out b
                          :error ("SOME-ERROR" "SOME-ERROR was signalled."))
                         (:in l)
                         (:out l :values (1))
                         ;; Downgraded to log
                         (:in v)
                         (:out v :values (2))))))))

(defun test-nested-external-events-with-unexpected-outcome ()
  (let ((journal-1 (funcall *make-journal*)))
    (assert-error (some-error)
      (with-journaling (:record journal-1)
        (journaled (a :version :infinity)
          (unwind-protect
               (journaled (b :version :infinity)
                 (error 'some-error))
            (assert (eq (journal-state journal-1) :logging))))))
    (assert (equal (list-events journal-1)
                   '((:in a :version :infinity)
                     (:in b :version :infinity)
                     ;; Downgraded to log
                     (:out b
                      :error ("SOME-ERROR" "SOME-ERROR was signalled."))
                     (:out a
                      :error ("SOME-ERROR" "SOME-ERROR was signalled.")))))))

(defun test-expected-type ()
  (assert (string= (funcall (expected-type '(member :a :b)) :a)
                   (with-standard-io-syntax
                     (prin1-to-string 'cl:keyword)))))


;;;; Test replay mismatch

(defun test-replay-mismatch ()
  (let ((journal-1 (funcall *make-journal*)))

    ;; Record
    (with-journaling (:record journal-1)
      (journaled (b1))
      (journaled (b2 :version 1)
        42))
    (assert (journal-divergent-p journal-1))
    (destructuring-bind (record-position replay-position)
        (journal-replay-mismatch journal-1)
      (with-open-journal (streamlet journal-1)
        (setf (read-position streamlet) record-position)
        (assert (equal (read-event streamlet) '(:in b2 :version 1))))
      (assert (null replay-position)))

    ;; Replay diverging with a version upgrade.
    (let ((journal-2 (funcall *make-journal*)))
      (with-journaling (:replay journal-1 :record journal-2)
        (journaled (b2 :version 2)
          7))
      (destructuring-bind (record-position replay-position)
          (journal-replay-mismatch journal-2)
        (with-open-journal (streamlet journal-2)
          (setf (read-position streamlet) record-position)
          (assert (equal (read-event streamlet) '(:in b2 :version 2))))
        (with-open-journal (streamlet journal-1)
          (setf (read-position streamlet) replay-position)
          (assert (equal (read-event streamlet) '(:in b2 :version 1))))))))


;;;; Test inserting

;;; REPLAY-STRATEGY looks at the next event to decide whether to
;;; insert. For OUT-EVENTs, this means the decision whether to insert
;;; is based on the parent frame, while for IN-EVENTs it is based on
;;; the child frame. This test inserts a new frame with the same name
;;; as the parent to trigger the above failure.
(defun test-insertable-next-vs-prev-event ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (journaled (a :version 1)
        (journaled (b :version 1))))
    (with-journaling (:replay journal :record (funcall *make-journal*)
                              :replay-eoj-error-p t)
      (journaled (a :version 1)
        (journaled (a :version 1 :insertable t)
          (journaled (b :version 1))))
      (assert (equal (list-events)
                     '((:in a :version 1)
                       (:in a :version 1)
                       (:in b :version 1)
                       (:out b :version 1 :values (nil))
                       (:out a :version 1 :values (nil))
                       (:out a :version 1 :values (nil))))))))

(defun test-force-insertable ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (journaled (a :version 1)
        (journaled (b :version 1))))
    (with-journaling (:replay journal :record (funcall *make-journal*)
                              :replay-eoj-error-p t)
      (journaled (a :version 1)
        (let ((*force-insertable* t))
          (journaled (a :version 1)
            (let ((*force-insertable* nil))
              (journaled (b :version 1))))))
      (assert (equal (list-events)
                     '((:in a :version 1)
                       (:in a :version 1)
                       (:in b :version 1)
                       (:out b :version 1 :values (nil))
                       (:out a :version 1 :values (nil))
                       (:out a :version 1 :values (nil))))))))

(defun test-insert-before-unexpected ()
  (let ((journal (funcall *make-journal*)))
    (assert-error (some-error)
      (with-journaling (:record journal)
        (journaled (a :version 1)
          (error 'some-error))))
    (with-journaling (:replay journal :record (funcall *make-journal*))
      (journaled (a :version 1)
        (journaled (b :version 1))
        (assert (eq (journal-state (record-journal)) :recording))))))

(defun test-insertable-external ()
  (let ((journal (funcall *make-journal*)))
    (assert-error (journal-error "EXTERNAL-EVENTs cannot be INSERTABLE")
      (with-journaling (:record journal)
        (journaled (a :version :infinity :insertable t))))))

(defun test-force-insertable-vs-external ()
  (let ((journal (funcall *make-journal*))
        (*force-insertable* t))
    (with-journaling (:record journal)
      (journaled (a :version :infinity)))))

(defun test-with-replay-streamlet ()
  (labels ((replaying-old-version-p ()
             (with-replay-streamlet (replay-streamlet)
               (let ((replay-event (peek-event replay-streamlet)))
                 (if (and replay-event
                          (not (eq (event-name replay-event) 'get-approval)))
                     t
                     nil))))
           (get-approval ()
             (if (replaying-old-version-p)
                 t
                 (journaled (get-approval :version :infinity)
                   (= (random 2) 0))))
           (replay-any-version (journal)
             (with-journaling (:replay journal :record t)
               (when (get-approval)
                 (journaled (pay :version :infinity))))))
    ;; Old version of the payment process. Just pay.
    (let ((journal (funcall *make-journal*)))
      (with-journaling (:record journal)
        (journaled (pay :version :infinity)))
      (replay-any-version journal))
    ;; New version of the payment process. Only pay if approved.
    (let ((journal (funcall *make-journal*)))
      (with-journaling (:record journal)
        (when (journaled (get-approval :version :infinity)
                (= (random 2) 0))
          (journaled (pay :version :infinity))))
      (replay-any-version journal))))

(defun test-peek-replay-event ()
  (flet ((run-v2 ()
           (let ((replay-process-event (peek-replay-event)))
             (journaled (process :version 2))
             (when (if (and replay-process-event
                            (< #+sbcl
                               (sb-ext:truly-the
                                fixnum (event-version replay-process-event))
                               #-sbcl
                               (the fixnum
                                    (event-version replay-process-event))
                               2))
                       ;; This will be upgraded to :INFINITY the second
                       ;; time around the LOOP.
                       (journaled (get-approval :version 1 :insertable t)
                         t)
                       (journaled (get-approval :version :infinity)
                         (= (random 2) 0)))
               (journaled (pay :version :infinity))))))
    ;; Start from v1 and upgrade to v2.
    (let ((bundle (make-in-memory-bundle)))
      ;; Old version of the payment process. Just pay.
      (with-bundle (bundle)
        (journaled (log))
        (journaled (process :version 1))
        (journaled (pay :version :infinity)))
      ;; New version of the payment process. Only pay if approved.
      (loop repeat 2 do
        (with-bundle (bundle)
          (run-v2))))
    ;; Now start a fresh run with v2.
    (let ((bundle (make-in-memory-bundle)))
      (loop repeat 2 do
        (with-bundle (bundle)
          (run-v2))))))


;;;; Test upgrades

(defun test-with-replay-filter-with-no-children-left ()
  (labels ((outer (x y)
             (journaled (outer :version 1)
               (inner x y)))
           (inner (x y)
             (journaled (inner :version 1)
               (+ x y)))
           (outer-2 (x y)
             (journaled (outer :version 1)
               (with-replay-filter (:patterns `((:name inner)))
                 (+ x y)))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (outer 1 2)
        (assert
         (equal (list-events)
                '((:in outer :version 1)
                  (:in inner :version 1)
                  (:out inner :version 1 :values (3))
                  (:out outer :version 1 :values (3))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (outer-2 1 2)
        (assert (eq (journal-state (record-journal)) :recording))
        (assert
         (equal (list-events)
                '((:in outer :version 1)
                  (:out outer :version 1 :values (3)))))))))

;;; This the first of the at-end-of-replay tests, designed to test
;;; that the :REPLAYING -> :RECORDING transition takes place. With no
;;; child blocks, this particular case is handled by the
;;; SKIP-EVENTS-AND-MAYBE-SET-STATE-TO-RECORDING call in
;;; WITH-REPLAY-FILTER itself.
(defun test-with-replay-filter-with-no-children-left-at-end-of-replay ()
  (labels ((inner (x y)
             (journaled (inner :version 1)
               (+ x y))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (inner 1 2)
        (assert
         (equal (list-events)
                '((:in inner :version 1)
                  (:out inner :version 1 :values (3))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (with-replay-filter (:patterns '((:name inner)))
          (assert (eq (journal-state (record-journal)) :replaying))
          (+ 1 2))
        (assert (eq (journal-state (record-journal)) :recording))
        (assert (equal (list-events) '()))))))

(defun test-with-replay-filter-with-insert ()
  (labels ((outer (x y)
             (journaled (outer :version 1)
               (inner x y)))
           (inner (x y)
             (journaled (inner :version 1)
               (+ x y)))
           (inserting (x y)
             (journaled (inserting :version 1 :insertable t)
               (+ x y)))
           (outer-2 (x y)
             (journaled (outer :version 1)
               ;; Although, INSERTING gets in between OUTER and INNER,
               ;; INNER still gets filtered.
               (with-replay-filter (:patterns `((:name inner)))
                 (inserting x y)))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (outer 1 2)
        (assert
         (equal (list-events)
                '((:in outer :version 1)
                  (:in inner :version 1)
                  (:out inner :version 1 :values (3))
                  (:out outer :version 1 :values (3))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (outer-2 1 2)
        (assert (eq (journal-state (record-journal)) :recording))
        (assert
         (equal (list-events)
                '((:in outer :version 1)
                  (:in inserting :version 1)
                  (:out inserting :version 1 :values (3))
                  (:out outer :version 1 :values (3)))))))))

;;; This case is also handled by the
;;; SKIP-EVENTS-AND-MAYBE-SET-STATE-TO-RECORDING in
;;; WITH-REPLAY-FILTER, we just test the :INSERTABLE does not screw
;;; things up.
(defun test-with-replay-filter-with-insert-at-end-of-replay ()
  (labels ((inner (x y)
             (journaled (inner :version 1)
               (+ x y)))
           (inserting (x y)
             (journaled (inserting :version 1 :insertable t)
               (+ x y))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (inner 1 2)
        (assert
         (equal (list-events)
                '((:in inner :version 1)
                  (:out inner :version 1 :values (3))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (with-replay-filter (:patterns `((:name inner)))
          (inserting 1 2))
        (assert (eq (journal-state (record-journal)) :recording))
        (assert
         (equal (list-events)
                '((:in inserting :version 1)
                  (:out inserting :version 1 :values (3)))))))))

;;; Handled by the :UPGRADE case in HANDLE-OUT-EVENT.
(defun test-with-replay-filter-with-upgrade-at-end-of-replay ()
  (let ((journal-1 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (journaled (outer :version 1)
        (journaled (inner :version 1)
          42))
      (assert
       (equal (list-events)
              '((:in outer :version 1)
                (:in inner :version 1)
                (:out inner :version 1 :values (42))
                (:out outer :version 1 :values (42))))))
    (with-journaling (:replay journal-1 :record (funcall *make-journal*))
      (with-replay-filter (:patterns '((:name outer)))
        (journaled (inner :version 2)
          7))
      (assert (eq (journal-state (record-journal)) :recording))
      (assert
       (equal (list-events)
              '((:in inner :version 2)
                (:out inner :version 2 :values (7))))))))

(defun test-with-replay-filter-simple-unwrap ()
  (labels ((outer (x y)
             (journaled (outer :version 1)
               (foo-1 x y)))
           (foo-1 (x y)
             (journaled (foo :version 1)
               (foo-2 x y)))
           (foo-2 (x y)
             (journaled (foo :version 2)
               (+ x y)))
           (filtered-outer (x y)
             (journaled (outer :version 1)
               ;; Remove the FOO-1 frame, but keep FOO-2.
               (with-replay-filter (:patterns '((:name foo :version< 2)))
                 (foo-2 x y)))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (outer 1 2)
        (assert (equal (list-events)
                       '((:in outer :version 1)
                         (:in foo :version 1)
                         (:in foo :version 2)
                         (:out foo :version 2 :values (3))
                         (:out foo :version 1 :values (3))
                         (:out outer :version 1 :values (3))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (filtered-outer 1 2)
        (assert (eq (journal-state (record-journal)) :recording))
        (assert (equal (list-events)
                       '((:in outer :version 1)
                         (:in foo :version 2)
                         (:out foo :version 2 :values (3))
                         (:out outer :version 1 :values (3)))))))))

;;; Handled by the :MATCH case in HANDLE-OUT-EVENT.
(defun test-with-replay-filter-simple-unwrap-at-end-of-replay ()
  (labels ((foo-1 (x y)
             (journaled (foo :version 1)
               (foo-2 x y)))
           (foo-2 (x y)
             (journaled (foo :version 2)
               (+ x y))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (foo-1 1 2)
        (assert (equal (list-events)
                       '((:in foo :version 1)
                         (:in foo :version 2)
                         (:out foo :version 2 :values (3))
                         (:out foo :version 1 :values (3))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (with-replay-filter (:patterns '((:name foo :version< 2)))
          (foo-2 1 2))
        (assert (eq (journal-state (record-journal)) :recording))
        (assert (equal (list-events)
                       '((:in foo :version 2)
                         (:out foo :version 2 :values (3)))))))))

(defun test-with-replay-filter-external-unwrap ()
  (labels ((outer (x y)
             (journaled (outer :version 1)
               (foo-1 x y)))
           (foo-1 (x y)
             (journaled (foo :version 1)
               (foo-2 x y)))
           (foo-2 (x y)
             (journaled (foo :version :infinity)
               (+ x y)))
           (filtered-outer (x y)
             (journaled (outer :version 1)
               ;; Remove the FOO-1 frame, but keep FOO-2.
               (with-replay-filter (:patterns '((:name foo :version< 2)))
                 (foo-2 x y)))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (outer 1 2)
        (assert (equal (list-events)
                       '((:in outer :version 1)
                         (:in foo :version 1)
                         (:in foo :version :infinity)
                         (:out foo :version :infinity :values (3))
                         (:out foo :version 1 :values (3))
                         (:out outer :version 1 :values (3))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (filtered-outer 1 2)
        (assert (eq (journal-state (record-journal)) :recording))
        (assert (equal (list-events)
                       '((:in outer :version 1)
                         (:in foo :version :infinity)
                         (:out foo :version :infinity :values (3))
                         (:out outer :version 1 :values (3)))))))))

;;; Handled by the VALUES-EVENT-P branch in MAYBE-REPLAY-OUTCOME.
(defun test-with-replay-filter-external-unwrap-at-end-of-replay ()
  (labels ((foo-1 (x y)
             (journaled (foo :version 1)
               (foo-2 x y)))
           (foo-2 (x y)
             (journaled (foo :version :infinity)
               (+ x y))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (foo-1 1 2)
        (assert (equal (list-events)
                       '((:in foo :version 1)
                         (:in foo :version :infinity)
                         (:out foo :version :infinity :values (3))
                         (:out foo :version 1 :values (3))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (with-replay-filter (:patterns '((:name foo :version< 2)))
          (foo-2 1 2))
        (assert (eq (journal-state (record-journal)) :recording))
        (assert (equal (list-events)
                       '((:in foo :version :infinity)
                         (:out foo :version :infinity :values (3)))))))))

;;; Handled by the CONDITION-EVENT-P branch in MAYBE-REPLAY-OUTCOME.
(defun test-with-replay-filter-external-condition-unwrap-at-end-of-replay ()
  (labels ((foo-1 ()
             (journaled (foo :version 1)
               (assert-error (simple-error "eh")
                 (foo-2))
               nil))
           (foo-2 ()
             (journaled (foo :version :infinity
                             :condition (lambda (condition)
                                          (when (typep condition 'simple-error)
                                            (princ-to-string condition))))
               (error "eh"))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (foo-1)
        (assert (equal (list-events)
                       '((:in foo :version 1)
                         (:in foo :version :infinity)
                         (:out foo :version :infinity :condition "eh")
                         (:out foo :version 1
                          :values (nil))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (assert-error (simple-error "eh")
          (with-replay-filter (:patterns '((:name foo :version< 2)))
            (foo-2)))
        (assert (eq (journal-state (record-journal)) :recording))
        (assert (equal (list-events)
                       '((:in foo :version :infinity)
                         (:out foo :version :infinity :condition "eh"))))))))

(defun test-with-replay-filter-unwrap ()
  (labels ((outer (x y)
             (journaled (outer :version 1)
               (foo-1 x y)
               (foo-1-1 x y)))
           (foo-1 (x y)
             (journaled (foo :version 1)
               (foo-2 x y)))
           (foo-2 (x y)
             (journaled (foo :version 2)
               (foo-3 x y)))
           (foo-3 (x y)
             (journaled (foo :version 3)
               (bar (+ x y))))
           (bar (x)
             (journaled (bar :version 1)
               x))
           (foo-1-1 (x y)
             (journaled (foo :version 1)
               (foo-inf x y)))
           (foo-inf (x y)
             (journaled (foo-inf :version :infinity)
               (foo-1 x y)))
           (filtered-outer (x y)
             (journaled (outer :version 1)
               ;; Remove the FOO-1 and FOO-2 frames around FOO-3 and
               ;; FOO-INF, but not within FOO-INF.
               (let ((v 3))
                 (with-replay-filter (:patterns `((:name foo :version< ,v)))
                   (foo-3 x y)
                   (foo-inf x y))))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (outer 1 2)
        (assert (equal (list-events)
                       '((:in outer :version 1)
                         (:in foo :version 1)
                         (:in foo :version 2)
                         (:in foo :version 3)
                         (:in bar :version 1)
                         (:out bar :version 1 :values (3))
                         (:out foo :version 3 :values (3))
                         (:out foo :version 2 :values (3))
                         (:out foo :version 1 :values (3))
                         (:in foo :version 1)
                         (:in foo-inf :version :infinity)
                         (:in foo :version 1)
                         (:in foo :version 2)
                         (:in foo :version 3)
                         (:in bar :version 1)
                         (:out bar :version 1 :values (3))
                         (:out foo :version 3 :values (3))
                         (:out foo :version 2 :values (3))
                         (:out foo :version 1 :values (3))
                         (:out foo-inf :version :infinity :values (3))
                         (:out foo :version 1 :values (3))
                         (:out outer :version 1 :values (3))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (filtered-outer 1 2)
        (assert (eq (journal-state (record-journal)) :recording))
        (assert (equal (list-events)
                       '((:in outer :version 1)
                         (:in foo :version 3)
                         (:in bar :version 1)
                         (:out bar :version 1 :values (3))
                         (:out foo :version 3 :values (3))
                         (:in foo-inf :version :infinity)
                         (:in foo :version 1)
                         (:in foo :version 2)
                         (:in foo :version 3)
                         (:in bar :version 1)
                         (:out bar :version 1 :values (3))
                         (:out foo :version 3 :values (3))
                         (:out foo :version 2 :values (3))
                         (:out foo :version 1 :values (3))
                         (:out foo-inf :version :infinity :values (3))
                         (:out outer :version 1 :values (3)))))))))

;;; Check that we don't filter replay events of the parent and above.
(defun test-with-replay-filter-parent-intact ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (journaled (outer :version 1)
        3)
      (assert (equal (list-events)
                     '((:in outer :version 1)
                       (:out outer :version 1 :values (3))))))
    (with-journaling (:replay journal :record (funcall *make-journal*)
                              ;; If we had eaten the out-event of the
                              ;; enclosing OUTER block, then when that
                              ;; event is triggered again there would
                              ;; be no match for it in the replay.
                              ;; Make sure that's an error.
                              :replay-eoj-error-p t)
      (journaled (outer :version 1)
        (with-replay-filter (:patterns `((:name outer)))
          3))
      (assert (eq (journal-state (record-journal)) :recording))
      (assert (equal (list-events)
                     '((:in outer :version 1)
                       (:out outer :version 1 :values (3))))))))

;;; When it's undecidable whether the events that could be filtered
;;; actually fall within the dynamic extent of replay, choose to
;;; filter them.
(defun test-with-replay-filter-greed ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (journaled (j2 :version 2))
      (journaled (j2 :version 1)))
    (with-journaling (:replay journal :replay-eoj-error-p t)
      (with-replay-filter (:patterns '((:name j2)))))))

(defun test-with-replay-filter-nesting ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (journaled (j1 :version 1)
        (journaled (j2 :version 3))
        (journaled (j2 :version 2)
          (journaled (j3 :version 1)))
        (journaled (j2 :version 1))))
    (with-journaling (:replay journal :replay-eoj-error-p t)
      (with-replay-filter (:patterns '((:name j1)))
        (journaled (j2 :version 3))
        ;; Due to greediness J2/1 is consumed, too.
        (with-replay-filter (:patterns '((:name j2)))
          (journaled (j3 :version 1)))))))

(defun test-with-replay-filter-recursive ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (journaled (j1 :version 1)
        (journaled (j2 :version 1)
          (journaled (j1 :version 2)
            (journaled (j1 :version 3)
              (journaled (j1 :version 3)))))))
    (with-journaling (:replay journal :replay-eoj-error-p t)
      (with-replay-filter (:patterns '((:name j1)))
        (journaled (j2 :version 1))))))

(defun test-with-replay-filter-with-imbalanced-log-events ()
  (let ((journal-1 (funcall *make-journal*))
        (journal-2 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (assert-error (some-error)
        ;; J1's in-event is a VERSIONED-EVENT, but the out-event is a
        ;; LOG-EVENT because we switch to :INSERT replay strategy on
        ;; :ERROR.
        (checked (j1)
          (checked (j2))
          (error 'some-error))))
    (with-journaling (:replay journal-1 :record journal-2)
      (with-replay-filter (:patterns '())
        (checked (j1)
          (checked (j2)))))))


;;;; Test replay restarts

(defun test-replay-restarts ()
  (let ((journal-1 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (checked (c1 :args '(1) :version 2)
        42))
    ;; :UPGRADE on REPLAY-NAME-MISMATCH
    (handler-bind ((replay-name-mismatch
                     (lambda (c)
                       (declare (ignore c))
                       (invoke-restart 'replay-force-upgrade))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (checked (other-name)
          42)
        (assert (equal (list-events)
                       '((:in other-name :version 1)
                         (:out other-name :version 1 :values (42)))))))
    ;; :INSERT on REPLAY-NAME-MISMATCH
    (handler-bind ((replay-name-mismatch
                     (lambda (c)
                       (declare (ignore c))
                       (invoke-restart 'replay-force-insert))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (checked (other-name)
          (checked (c1 :args '(1) :version 2)
            42))
        (assert (equal (list-events)
                       '((:in other-name :version 1)
                         (:in c1 :version 2 :args (1))
                         (:out c1 :version 2 :values (42))
                         (:out other-name :version 1 :values (42)))))))
    ;; :UPGRADE on REPLAY-VERSION-DOWNGRADE
    (handler-bind ((replay-version-downgrade
                     (lambda (c)
                       (declare (ignore c))
                       (invoke-restart 'replay-force-upgrade))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (checked (c1 :args '(1) :version 1)
          42)
        (assert (equal (list-events)
                       '((:in c1 :version 1 :args (1))
                         (:out c1 :version 1 :values (42)))))))
    ;; :UPGRADE on REPLAY-ARGS-MISMATCH
    (handler-bind ((replay-args-mismatch
                     (lambda (c)
                       (declare (ignore c))
                       (invoke-restart 'replay-force-upgrade))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (checked (c1 :args '(7) :version 2)
          42)
        (assert (equal (list-events)
                       '((:in c1 :version 2 :args (7))
                         (:out c1 :version 2 :values (42)))))))
    ;; :UPGRADE on REPLAY-OUTCOME-MISMATCH
    (handler-bind ((replay-outcome-mismatch
                     (lambda (c)
                       (declare (ignore c))
                       (invoke-restart 'replay-force-upgrade))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (checked (c1 :args '(1) :version 2)
          7)
        (assert (equal (list-events)
                       '((:in c1 :version 2 :args (1))
                         (:out c1 :version 2 :values (7)))))))))


(defun test-events ()
  ;; Recording of log events
  (test-log-values-record)
  (test-log-condition-record)
  (test-log-error-record)
  (test-log-nlx-record)
  ;; LOG-RECORD
  (test-log-record-without-with-journaling)
  (test-log-record-in-with-journaling)
  (test-log-record-to-record-journal)
  ;; LOGGED
  (test-logged-without-with-journaling)
  (test-logged-in-with-journaling)
  (test-logged-to-record-journal)
  ;; LOG-DECORATOR
  (test-log-decorator)
  ;; Replay corner cases
  (test-replay-end-of-journal-p-at-in-event)
  (test-replay-end-of-journal-p-at-out-event)
  (test-replay-incomplete)
  (test-replay-incomplete-without-record)
  (test-replay-incomplete-not-signalled-case)
  (test-replay-incomplete-not-signalled-case-without-record)
  (test-recording-to-completed-journal)
  (test-log-recording-to-completed-journal)
  (test-recording-to-failed-journal)
  (test-recording-to-mismatched-journal)
  (test-log-recording-to-failed-journal)
  (test-skip-log-with-no-record)
  (test-recording-to-mismatched-journal)
  ;; Recording and replay of versioned events
  (test-values-replay)
  (test-condition-replay)
  (test-error-replay)
  (test-nlx-replay)
  (test-various-replay-cases)
  ;; Recording and replay of external events
  (test-external-in-log)
  (test-log-in-external)
  (test-non-log-in-external)
  (test-replay-values)
  (test-replay-condition)
  (test-external-event-unexpected-outcome)
  (test-unwinding-from-record-unexpected-outcome)
  (test-nested-external-events-with-unexpected-outcome)
  (test-expected-type)
  ;; Replay mismatch
  (test-replay-mismatch)
  ;; Inserting
  (test-insertable-next-vs-prev-event)
  (test-force-insertable-vs-external)
  (test-insert-before-unexpected)
  (test-insertable-external)
  (test-force-insertable-vs-external)
  (test-with-replay-streamlet)
  (test-peek-replay-event)
  ;; Upgrades
  (test-with-replay-filter-with-no-children-left)
  (test-with-replay-filter-with-no-children-left-at-end-of-replay)
  (test-with-replay-filter-with-insert)
  (test-with-replay-filter-with-insert-at-end-of-replay)
  (test-with-replay-filter-with-upgrade-at-end-of-replay)
  (test-with-replay-filter-simple-unwrap)
  (test-with-replay-filter-simple-unwrap-at-end-of-replay)
  (test-with-replay-filter-external-unwrap)
  (test-with-replay-filter-external-unwrap-at-end-of-replay)
  (test-with-replay-filter-external-condition-unwrap-at-end-of-replay)
  (test-with-replay-filter-unwrap)
  (test-with-replay-filter-parent-intact)
  (test-with-replay-filter-greed)
  (test-with-replay-filter-nesting)
  (test-with-replay-filter-recursive)
  (test-with-replay-filter-with-imbalanced-log-events)
  ;; Replay restarts
  (test-replay-restarts))

(defun test-in-memory-journal ()
  (test-events))

(defun test-file-journal ()
  (call-with-file-journal-settings #'test-events))

(defun call-with-file-journal-settings (fn)
  (dolist (sync '(nil t))
    (let ((files-created ()))
      (flet ((make-temp-file-journal ()
               (let ((pathname (cl-fad:with-open-temporary-file (stream)
                                 (pathname stream))))
                 (push pathname files-created)
                 (make-file-journal pathname :sync sync))))
        (let ((*make-journal* #'make-temp-file-journal))
          (unwind-protect
               (funcall fn)
            (dolist (file files-created)
              (ignore-errors (delete-file file)))))))))


(defun test-io-direction ()
  (let ((journal (make-in-memory-journal :state :completed)))
    (with-open-journal (streamlet journal :direction :input)))
  (assert-error (file-error)
    ;; This file does not exist.
    (let ((journal (make-file-journal "sd8f76876dsaf,cv")))
      (with-open-journal (streamlet journal :direction :input))))
  (assert-error (streamlet-error "is not an output streamlet")
    (let ((journal (make-in-memory-journal)))
      (with-open-journal (streamlet journal :direction :input)
        (write-event (make-in-event) streamlet))))
  (assert-error (streamlet-error "is not an input streamlet")
    (let ((journal (make-in-memory-journal)))
      (with-open-journal (streamlet journal :direction :output)
        (read-event streamlet)))))


(defun test-in-memory-bundle ()
  (let ((bundle (make-in-memory-bundle :max-n-failed 1)))
    (assert (= 0 (length (jrn::journals bundle))))

    ;; Do a quick successful run.
    (with-bundle (bundle)
      (check-in-memory-bundle bundle '(:recording))
      (journaled (foo :version 1 :args '(1 2))
        (check-in-memory-bundle bundle '(:recording))
        42))
    (check-in-memory-bundle bundle '(:completed))

    ;; Now fail.
    (assert-error (replay-name-mismatch)
      (with-bundle (bundle)
        (check-in-memory-bundle bundle '(:replaying :completed))
        (unwind-protect
             (journaled (bar :version 1 :args '(1 2))
               (assert nil))
          (check-in-memory-bundle bundle '(:mismatched :completed)))))
    (check-in-memory-bundle bundle '(:failed :completed))

    ;; Fail again, differently, and hit MAX-N-FAILED.
    (let ((journals-before (coerce (copy-seq (jrn::journals bundle)) 'list)))
      (assert-error (replay-outcome-mismatch)
        (with-bundle (bundle)
          (check-in-memory-bundle bundle '(:replaying :failed :completed))
          (unwind-protect
               (journaled (foo :version 1 :args '(1 2))
                 7)
            (check-in-memory-bundle
             bundle '(:mismatched :failed :completed)))))
      (check-in-memory-bundle bundle '(:failed :completed))
      ;; Check that the least recent one was removed.
      (assert (not (equal journals-before
                          (coerce (jrn::journals bundle) 'list)))))

    ;; Try again and succeed.
    (with-bundle (bundle)
      (check-in-memory-bundle bundle '(:replaying :failed :completed))
      (journaled (foo :version 1 :args '(1 2))
        42)
      (check-in-memory-bundle bundle '(:recording :failed :completed)))

    ;; Check that REAP-IDENTICAL-OR-NON-DIVERGENT-JOURNALS removes the latest.
    (check-in-memory-bundle bundle '(:failed :completed))))

(defun check-in-memory-bundle (bundle states)
  (assert (equal (mapcar #'journal-state (jrn::journals bundle))
                 states)))

(defun test-file-bundle ()
  (call-with-file-bundle #'test-file-bundle*))

(defun call-with-file-bundle (fn)
  ;; CL-FAD sets up logical pathname translation for TEMPORARY-FILES.
  (let* ((random-string (format nil "~:@(~36,8,'0R~)"
                                (random (expt 36 8) (make-random-state t))))
         (directory (merge-pathnames
                     (format nil "journal-test-~A/" random-string)
                     (translate-logical-pathname "TEMPORARY-FILES:")))
         (bundle (make-file-bundle directory :max-n-failed 1
                                   :max-n-completed nil)))
    (unwind-protect
         (funcall fn bundle)
      (delete-file-bundle directory))))

(defun test-file-bundle* (bundle)
  (assert (= 0 (length (jrn::journals bundle))))

  ;; Do a quick successful run.
  (with-bundle (bundle)
    (check-file-bundle bundle '((0 :recording)) '((0 :completed)))
    (journaled (foo :version 1 :args '(1 2))
      (check-file-bundle bundle '((0 :recording)) '((0 :completed)))
      42))
  (check-file-bundle bundle '((0 :completed)) '((0 :completed)))

  ;; Now fail.
  (assert-error (replay-name-mismatch)
    (with-bundle (bundle)
      (check-file-bundle bundle '((1 :replaying) (0 :completed))
                         '((1 :failed) (0 :completed)))
      (unwind-protect
           (journaled (bar :version 1 :args '(1 2))
             (assert nil))
        (check-file-bundle bundle '((1 :mismatched) (0 :completed))
                           '((1 :failed) (0 :completed))))))
  (check-file-bundle bundle '((1 :failed) (0 :completed))
                     '((1 :failed) (0 :completed)))

  ;; Fail again (but differently to avoid the identical branch in
  ;; JRN::REAP-IDENTICAL-OR-NON-DIVERGENT-JOURNALS) and hit MAX-N-FAILED.
  (assert-error (replay-outcome-mismatch)
    (with-bundle (bundle)
      (check-file-bundle bundle '((2 :replaying) (1 :failed) (0 :completed))
                         '((2 :failed) (1 :failed) (0 :completed)))
      (unwind-protect
           (journaled (foo :version 1 :args '(1 2))
             7)
        (check-file-bundle bundle
                           '((2 :mismatched) (1 :failed) (0 :completed))
                           '((2 :failed) (1 :failed) (0 :completed))))))
  (check-file-bundle bundle '((2 :failed) (0 :completed))
                     '((2 :failed) (0 :completed)))

  ;; Try again and succeed.
  (with-bundle (bundle)
    (check-file-bundle bundle '((3 :replaying) (2 :failed) (0 :completed))
                       '((3 :failed) (2 :failed) (0 :completed)))
    (journaled (foo :version 1 :args '(1 2))
      42)
    (check-file-bundle bundle '((3 :recording) (2 :failed) (0 :completed))
                       '((3 :completed) (2 :failed) (0 :completed))))

  ;; Check that REAP-IDENTICAL-OR-NON-DIVERGENT-JOURNALS removes the latest.
  (check-file-bundle bundle '((2 :failed) (0 :completed))
                     '((2 :failed) (0 :completed))))

(defun check-file-bundle (bundle ids-and-states reloaded-ids-and-states)
  (flet ((ids-and-states (bundle)
           (mapcar (lambda (journal)
                     (list (jrn::bundle-file-version (pathname-of journal))
                           (journal-state journal)))
                   (jrn::journals bundle))))
    (assert (equal (ids-and-states bundle) ids-and-states))
    (let ((reloaded-bundle
            ;; KLUDGE: We want a new instance pointing to the same
            ;; directory, and to populate it with new FILE-JOURNALS.
            (let ((jrn::*truename-to-file-bundle* (make-hash-table
                                                   :test #'equal))
                  (jrn::*truename-to-file-journal* (make-hash-table
                                                    :test #'equal)))
              (make-file-bundle (directory-of bundle)))))
      (assert (equal (ids-and-states reloaded-bundle)
                     reloaded-ids-and-states)))))


;;;; Synchronization

(defun test-file-sync ()
  (let* ((random-string (format nil "~:@(~36,8,'0R~)"
                                (random (expt 36 8) (make-random-state t))))
         (filename (merge-pathnames
                    (format nil "file-sync-test-~A" random-string)
                    (translate-logical-pathname "TEMPORARY-FILES:"))))
    (with-standard-io-syntax
      (with-open-file (stream filename :direction :output)
        ;; :COMPLETED
        (write-char #\Newline stream)
        (prin1 '(:in a :version 1) stream)
        ;; Committed transaction
        (write-char #\Ack stream)
        (prin1 '(:in b :version 1) stream)
        (prin1 '(:in c :version 1) stream)
        ;; Unfinished transaction
        (write-char #\Del stream)
        (prin1 '(:in d :version 1) stream)))
    (assert (equal (list-events (make-file-journal filename :sync t))
                   '((:in a :version 1)
                     (:in b :version 1)
                     (:in c :version 1))))))

(defun test-file-sync-garbage ()
  (let* ((random-string (format nil "~:@(~36,8,'0R~)"
                                (random (expt 36 8) (make-random-state t))))
         (filename (merge-pathnames
                    (format nil "file-sync-test-~A" random-string)
                    (translate-logical-pathname "TEMPORARY-FILES:"))))
    (with-standard-io-syntax
      (with-open-file (stream filename :direction :output)
        ;; :COMPLETED
        (write-char #\Newline stream)
        (prin1 '(:in a :version 1) stream)
        ;; Committed transaction
        (write-char #\Ack stream)
        (prin1 '(:in b :version 1) stream)
        (prin1 '(:in c :version 1) stream)
        ;; Unfinished transaction
        (write-char #\Del stream)
        (format stream "(:in d :ve")))
    (assert (equal (list-events (make-file-journal filename :sync t))
                   '((:in a :version 1)
                     (:in b :version 1)
                     (:in c :version 1))))))

(defun test-sync-t ()
  (let ((db ())
        (db-since-prev-sync ())
        (sync-pos nil)
        (n-syncs 0))
    (labels ((sync-to-db (journal)
               (when (and (member (journal-state journal)
                                  '(:recording :logging :completed))
                          (journal-divergent-p journal))
                 (setq db (coerce (journal-events journal) 'list))
                 (setq sync-pos (journal-previous-sync-position journal))
                 (setq db-since-prev-sync
                       (coerce (subseq (journal-events journal) sync-pos)
                               'list))
                 (incf n-syncs)))
             (make-db-backed-record-journal ()
               (make-in-memory-journal :sync-fn #'sync-to-db))
             (make-db-backed-replay-journal ()
               (make-in-memory-journal :events db)))
      (with-journaling (:record (make-db-backed-record-journal)
                                :replay (make-db-backed-replay-journal))
        (journaled (log))
        (assert (= n-syncs 0))
        (journaled (versioned :version 1))
        (assert (= n-syncs 0))
        (journaled (a :version :infinity)
          (assert (= n-syncs 0))
          2)
        (assert (equal db-since-prev-sync
                       '((:in log)
                         (:out log :values (nil))
                         (:in versioned :version 1)
                         (:out versioned :version 1 :values (nil))
                         (:in a :version :infinity)
                         (:out a :version :infinity :values (2)))))
        (assert (= sync-pos 0))
        (assert (= n-syncs 1))
        (assert-error (some-error)
          (journaled (b :version :infinity)
            (assert (= n-syncs 1))
            (error 'some-error)))
        (assert (= n-syncs 1)))
      ;; SYNC-STREAMLET is called in CLOSE-STREAMLET, but it does
      ;; nothing because no events were written since the previous
      ;; sync.
      (assert (= n-syncs 2))

      ;; Replay
      (setq db-since-prev-sync ()
            sync-pos nil
            n-syncs 0)
      (with-journaling (:record (make-db-backed-record-journal)
                                :replay (make-db-backed-replay-journal))
        ;; We don't repeat the log block, it doesn't matter.
        (journaled (versioned :version 1))
        (assert (= n-syncs 0))
        (journaled (a :version :infinity)
          (assert (= n-syncs 0))
          2)
        (assert (= n-syncs 0))
        ;; This fixes up the error in the previous run and must
        ;; trigger a sync.
        (journaled (b :version :infinity)
          3)
        (assert (= n-syncs 1))
        (assert (= sync-pos 0))
        (assert (equal db-since-prev-sync
                       '((:in versioned :version 1)
                         (:out versioned :version 1 :values (nil))
                         (:in a :version :infinity)
                         (:out a :version :infinity :values (2))
                         (:in b :version :infinity)
                         (:out b :version :infinity :values (3)))))
        ;; Extend the replay with events that don't trigger syncing ...
        (journaled (c :version 1))
        (assert (= n-syncs 1)))
      ;; ... and check that CLOSE-STREAMLET syncs.
      (assert (= n-syncs 2))

      ;; Second replay
      (setq db-since-prev-sync ()
            sync-pos nil
            n-syncs 0)
      (with-journaling (:record (make-db-backed-record-journal)
                                :replay (make-db-backed-replay-journal))
        (journaled (versioned :version 1))
        (assert (= n-syncs 0))
        (journaled (a :version :infinity)
          (assert (= n-syncs 0))
          2)
        (assert (= n-syncs 0))
        (journaled (b :version :infinity)
          3)
        (assert (= n-syncs 0))
        (journaled (c :version 1))
        (assert (not (journal-divergent-p (record-journal))))
        ;; No sync upon :REPLAYING -> :RECORDING.
        (assert (= n-syncs 0)))
      ;; No sync in CLOSE-STREAMLET.
      (assert (= n-syncs 0)))))


(defun foo (x)
  (1+ x))

(defun bar (x)
  (foo (+ x 2))
  (error "xxx"))

(defun test-jtrace ()
  (let ((*package* (find-package :journal-test)))
    (juntrace)
    (jtrace foo bar)
    (assert (equal (sort (jtrace) #'string< :key #'symbol-name) '(bar foo)))
    (assert (equal
             (with-output-to-string (*trace-output*)
               (foo 1))
             (format nil "~%(FOO 1)~%=> 2")))
    (assert (equal
             (let ((*trace-pretty* nil))
               (with-output-to-string (*trace-output*)
                 (foo 1)))
             (format nil "~%(:IN FOO :ARGS (1))~%~
                        (:OUT FOO :VALUES (2))")))
    (assert (equal
             (with-output-to-string (*trace-output*)
               (assert-error (simple-error "xxx")
                 (bar 1)))
             (format nil "~%(BAR 1)~%  (FOO 3)~%  => 4~%~
                       =E \"SIMPLE-ERROR\" \"xxx\"")))
    (juntrace bar)
    (assert (equal (sort (jtrace) #'string< :key #'symbol-name) '(foo)))
    (juntrace)
    (assert (endp (jtrace)))))


;;;; Single-writer

(defun test-nested-with-journaling ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (assert (eq (journal-state journal) :recording))
      (assert-error (journal-error "is not in state :NEW.")
        (with-journaling (:record journal)
          (journaled (foo)))))))

(defun test-concurrent-writers ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (with-open-journal (streamlet journal :direction :input))
      (assert-error (journal-error "Concurrent write access")
        (with-open-journal (streamlet journal :direction :io)))
      (assert-error (journal-error "Concurrent write access")
        (with-open-journal (streamlet journal :direction :output))))))

(defun test-nested-log-record ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (journaled (foo :log-record journal)))))

(defun test-log-record-before-journaled ()
  (let ((journal (funcall *make-journal*)))
    (journaled (foo :log-record journal))
    (assert-error (journal-error "Concurrent write access")
      (with-journaling (:record journal)))))

(defun test-concurrent-log-record ()
  (when bt:*supports-threads-p*
    (let ((journal (funcall *make-journal*))
          (phase nil))
      (bt:make-thread (lambda ()
                        (loop until (eq phase :in))
                        (unwind-protect
                             (assert-error (journal-error
                                            "Concurrent write access")
                               (journaled (foo :log-record journal)))
                          (setq phase :done))))
      (with-journaling (:record journal)
        (setq phase :in)
        (loop until (eq phase :done))))))

(defun test-nested-with-bundle ()
  (let ((bundle (make-in-memory-bundle)))
    (with-bundle (bundle)
      (assert-error (journal-error "Concurrent write access")
        (with-bundle (bundle))))))

(defun test-concurrent-with-bundle ()
  (when bt:*supports-threads-p*
    (let ((bundle (make-in-memory-bundle))
          (phase nil))
      (bt:make-thread (lambda ()
                        (loop until (eq phase :in))
                        (unwind-protect
                             (assert-error (journal-error
                                            "Concurrent write access")
                               (with-bundle (bundle)))
                          (setq phase :done))))
      (with-bundle (bundle)
        (setq phase :in)
        (loop until (eq phase :done))))))

(defun test-single-writer ()
  (test-nested-with-journaling)
  (test-concurrent-writers)
  (test-nested-log-record)
  (test-log-record-before-journaled)
  (test-concurrent-log-record)
  (test-nested-with-bundle)
  (test-concurrent-with-bundle))


;;;; Error handling

;;; Check that WITH-JOURNALING doesn't care about errors and such if
;;; the events are right.
(defun test-nlx-from-with-journaling ()
  (let ((journal-1 (funcall *make-journal*)))
    (catch 'foo
      (with-journaling (:record journal-1)
        (journaled (b1 :version 1)
          1)
        (throw 'foo nil)))
    (assert (equal (list-events journal-1)
                   '((:in b1 :version 1)
                     (:out b1 :version 1 :values (1)))))
    ;; Replay
    (let ((journal-2 (funcall *make-journal*)))
      (assert-error (some-error)
        (with-journaling (:replay journal-1 :record journal-2)
          (journaled (b1 :version 1)
            1)
          (error 'some-error)))
      (assert (eq (journal-state journal-2) :completed))
      (assert (equal (list-events journal-2)
                     '((:in b1 :version 1)
                       (:out b1 :version 1 :values (1))))))))

(defun test-error-two-deep ()
  (let ((journal-1 (funcall *make-journal*)))
    (assert-error (some-error)
      (with-journaling (:record journal-1)
        (journaled (b1 :version 1)
          (journaled (b2 :version 1)
            (error 'some-error))
          1)))
    (assert (equal (list-events journal-1)
                   '((:in b1 :version 1)
                     (:in b2 :version 1)
                     (:out b2
                      :error ("SOME-ERROR" "SOME-ERROR was signalled."))
                     (:out b1
                      :error ("SOME-ERROR" "SOME-ERROR was signalled.")))))))

(defun test-replay-failure-two-deep ()
  (let ((journal-1 (funcall *make-journal*)))
    ;; Record
    (with-journaling (:record journal-1)
      (journaled (b1 :version 1)
        (journaled (b2 :version 1)
          2)
        1))
    ;; Fail the replay in B2.
    (dolist (journal-2 (list (funcall *make-journal*) nil))
      (assert-error (replay-outcome-mismatch)
        (with-journaling (:replay journal-1 :record journal-2)
          (journaled (b1 :version 1)
            (journaled (b2 :version 1)
              7)
            1)))
      (when journal-2
        (assert (eq (journal-state journal-2) :failed)))
      ;; The last event recorded is what caused the replay error. B1's
      ;; out-event is not recorded.
      (when (typep journal-2 'in-memory-journal)
        (assert (equal (list-events journal-2)
                       '((:in b1 :version 1)
                         (:in b2 :version 1)
                         (:out b2 :version 1 :values (7))
                         (:out b1 :version 1 :error
                          ("REPLAY-OUTCOME-MISMATCH"
                           "The EXITs and OUTCOMEs of the new (:OUT JOURNAL-TEST::B2 :VERSION 1 :VALUES (7)) and the replay (:OUT JOURNAL-TEST::B2 :VERSION 1 :VALUES (2)) (at position 2) events are not equal.")))))))))

(defun test-replay-failure-two-deep-with-error ()
  (let ((journal-1 (funcall *make-journal*)))
    ;; Record
    (with-journaling (:record journal-1)
      (journaled (b1 :version 1)
        (journaled (b2 :version 1)
          2)
        1))
    ;; Fail the replay in B2, but this time by signalling an ERROR.
    (dolist (journal-2 (list (funcall *make-journal*) nil))
      (assert-error (replay-unexpected-outcome)
        (unwind-but-dont-receive some-error
          (with-journaling (:replay journal-1 :record journal-2)
            (journaled (b1 :version 1)
              (journaled (b2 :version 1)
                (error 'some-error))
              1))))
      (when journal-2
        (assert (eq (journal-state journal-2) :failed)))
      ;; The last event recorded is what caused the replay error. B1's
      ;; out-event is not recorded.
      (when (typep journal-2 'in-memory-journal)
        (assert (equal (list-events journal-2)
                       '((:in b1 :version 1)
                         (:in b2 :version 1)
                         (:out b2 :version 1
                          :error ("SOME-ERROR" "SOME-ERROR was signalled."))
                         (:out b1 :version 1 :error
                          ("REPLAY-UNEXPECTED-OUTCOME"
                           "The new event (:OUT JOURNAL-TEST::B2 :VERSION 1 :ERROR (\"SOME-ERROR\" \"SOME-ERROR was signalled.\")) has an unexpected outcome while the replay event (:OUT JOURNAL-TEST::B2 :VERSION 1 :VALUES (2)) (at position 2) has not.")))))))))

(defun test-values-fn-failure-recording ()
  (let* ((journal-1 (funcall *make-journal*))
         (sync (journal-sync journal-1)))
    (assert-error (journaling-failure "SOME-ERROR")
      (with-journaling (:record journal-1)
        (unwind-but-dont-receive some-error
          (journaled (b1 :version 1 :values (lambda (list)
                                              (declare (ignore list))
                                              (error 'some-error)))))))
    (assert (equal (list-events journal-1)
                   (if sync () '((:in b1 :version 1)))))
    (assert (eq (journal-state journal-1) :completed))
    ;; Replay is the same.
    (let ((journal-2 (funcall *make-journal*)))
      (assert-error (journaling-failure "SOME-ERROR")
        (with-journaling (:replay journal-1 :record journal-2)
          (unwind-but-dont-receive some-error
            (journaled (b1 :version 1 :values (lambda (list)
                                                (declare (ignore list))
                                                (error 'some-error)))))))
      (assert (eq (journal-state journal-2) :completed))
      (assert (equal (list-events journal-2)
                     (if sync () '((:in b1 :version 1))))))))

(defun test-values-fn-failure-replaying ()
  (let* ((journal-1 (funcall *make-journal*))
         (sync (journal-sync journal-1)))
    (with-journaling (:record journal-1)
      (journaled (b1 :version 1)
        1))
    (assert (equal (list-events journal-1)
                   '((:in b1 :version 1)
                     (:out b1 :version 1 :values (1)))))
    (assert (eq (journal-state journal-1) :completed))
    ;; Replay
    (let ((journal-2 (funcall *make-journal*)))
      (assert-error (journaling-failure "SOME-ERROR")
        (with-journaling (:replay journal-1 :record journal-2)
          (unwind-protect
               (unwind-but-dont-receive some-error
                 (journaled (b1 :version 1 :values (lambda (list)
                                                     (declare (ignore list))
                                                     (error 'some-error)))
                   1))
            (assert (eq (journal-state journal-2) :failed))
            ;; It is an error to use JOURNALED in state :FAILED.
            (assert-error (journaling-failure "Resignalling previous")
              (journaled (b2))))))
      (assert (eq (journal-state journal-2) :failed))
      (assert (equal (list-events journal-2)
                     (if sync () '((:in b1 :version 1))))))))

(defun test-journaling-failure ()
  (let* ((journal-1 (funcall *make-journal*))
         (sync (journal-sync journal-1)))
    (assert-error (journaling-failure "SOME-ERROR")
      (with-journaling (:record journal-1)
        (unwind-but-dont-receive some-error
          (journaled (b1 :version 1 :values (lambda (list)
                                              (declare (ignore list))
                                              (error 'some-error)))))
        (journaled (b2 :version 1))))
    (assert (equal (list-events journal-1)
                   (if sync
                       ()
                       '((:in b1 :version 1)))))
    (assert (eq (journal-state journal-1) :completed))

    (dolist (make-journal (list *make-journal* (constantly nil)))

      ;; Error in WRITE-EVENT when :REPLAYING in replay
      (let ((journal-2 (funcall make-journal)))
        (assert-error (journaling-failure "SOME-ERROR")
          (with-journaling (:record journal-2 :replay journal-1)
            (unwind-protect
                 (unwind-but-dont-receive some-error
                   (let ((jrn::*next-write-event-fn* (lambda (event)
                                                       (declare (ignore event))
                                                       (error 'some-error))))
                     (journaled (b1 :version 1))))
              ;; Check that WITH-JOURNALING-FAILURE-ON-NLX marked the
              ;; record journal as failed, and that it's not
              ;; writeable.
              (when journal-2
                (assert (eq (journal-state journal-2)
                            (if sync :completed :failed))))
              (assert-error (journaling-failure "Resignal")
                (journaled (b2 :version 1))))))
        (when journal-2
          (assert (equal (list-events journal-2) ()))
          (assert (eq (journal-state journal-2)
                      (if sync :completed :failed)))))

      ;; THROW in WRITE-EVENT when :REPLAYING in replay
      (let ((journal-2 (funcall make-journal)))
        (catch 'foo
          (assert-error (journaling-failure
                         "JOURNALED failed with an unidentified")
            (with-journaling (:record journal-2 :replay journal-1)
              (setq jrn::*next-write-event-fn* (lambda (event)
                                                 (declare (ignore event))
                                                 (throw 'foo nil)))
              (unwind-protect
                   (unwind-but-dont-receive some-error
                     (journaled (b1 :version 1)))
                ;; Check that WITH-JOURNALING-FAILURE-ON-NLX marked
                ;; the record journal as failed, and that it's not
                ;; writeable.
                (when journal-2
                  (assert (eq (journal-state journal-2)
                              (if sync :completed :failed))))
                (assert-error (journaling-failure "Resignal")
                  (journaled (b2 :version 1)))))))
        (when journal-2
          (assert (equal (list-events journal-2) ()))
          (assert (eq (journal-state journal-2)
                      (if sync :completed :failed))))))

    (dolist (commit (if sync '(nil t) '(nil)))
      (dolist (make-journal (if commit
                                (list *make-journal*)
                                (list *make-journal* (constantly nil))))

        ;; Error in WRITE-EVENT when :RECORDING in replay
        (let ((journal-2 (funcall make-journal)))
          (assert-error (journaling-failure "SOME-ERROR")
            (with-journaling (:record journal-2 :replay journal-1)
              (unwind-protect
                   (progn
                     (journaled (b1 :version 1))
                     (when commit
                       (sync-journal))
                     (setq jrn::*next-write-event-fn* (lambda (event)
                                                        (declare (ignore event))
                                                        (error 'some-error)))
                     (unwind-but-dont-receive some-error
                       (journaled (b1 :version 1))))
                (when journal-2
                  (assert (eq (journal-state journal-2) :completed)))
                (assert-error (journaling-failure "Resignal")
                  (journaled (b2 :version 1))))))
          (when journal-2
            (assert (equal (list-events journal-2)
                           (if (and sync (not commit))
                               ()
                               '((:in b1 :version 1)
                                 (:out b1 :version 1 :values (nil))))))
            (assert (eq (journal-state journal-2) :completed))))

        ;; THROW in WRITE-EVENT when :RECORDING in replay
        (let ((journal-2 (funcall make-journal)))
          (catch 'foo
            (assert-error (journaling-failure
                           "JOURNALED failed with an unidentified")
              (with-journaling (:record journal-2 :replay journal-1)
                (unwind-protect
                     (progn
                       (journaled (b1 :version 1))
                       (when commit
                         (sync-journal))
                       (setq jrn::*next-write-event-fn*
                             (lambda (event)
                               (declare (ignore event))
                               (throw 'foo nil)))
                       (unwind-but-dont-receive some-error
                         (journaled (b1 :version 1))))
                  (when journal-2
                    (assert (eq (journal-state journal-2) :completed)))
                  (assert-error (journaling-failure "Resignal")
                    (journaled (b2 :version 1)))))))
          (when journal-2
            (assert (equal (list-events journal-2)
                           (if (and sync (not commit))
                               ()
                               '((:in b1 :version 1)
                                 (:out b1 :version 1 :values (nil))))))
            (assert (eq (journal-state journal-2) :completed))))))))

(defun test-error-handling ()
  (test-nlx-from-with-journaling)
  (test-error-two-deep)
  (test-replay-failure-two-deep)
  (test-replay-failure-two-deep-with-error)
  (test-values-fn-failure-recording)
  (test-values-fn-failure-replaying)
  (test-journaling-failure))

(defun test-in-memory-journal-error-handling ()
  (test-error-handling))

(defun test-file-journal-error-handling ()
  (call-with-file-journal-settings #'test-error-handling))


;;;; Test DEFINE-FILE-BUNDLE-TEST (see @TESTING)

(defparameter *db* (make-hash-table))

(defun set-key (key value)
  (journaled ("set-key" :version :infinity :args `(,key ,value))
    (setf (gethash key *db*) value)
    nil))

(defun get-key (key)
  (journaled ("get-key" :version :infinity :args `(,key))
    (gethash key *db*)))

(defun ask-username ()
  (journaled ("ask-username" :version :infinity)
    (format t "Please type your username: ")
    (read-line)))

(defun maybe-win-the-grand-prize ()
  (journaled ("maybe-win-the-grand-prize" :version 1)
    (when (= 1000000 (hash-table-count *db*))
      (format t "You are the lucky one!"))))

(defun register-user (username)
  (unless (get-key username)
    (set-key username `(:user-object :username ,username))
    (maybe-win-the-grand-prize)))

(defvar *n-registrations* 2)

(define-file-bundle-test (test-user-registration
                          :directory (asdf:system-relative-pathname
                                      :journal "test/registration/"))
  (let* ((*db* (make-hash-table))
         (username (ask-username)))
    (loop repeat *n-registrations* do
      (register-user username)
      (assert (get-key username)))))

(defun test-define-file-bundle-test ()
  (test-user-registration)
  (assert-error (simple-error "not equivalent")
    (let ((*n-registrations* 3))
      (test-user-registration))))


(defun test-make-file-journal ()
  (call-with-file-journal-settings
   (lambda ()
     ;; :NEW
     (let ((journal (funcall *make-journal*)))
       (assert (eq (journal-state journal) :new))
       (dolist (sync '(nil t))
         (if (eq sync (journal-sync journal))
             (assert (eq (make-file-journal (pathname-of journal) :sync sync)
                         journal))
             (assert-error (journal-error "Incompatible options")
               (make-file-journal (pathname-of journal) :sync sync)))))
     ;; :COMPLETED
     (let ((journal (funcall *make-journal*)))
       (with-journaling (:record journal))
       (assert (eq (journal-state journal) :completed))
       (dolist (sync '(nil t))
         (if (eq sync (journal-sync journal))
             (assert (eq (make-file-journal (pathname-of journal) :sync sync)
                         journal))
             (assert-error (journal-error "Incompatible options")
               (make-file-journal (pathname-of journal) :sync sync)))))
     ;; :COMPLETED, deleted
     (let ((journal (funcall *make-journal*)))
       (with-journaling (:record journal))
       (assert (eq (journal-state journal) :completed))
       (delete-file (pathname-of journal))
       (assert (not (eq (make-file-journal (pathname-of journal))
                        journal)))
       (assert (eq (slot-value journal 'jrn::n-writers) :invalidated))
       (assert-error (journal-error "unlinked")
         (with-open-journal (streamlet journal :direction :output)))))))

(defun test-make-file-bundle ()
  (call-with-file-bundle
   (lambda (bundle)
     (assert (eq (make-file-bundle (directory-of bundle)
                                   :max-n-failed (max-n-failed bundle)
                                   :max-n-completed (max-n-completed bundle)
                                   :sync (slot-value bundle 'jrn::sync))
                 bundle))
     (assert-error (journal-error "Incompatible options")
       (make-file-bundle (directory-of bundle)
                         :max-n-failed (max-n-failed bundle)
                         :max-n-completed (max-n-completed bundle)
                         :sync (not (slot-value bundle 'jrn::sync)))))))


(defun test ()
  (test-events-to-frames)
  (test-in-memory-journal)
  (test-file-journal)
  (test-io-direction)
  (test-in-memory-bundle)
  (test-file-bundle)
  (test-file-sync)
  (test-file-sync-garbage)
  (test-sync-t)
  (test-jtrace)
  (test-single-writer)
  (test-in-memory-journal-error-handling)
  (test-file-journal-error-handling)
  (test-define-file-bundle-test)
  (test-make-file-journal)
  (test-make-file-bundle))

#+nil
(test)
