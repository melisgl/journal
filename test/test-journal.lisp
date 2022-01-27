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

(defmacro unwind-but-dont-receive (condition-type &body body)
  (alexandria:with-gensyms (c)
    `(handler-case (progn ,@body)
       (,condition-type (,c)
         (is nil :msg (list "Oops. Received condition ~S." ,c))))))

(defun check-file-journal-state (journal state)
  (when (typep journal 'file-journal)
    (is (probe-file (pathname-of journal)))
    ;; KLUDGE: We want a new instance pointing to the same directory.
    (let* ((jrn::*truename-to-file-journal* (make-hash-table :test #'equal))
           (new-journal (make-file-journal (pathname-of journal))))
      (is (eq (journal-state new-journal) state)))))

(defparameter *make-journal* 'make-in-memory-journal)


(deftest test-events-to-frames ()
  (is (equal (events-to-frames '((:in foo :args (1 2))
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

(deftest test-log-values-record ()
  (let ((journal (funcall *make-journal*)))
    (is (eq (journal-state journal) :new))
    (with-journaling (:record journal)
      (is (eq (journal-state journal) :recording))
      (is (= 42 (journaled (foo) 42)))
      (is (eq (journal-state journal) :recording))
      (is (equal (list-events journal)
                     '((:in foo)
                       (:out foo :values (42))))))
    (is (eq (journal-state journal) :completed))
    (check-file-journal-state journal :completed)))

(deftest test-log-condition-record ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (is (eq (journal-state journal) :recording))
      (signals (some-error)
        (journaled ("foo" :condition (constantly 'eh))
          (error 'some-error)
          42))
      (is (eq (journal-state journal) :recording))
      (is (equal (list-events journal)
                 '((:in "foo")
                   (:out "foo" :condition eh)))))
    (is (eq (journal-state journal) :completed))
    (check-file-journal-state journal :completed)))

(deftest test-log-error-record ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (is (eq (journal-state journal) :recording))
      (signals (some-error)
        (journaled (foo)
          (error 'some-error)
          42))
      (is (eq (journal-state journal) :recording))
      (is (equivalent-events-p (list-events journal)
                                   '((:in foo)
                                     (:out foo :error this-is-not-compared)))))
    (is (eq (journal-state journal) :completed))
    (check-file-journal-state journal :completed)))

(deftest test-log-nlx-record ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (is (eq (journal-state journal) :recording))
      (is (= 42 (catch 'foo
                      (journaled (foo)
                        (throw 'foo 42)))))
      (is (eq (journal-state journal) :recording))
      (is (equal (list-events)
                     '((:in foo)
                       (:out foo :nlx nil)))))
    (is (eq (journal-state journal) :completed))
    (check-file-journal-state journal :completed)))


;;;; Test LOG-RECORD

(deftest test-log-record-without-with-journaling ()
  (let ((journal (funcall *make-journal*)))
    (is (eq (journal-state journal) :new))
    (is (= 42 (framed (foo :log-record journal) 42)))
    (checked (bar) 42)
    (replayed (bar) 42)
    (is (equal (list-events journal)
                   '((:in foo)
                     (:out foo :values (42)))))
    (is (eq (journal-state journal) :new))
    ;; TEST-LOG-RECORDING-TO-FAILED-JOURNAL is related.
    (check-file-journal-state journal :failed)))

(deftest test-log-record-in-with-journaling ()
  (let ((log-journal (funcall *make-journal*))
        (journal (funcall *make-journal*)))
    (is (eq (journal-state journal) :new))
    (with-journaling (:record journal)
      (is (eq (journal-state journal) :recording))
      (framed (foo :log-record log-journal) 42)
      (checked (bar) 42)
      (replayed (bar) 42))
    (is (eq (journal-state journal) :completed))
    (is (equal (list-events journal)
                   '((:in bar :version 1)
                     (:out bar :version 1 :values (42))
                     (:in bar :version :infinity)
                     (:out bar :version :infinity :values (42)))))
    (is (equal (list-events log-journal)
                   '((:in foo)
                     (:out foo :values (42)))))))

(deftest test-log-record-to-record-journal ()
  (let ((journal (funcall *make-journal*)))
    (is (eq (journal-state journal) :new))
    (with-journaling (:record journal)
      (is (eq (journal-state journal) :recording))
      (framed (foo :log-record journal) 42)
      (checked (bar) 7))
    (is (eq (journal-state journal) :completed))
    (is (equal (list-events journal)
                   '((:in foo)
                     (:out foo :values (42))
                     (:in bar :version 1)
                     (:out bar :version 1 :values (7)))))))


;;;; Test LOGGED

(deftest test-logged-without-with-journaling ()
  (let ((journal (funcall *make-journal*)))
    (is (eq (journal-state journal) :new))
    (is (null (logged (journal) "42")))
    (is (equal (list-events journal)
                   '((:leaf "42"))))
    (is (eq (journal-state journal) :new))
    (check-file-journal-state journal :failed)))

(deftest test-logged-in-with-journaling ()
  (let ((log-journal (funcall *make-journal*))
        (journal (funcall *make-journal*)))
    (is (eq (journal-state journal) :new))
    (with-journaling (:record journal)
      (is (eq (journal-state journal) :recording))
      (logged () "7")
      (logged (log-journal) "42"))
    (is (eq (journal-state journal) :completed))
    (is (equal (list-events journal)
                   '((:leaf "7"))))
    (is (equal (list-events log-journal)
                   '((:leaf "42"))))))

(deftest test-logged-to-record-journal ()
  (let ((journal (funcall *make-journal*)))
    (is (eq (journal-state journal) :new))
    (with-journaling (:record journal)
      (is (eq (journal-state journal) :recording))
      (logged (journal) "~S" 42))
    (is (eq (journal-state journal) :completed))
    (is (equal (list-events journal)
                   '((:leaf "42"))))))


;;;; Test LOG-DECORATOR

(deftest test-log-decorator ()
  (let ((*time* t)
        (journal (funcall *make-journal*)))
    (declare (special *time*))
    (is (eq (journal-state journal) :new))
    (setf (journal-log-decorator journal)
          (make-log-decorator :thread t :time '*time*))
    (framed (foo :log-record journal) 42)
    (destructuring-bind (e1 e2) (list-events journal)
      (is (getf e1 :time))
      (is (getf e1 :thread))
      (is (getf e2 :time))
      (is (getf e2 :thread)))
    (let ((*time* nil))
      (declare (special *time*))
      (logged (journal) "42")
      (is (= (length (list-events journal)) 3))
      (destructuring-bind (e3) (subseq (list-events journal) 2)
        (is (not (getf e3 :time)))
        (is (getf e3 :thread))))))


;;;;; Test replay corner cases

(deftest test-replay-end-of-journal-p-at-in-event ()
  (let ((journal-1 (make-in-memory-journal :state :completed))
        (journal-2 (funcall *make-journal*)))
    (signals (end-of-journal)
      (with-journaling (:replay journal-1 :record journal-2
                                :replay-eoj-error-p t)
        (is (not (journal-divergent-p journal-2)))
        (is (eq (journal-state journal-2) :recording))
        (checked (foo)
          (is (journal-divergent-p journal-2))
          42)))
    (is (eq (journal-state journal-2) :completed))
    (check-file-journal-state journal-2 :completed)))

(deftest test-replay-end-of-journal-p-at-out-event ()
  (let ((journal-1 (make-in-memory-journal))
        (journal-2 (funcall *make-journal*)))
    ;; Let's create a truncated journal by hand, where the out-event
    ;; is missing entirely.
    (write-event '(:in foo :version 1) journal-1)
    (setf (jrn::%state journal-1) :completed)
    (signals (end-of-journal)
      (with-journaling (:replay journal-1 :record journal-2
                                :replay-eoj-error-p t)
        (is (eq (journal-state journal-2) :replaying))
        (checked (foo)
          (is (not (journal-divergent-p journal-2)))
          42)
        (is (journal-divergent-p journal-2))))
    (is (eq (journal-state journal-2) :completed))
    (check-file-journal-state journal-2 :completed)))

(deftest test-replay-incomplete ()
  (let ((journal-1 (funcall *make-journal*))
        (journal-2 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (checked (foo) 42))
    (signals (replay-incomplete)
      (with-journaling (:record journal-2 :replay journal-1)))
    (is (not (journal-divergent-p journal-2)))
    (is (eq (journal-state journal-2) :failed))
    (check-file-journal-state journal-2 :failed)))

(deftest test-replay-incomplete-without-record ()
  (let ((journal-1 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (checked (foo) 42))
    (signals (replay-incomplete)
      (with-journaling (:replay journal-1)))))

;;; Check that REPLAY-INCOMPLETE is not signalled if
;;; WITH-JOURNALING did not finish normally.
(deftest test-replay-incomplete-not-signalled-case ()
  (let ((journal-1 (funcall *make-journal*))
        (journal-2 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (checked (foo) 42))
    (signals (simple-error :pred "eh")
      (with-journaling (:record journal-2 :replay journal-1)
        (error "eh")))
    (is (not (journal-divergent-p journal-2)))))

(deftest test-replay-incomplete-not-signalled-case-without-record ()
  (let ((journal-1 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (checked (foo) 42))
    (signals (simple-error :pred "eh")
      (with-journaling (:replay journal-1)
        (error "eh")))))

(deftest test-recording-to-completed-journal ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (= (framed (foo) 42)))
    (is (eq (journal-state journal) :completed))
    (signals (journal-error :pred "is not in state :NEW")
      (with-journaling (:record journal)))))

(deftest test-log-recording-to-completed-journal ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (= (framed (foo) 42)))
    (is (eq (journal-state journal) :completed))
    (signals (journal-error :pred "Refusing to log to journal")
      (framed (foo :log-record journal)))))

(deftest test-recording-to-failed-journal ()
  (let ((journal (funcall *make-journal*)))
    (setf (jrn::%state journal) :failed)
    (signals (journal-error :pred "is not in state :NEW")
      (with-journaling (:record journal)))))

(deftest test-recording-to-mismatched-journal ()
  (let ((journal (funcall *make-journal*)))
    (setf (jrn::%state journal) :mismatched)
    (signals (journal-error :pred "is not in state :NEW")
      (with-journaling (:record journal)))))

(deftest test-log-recording-to-failed-journal ()
  (let ((journal (funcall *make-journal*)))
    (setf (jrn::%state journal) :failed)
    ;; Also see TEST-LOG-RECORD-WITHOUT-WITH-JOURNALING.
    (framed (foo :log-record journal))))

(deftest test-skip-log-with-no-record ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (framed (j1)))
    (with-journaling (:replay journal :replay-eoj-error-p t))))

(deftest test-recording-after-replay-failure ()
  (let ((journal-1 (funcall *make-journal*))
        (journal-2 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (checked (j1))
      (checked (j1)))
    (with-journaling (:replay journal-1 :record journal-2)
      (signals (replay-name-mismatch)
        (checked (j2)))
      (is (eq (journal-state journal-2) :mismatched))
      (let ((read-position (read-position jrn::*replay-streamlet*)))
        (checked (j3))
        ;; We switch to the :INSERT replay strategy in :MISMATCHED.
        (is (= read-position (read-position jrn::*replay-streamlet*))))
      (is (equal (list-events)
                     '((:in j2 :version 1)
                       (:in j3)
                       (:out j3 :values (nil))))))))


;;;; Recording and replay of versioned events

(deftest test-values-replay ()
  (let ((journal-1 (funcall *make-journal*)))
    ;; Record
    (with-journaling (:record journal-1)
      (journaled (foo :version 1)
        (is (equal (list-events)
                       '((:in foo :version 1))))
        42))
    ;; Check what was recorded.
    (is (equal (list-events journal-1)
                   '((:in foo :version 1)
                     (:out foo :version 1 :values (42)))))
    (is (journal-divergent-p journal-1))

    ;; Replay with the same :VALUES outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (with-journaling (:replay journal-1 :record journal-2)
        (is (eq (journal-state journal-2) :replaying))
        (journaled (foo :version 1)
          (is (eq (journal-state journal-2) :replaying))
          42)
        (is (eq (journal-state journal-2) :recording)))
      (is (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (is (not (journal-divergent-p journal-2))))

    ;; Replay with different :VALUES outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (signals (replay-outcome-mismatch)
        (with-journaling (:replay journal-1 :record journal-2)
          (is (eq (journal-state journal-2) :replaying))
          (journaled (foo :version 1) 7)
          (is nil)))
      (is (equal (list-events journal-2)
                     '((:in foo :version 1)
                       (:out foo :version 1 :values (7)))))
      (is (eq (journal-state journal-2) :failed))
      (check-file-journal-state journal-2 :failed)
      (is (journal-divergent-p journal-2)))

    ;; Replay with :CONDITION outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (unwind-but-dont-receive some-error
        (signals (replay-outcome-mismatch)
          (with-journaling (:replay journal-1 :record journal-2)
            (journaled (foo :version 1 :condition (constantly 'eh))
              (is (eq (journal-state journal-2) :replaying))
              (error 'some-error)))))
      (is (eq (journal-state journal-2) :failed))
      (check-file-journal-state journal-2 :failed)
      (is (journal-divergent-p journal-2)))

    ;; Replay with :ERROR outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (signals (replay-unexpected-outcome)
        (with-journaling (:replay journal-1 :record journal-2)
          (unwind-but-dont-receive some-error
            (journaled (foo :version 1)
              (is (eq (journal-state journal-2) :replaying))
              (error 'some-error)))))
      (is (eq (journal-state journal-2) :failed))
      (check-file-journal-state journal-2 :failed)
      (is (journal-divergent-p journal-2)))

    ;; Replay with :NLX outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (signals (replay-unexpected-outcome)
        (catch 'foo
          (with-journaling (:replay journal-1 :record journal-2)
            (journaled (foo :version 1)
              (is (eq (journal-state journal-2) :replaying))
              (throw 'foo 7)))))
      (is (eq (journal-state journal-2) :failed))
      (check-file-journal-state journal-2 :failed)
      (is (journal-divergent-p journal-2)))))

(deftest test-condition-replay ()
  (let ((journal-1 (funcall *make-journal*)))
    ;; Record
    (with-journaling (:record journal-1)
      (signals (some-error)
        (journaled (foo :version 1 :condition (constantly 'eh))
          (error 'some-error)
          42))
      ;; Check what was recorded.
      (is (equal (list-events journal-1)
                     '((:in foo :version 1)
                       (:out foo :version 1 :condition eh)))))
    (is (journal-divergent-p journal-1))

    ;; Replay with any :VALUES outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (signals (replay-outcome-mismatch)
        (with-journaling (:replay journal-1 :record journal-2)
          (journaled (foo :version 1)
            (is (eq (journal-state journal-2) :replaying))
            42)
          (is nil)))
      (is (eq (journal-state journal-2) :failed))
      (check-file-journal-state journal-2 :failed)
      (is (journal-divergent-p journal-2)))

    ;; Replay with the same :CONDITION outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (signals (some-error)
        (with-journaling (:replay journal-1 :record journal-2)
          (unwind-protect
               (journaled (foo :version 1 :condition (constantly 'eh))
                 (is (eq (journal-state journal-2) :replaying))
                 (error 'some-error)
                 42)
            (is (eq (journal-state journal-2) :recording)))))
      (is (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (is (not (journal-divergent-p journal-2))))

    ;; Replay with a different :CONDITION outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (unwind-but-dont-receive some-error
        (signals (replay-outcome-mismatch)
          (with-journaling (:replay journal-1 :record journal-2)
            (journaled (foo :version 1 :condition (constantly 'bah))
              (is (eq (journal-state journal-2) :replaying))
              (error 'some-error)
              42))))
      (is (eq (journal-state journal-2) :failed))
      (check-file-journal-state journal-2 :failed)
      (is (journal-divergent-p journal-2)))

    ;; Replay with :ERROR outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (unwind-but-dont-receive some-error
        (signals (replay-unexpected-outcome)
          (with-journaling (:replay journal-1 :record journal-2)
            (journaled (foo :version 1)
              (is (eq (journal-state journal-2) :replaying))
              (error 'some-error)))))
      (is (eq (journal-state journal-2) :failed))
      (check-file-journal-state journal-2 :failed)
      (is (journal-divergent-p journal-2)))

    ;; Replay with :NLX outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (signals (replay-unexpected-outcome)
        (catch 'foo
          (with-journaling (:replay journal-1 :record journal-2)
            (journaled (foo :version 1)
              (is (eq (journal-state journal-2) :replaying))
              (throw 'foo 7)))))
      (is (eq (journal-state journal-2) :failed))
      (check-file-journal-state journal-2 :failed)
      (is (journal-divergent-p journal-2)))))

(deftest test-error-replay ()
  (let ((journal-1 (funcall *make-journal*)))
    ;; Record
    (with-journaling (:record journal-1)
      (signals (some-error)
        (journaled (foo :version 1)
          (error 'some-error)
          42))
      ;; Check what was recorded.
      (is (equivalent-events-p
               (list-events journal-1)
               '((:in foo :version 1)
                 (:out foo :version nil :error this-is-not-compared)))))
    (is (journal-divergent-p journal-1))

    ;; Replay with any :VALUES outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (= 42 (with-journaling (:replay journal-1 :record journal-2)
              (is (eq (journal-state journal-2) :replaying))
              (unwind-protect
                   (journaled (foo :version 1) 42)
                (is (eq (journal-state journal-2) :recording)))))
      (is (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (is (journal-divergent-p journal-2)))

    ;; Replay with any :CONDITION outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (signals (some-error)
        (with-journaling (:replay journal-1 :record journal-2)
          (unwind-protect
               (journaled (foo :version 1 :condition (constantly 'eh))
                 (is (eq (journal-state journal-2) :recording))
                 (error 'some-error)
                 42)
            (is (eq (journal-state journal-2) :recording)))))
      (is (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (is (journal-divergent-p journal-2)))

    ;; Replay with the same :ERROR outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (signals (some-error)
        (with-journaling (:replay journal-1 :record journal-2)
          (unwind-protect
               (journaled (foo :version 1)
                 (is (eq (journal-state journal-2) :recording))
                 (error 'some-error))
            (is (eq (journal-state journal-2) :logging)))))
      (is (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (is (not (journal-divergent-p journal-2))))

    ;; Replay with a different :ERROR outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (signals (another-error)
        (with-journaling (:replay journal-1 :record journal-2)
          (unwind-protect
               (journaled (foo :version 1)
                 (is (eq (journal-state journal-2) :recording))
                 (error 'another-error))
            (is (eq (journal-state journal-2) :logging)))))
      (is (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (is (not (journal-divergent-p journal-2))))

    ;; Replay with :NLX outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (= 7 (catch 'foo
             (with-journaling (:replay journal-1 :record journal-2)
               (unwind-protect
                    (journaled (foo :version 1)
                      (is (eq (journal-state journal-2) :recording))
                      (throw 'foo 7))
                 (is (eq (journal-state journal-2) :logging))))))
      (is (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (is (not (journal-divergent-p journal-2))))))

(deftest test-nlx-replay ()
  (let ((journal-1 (funcall *make-journal*)))
    ;; Record
    (with-journaling (:record journal-1)
      (is (= 42 (catch 'foo
                      (journaled (foo :version 1)
                        (throw 'foo 42))))))
    ;; Check what was recorded.
    (is (equal (list-events journal-1)
                   '((:in foo :version 1)
                     (:out foo :nlx nil))))
    (is (journal-divergent-p journal-1))

    ;; Replay with any :VALUES outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (= 42 (with-journaling (:replay journal-1 :record journal-2)
              (unwind-protect
                   (journaled (foo :version 1)
                     (is (eq (journal-state journal-2) :recording))
                     42)
                (is (eq (journal-state journal-2) :recording)))))
      (is (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (is (journal-divergent-p journal-2)))

    ;; Replay with any :CONDITION outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (signals (some-error)
        (with-journaling (:replay journal-1 :record journal-2)
          (unwind-protect
               (journaled (foo :version 1 :condition (constantly 'eh))
                 (is (eq (journal-state journal-2) :recording))
                 (error 'some-error)
                 42)
            (is (eq (journal-state journal-2) :recording)))))
      (is (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (is (journal-divergent-p journal-2)))

    ;; Replay with any :ERROR outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (signals (some-error)
        (with-journaling (:replay journal-1 :record journal-2)
          (unwind-protect
               (journaled (foo :version 1)
                 (is (eq (journal-state journal-2) :recording))
                 (error 'some-error))
            (is (eq (journal-state journal-2) :logging)))))
      (is (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (is (not (journal-divergent-p journal-2))))

    ;; Replay with :NLX outcome.
    (let ((journal-2 (funcall *make-journal*)))
      (= 7 (catch 'foo
             (with-journaling (:replay journal-1 :record journal-2)
               (unwind-protect
                    (journaled (foo :version 1)
                      (is (eq (journal-state journal-2) :recording))
                      (throw 'foo 7))
                 (is (eq (journal-state journal-2) :logging))))))
      (is (eq (journal-state journal-2) :completed))
      (check-file-journal-state journal-2 :completed)
      (is (not (journal-divergent-p journal-2))))))

(deftest test-various-replay-cases ()
  (let ((journal-1 (funcall *make-journal*)))
    ;; Record
    (with-journaling (:record journal-1)
      (journaled (foo :version 2)
        (is (equal (list-events)
                       '((:in foo :version 2))))
        42))
    ;; Check what was recorded.
    (is (equal (list-events journal-1)
                   '((:in foo :version 2)
                     (:out foo :version 2 :values (42)))))
    (is (journal-divergent-p journal-1))

    (dolist (make-journal (list *make-journal* (constantly nil)))
      ;; Same version with mismatched name
      (let ((journal-2 (funcall make-journal)))
        (signals (replay-name-mismatch)
          (with-journaling (:replay journal-1 :record journal-2)
            (unwind-protect
                 (journaled (bar :version 2)
                   (is nil)
                   42)
              (when journal-2
                (is (eq (journal-state journal-2) :mismatched))))))
        (check-file-journal-state journal-2 :failed)
        (when journal-2
          (is (journal-divergent-p journal-2))))

      ;; Same name and version with mismatched args
      (let ((journal-2 (funcall make-journal)))
        (signals (replay-args-mismatch)
          (with-journaling (:replay journal-1 :record journal-2)
            (unwind-protect
                 (journaled (foo :version 2 :args '(1 2))
                   (is nil))
              (when journal-2
                (is (eq (journal-state journal-2) :mismatched))))))
        (check-file-journal-state journal-2 :failed)
        (when journal-2
          (is (journal-divergent-p journal-2))))

      ;; Higher version. Name is still checked.
      (let ((journal-2 (funcall make-journal)))
        (signals (replay-name-mismatch)
          (with-journaling (:replay journal-1 :record journal-2)
            (unwind-protect
                 (journaled (bar :version 3)
                   (is nil))
              (when journal-2
                (is (eq (journal-state journal-2) :mismatched))))))
        (check-file-journal-state journal-2 :failed)
        (when journal-2
          (is (journal-divergent-p journal-2))))

      ;; Higher version. Args, outcome not checked.
      (let ((journal-2 (funcall make-journal)))
        (is
         (= 7 (with-journaling (:replay journal-1 :record journal-2)
                (unwind-protect
                     (journaled (foo :version 3)
                       (when journal-2
                         (is (eq (journal-state journal-2) :replaying)))
                       7)
                  (when journal-2
                    (is (eq (journal-state journal-2) :recording)))))))
        (when journal-2
          (is (eq (journal-state journal-2) :completed)))
        (check-file-journal-state journal-2 :completed)
        (when journal-2
          (is (journal-divergent-p journal-2))))

      ;; Upgrade to :INFINITY. Args, outcome not checked.
      (let ((journal-2 (funcall make-journal)))
        (is
         (= 42 (with-journaling (:replay journal-1 :record journal-2)
                 (unwind-protect
                      (journaled (foo :version :infinity)
                        (when journal-2
                          (is (eq (journal-state journal-2) :replaying)))
                        (is nil))
                   (when journal-2
                     (is (eq (journal-state journal-2) :recording)))))))
        (when journal-2
          (is (eq (journal-state journal-2) :completed)))
        (check-file-journal-state journal-2 :completed)
        (when journal-2
          (is (journal-divergent-p journal-2))))

      ;; Lower version.
      (let ((journal-2 (funcall make-journal)))
        (with-journaling (:replay journal-1 :record journal-2)
          (signals (replay-version-downgrade)
            (journaled (foo :version 1)))
          (when journal-2
            (is (eq (journal-state journal-2) :mismatched)))
          (check-file-journal-state journal-2 :failed))
        (check-file-journal-state journal-2 :failed)
        (when journal-2
          (is (eq (journal-state journal-2) :failed))
          (is (journal-divergent-p journal-2))))

      ;; Log replay event. Name, args, outcome not checked.
      (let ((journal-2 (funcall make-journal)))
        (signals (replay-incomplete)
          (with-journaling (:replay journal-1 :record journal-2)
            (unwind-protect
                 (journaled (bar :version nil :args '(1 2))
                   (when journal-2
                     (is (eq (journal-state journal-2) :replaying)))
                   7)
              ;; We haven't replayed the FOO frame.
              (when journal-2
                (is (eq (journal-state journal-2) :replaying))))))
        (when journal-2
          (is (eq (journal-state journal-2) :failed)))
        (check-file-journal-state journal-2 :failed)
        (when journal-2
          (is (not (journal-divergent-p journal-2)))))

      ;; Extra log replay event.
      (let ((journal-2 (funcall make-journal)))
        (with-journaling (:replay journal-1 :record journal-2)
          (journaled (log)
            (journaled (foo :version 2)
              42)))
        (when journal-2
          (is (eq (journal-state journal-2) :completed)))
        (check-file-journal-state journal-2 :completed)
        (when journal-2
          (is (not (journal-divergent-p journal-2))))))))


;;;; Test recording and replay of external events

(deftest test-external-in-log ()
  (let ((n-foo-evals 0))
    (labels ((foo (x y)
               (journaled (foo :version :infinity :args `(,x ,y))
                 (incf n-foo-evals)
                 (+ x y)))
             (bar (x y z)
               (journaled (bar :args `(,x ,y ,z))
                 (values (* z (foo x y)) :second))))
      (is (equal '(9 :second) (multiple-value-list (bar 1 2 3))))
      (is (= 1 n-foo-evals))
      (let ((journal-1 (funcall *make-journal*)))
        (with-journaling (:record journal-1)
          (is (equal (multiple-value-list (bar 1 2 3))
                         '(9 :second)))
          (is (equal (list-events)
                         '((:in bar :args (1 2 3))
                           (:in foo :version :infinity :args (1 2))
                           (:out foo :version :infinity :values (3))
                           (:out bar :values (9 :second))))))
        (is (= 2 n-foo-evals))
        (let ((journal-2 (funcall *make-journal*)))
          (with-journaling (:replay journal-1 :record journal-2)
            (is (equal (multiple-value-list (bar 1 2 3))
                           '(9 :second)))
            (is (equal (list-events journal-1)
                           (list-events journal-2))))
          (is (= 2 n-foo-evals)))))))

(deftest test-log-in-external ()
  (labels ((foo (x y)
             (journaled (l1 :args `(,x ,y))
               (journaled (v :version 1)
                 (journaled (l2)
                   (+ x y)))))
           (bar (x y z)
             (journaled (bar :version :infinity :args `(,x ,y ,z))
               (values (* z (foo x y)) :second))))
    (is (equal '(9 :second) (multiple-value-list (bar 1 2 3))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1 :replay-eoj-error-p t)
        (is (equal (multiple-value-list (bar 1 2 3))
                       '(9 :second)))
        (is (equal (list-events)
                       '((:in bar :version :infinity :args (1 2 3))
                         (:in l1 :args (1 2))
                         (:in v :version 1)
                         (:in l2)
                         (:out l2 :values (3))
                         (:out v :version 1 :values (3))
                         (:out l1 :values (3))
                         (:out bar :version :infinity :values (9 :second))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (is (equal (multiple-value-list (bar 1 2 3))
                       '(9 :second)))
        (is (equal (list-events)
                       '((:in bar :version :infinity :args (1 2 3))
                         (:in l1 :args (1 2))
                         (:in v :version 1)
                         (:in l2)
                         (:out l2 :values (3))
                         (:out v :version 1 :values (3))
                         (:out l1 :values (3))
                         (:out bar :version :infinity
                          :values (9 :second)))))))))

(deftest test-non-log-in-external ()
  (labels ((foo (x y)
             (journaled (foo :version 1 :args `(,x ,y))
               (+ x y)))
           (bar (x y z)
             (journaled (bar :version :infinity :args `(,x ,y ,z))
               (values (* z (foo x y)) :second))))
    (is (equal '(9 :second) (multiple-value-list (bar 1 2 3))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1 :replay-eoj-error-p t)
        (is (equal (multiple-value-list (bar 1 2 3))
                       '(9 :second)))
        (is (equal (list-events)
                       '((:in bar :version :infinity :args (1 2 3))
                         (:in foo :version 1 :args (1 2))
                         (:out foo :version 1 :values (3))
                         (:out bar :version :infinity :values (9 :second))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (is (equal (multiple-value-list (bar 1 2 3))
                       '(9 :second)))
        (is (equal (list-events)
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

(deftest test-replay-values ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (get-message))
    (is (eq *user7*
                (with-journaling (:replay journal
                                          :record (funcall *make-journal*))
                  (get-message))))))


;;;; Recording and replay of external events

(deftest test-replay-condition ()
  (let ((journal-1 (funcall *make-journal*))
        (journal-2 (funcall *make-journal*)))
    (signals (some-error)
      (with-journaling (:record journal-1)
        (journaled (e :version :infinity
                      :condition (lambda (c)
                                   (when (typep c 'some-error)
                                     `(error ',(type-of c)))))
          (error 'some-error))))
    (signals (some-error)
      (with-journaling (:replay journal-1 :record journal-2)
        (journaled (e :version :infinity :replay-condition #'eval))))
    (is (not (journal-divergent-p journal-2)))
    (is (eq (journal-state journal-2) :completed))
    (is (identical-events-p (list-events journal-2)
                            '((:in e :version :infinity)
                              (:out e :version :infinity
                               :condition (error 'some-error)))))))

(deftest test-external-event-unexpected-outcome ()
  (let* ((journal-1 (funcall *make-journal*))
         (sync (journal-sync journal-1)))
    (signals (some-error)
      (with-journaling (:record journal-1)
        (unwind-protect
             (signals (record-unexpected-outcome :pred "SOME-ERROR"
                                                 :handler nil)
               (journaled (b :version :infinity)
                 (error 'some-error)))
          (is (eq (journal-state journal-1) :logging))
          (journaled (l) 1)
          (journaled (v :version 1) 2)
          (signals (data-event-lossage)
            (journaled (f :version :infinity))))))
    (is (equal (list-events journal-1)
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
      (signals (some-error)
        (with-journaling (:record journal-2 :replay journal-1)
          (unwind-protect
               (signals (record-unexpected-outcome :pred "SOME-ERROR"
                                                   :handler nil)
                 (journaled (b :version :infinity)
                   (error 'some-error)))
            (is (eq (journal-state journal-2) :logging))
            (journaled (l) 1)
            (journaled (v :version 1) 2)
            (signals (data-event-lossage)
              (journaled (f :version :infinity)))))))))

(deftest test-external-event-in-mismatched-triggered-on-in-event ()
  (let ((bundle (make-in-memory-bundle)))
    (with-bundle (bundle)
      (checked (c :args '(1))))
    (with-bundle (bundle)
      (signals (replay-args-mismatch)
        (checked (c :args '(2))))
      (is (eq (journal-state (record-journal)) :mismatched))
      (signals (data-event-lossage)
        (replayed (r)
          (is nil))))))

(deftest test-external-event-in-logging-triggered-on-in-event ()
  (let ((bundle (make-in-memory-bundle)))
    (with-bundle (bundle)
      (catch 'not-finished
        (checked (c)
          (throw 'not-finished nil)))
      (is (eq (journal-state (record-journal)) :logging))
      (signals (data-event-lossage)
        (replayed (r)
          (is nil))))))

(deftest test-unwinding-from-record-unexpected-outcome ()
  (let* ((journal-1 (funcall *make-journal*))
         (sync (journal-sync journal-1)))
    (signals (record-unexpected-outcome :pred "SOME-ERROR")
      (with-journaling (:record journal-1)
        (unwind-protect
             (unwind-but-dont-receive some-error
               (journaled (b :version :infinity)
                 (error 'some-error)))
          (is (eq (journal-state journal-1) :logging))
          (journaled (l) 1)
          (journaled (v :version 1) 2)
          (signals (data-event-lossage)
            (journaled (f :version :infinity))))))
    (is (equal (list-events journal-1)
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

(deftest test-nested-external-events-with-unexpected-outcome ()
  (let ((journal-1 (funcall *make-journal*)))
    (signals (some-error)
      (with-journaling (:record journal-1)
        (journaled (a :version :infinity)
          (unwind-protect
               (journaled (b :version :infinity)
                 (error 'some-error))
            (is (eq (journal-state journal-1) :logging))))))
    (is (equal (list-events journal-1)
                   '((:in a :version :infinity)
                     (:in b :version :infinity)
                     ;; Downgraded to log
                     (:out b
                      :error ("SOME-ERROR" "SOME-ERROR was signalled."))
                     (:out a
                      :error ("SOME-ERROR" "SOME-ERROR was signalled.")))))))

(deftest test-expected-type ()
  (is (string= (funcall (expected-type '(member :a :b)) :a)
                   (jrn::with-standard-io-syntax*
                     (prin1-to-string
                      #+allegro 'cl:symbol
                      #-allegro 'cl:keyword)))))


;;;; Test replay mismatch

(deftest test-replay-mismatch ()
  (let ((journal-1 (funcall *make-journal*)))

    ;; Record
    (with-journaling (:record journal-1)
      (journaled (b1))
      (journaled (b2 :version 1)
        42))
    (is (journal-divergent-p journal-1))
    (destructuring-bind (record-position replay-position)
        (journal-replay-mismatch journal-1)
      (with-open-journal (streamlet journal-1)
        (setf (read-position streamlet) record-position)
        (is (equal (read-event streamlet) '(:in b2 :version 1))))
      (is (null replay-position)))

    ;; Replay diverging with a version upgrade.
    (let ((journal-2 (funcall *make-journal*)))
      (with-journaling (:replay journal-1 :record journal-2)
        (journaled (b2 :version 2)
          7))
      (destructuring-bind (record-position replay-position)
          (journal-replay-mismatch journal-2)
        (with-open-journal (streamlet journal-2)
          (setf (read-position streamlet) record-position)
          (is (equal (read-event streamlet) '(:in b2 :version 2))))
        (with-open-journal (streamlet journal-1)
          (setf (read-position streamlet) replay-position)
          (is (equal (read-event streamlet) '(:in b2 :version 1))))))))


;;;; Test inserting

;;; REPLAY-STRATEGY looks at the next event to decide whether to
;;; insert. For OUT-EVENTs, this means the decision whether to insert
;;; is based on the parent frame, while for IN-EVENTs it is based on
;;; the child frame. This test inserts a new frame with the same name
;;; as the parent to trigger the above failure.
(deftest test-insertable-next-vs-prev-event ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (journaled (a :version 1)
        (journaled (b :version 1))))
    (with-journaling (:replay journal :record (funcall *make-journal*)
                              :replay-eoj-error-p t)
      (journaled (a :version 1)
        (journaled (a :version 1 :insertable t)
          (journaled (b :version 1))))
      (is (equal (list-events)
                     '((:in a :version 1)
                       (:in a :version 1)
                       (:in b :version 1)
                       (:out b :version 1 :values (nil))
                       (:out a :version 1 :values (nil))
                       (:out a :version 1 :values (nil))))))))

(deftest test-force-insertable ()
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
      (is (equal (list-events)
                     '((:in a :version 1)
                       (:in a :version 1)
                       (:in b :version 1)
                       (:out b :version 1 :values (nil))
                       (:out a :version 1 :values (nil))
                       (:out a :version 1 :values (nil))))))))

(deftest test-insert-before-unexpected ()
  (let ((journal (funcall *make-journal*)))
    (signals (some-error)
      (with-journaling (:record journal)
        (journaled (a :version 1)
          (error 'some-error))))
    (with-journaling (:replay journal :record (funcall *make-journal*))
      (journaled (a :version 1)
        (journaled (b :version 1))
        (is (eq (journal-state (record-journal)) :recording))))))

(deftest test-force-insertable-vs-external ()
  (let ((journal (funcall *make-journal*))
        (*force-insertable* t))
    (with-journaling (:record journal)
      (journaled (a :version :infinity)))))

(deftest test-with-replay-streamlet ()
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

(deftest test-peek-replay-event ()
  (flet ((run-v2 ()
           (let ((replay-process-event (peek-replay-event)))
             (journaled (process :version 2))
             (when (if (and replay-process-event
                            (< #+sbcl
                               (sb-ext:truly-the
                                fixnum (event-version replay-process-event))
                               #+cmucl
                               (ext:truly-the
                                fixnum
                                (event-version replay-process-event))
                               #-(or cmucl sbcl)
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

(deftest test-with-replay-filter-with-no-children-left ()
  (labels ((outer (x y)
             (journaled (outer :version 1)
               (inner x y)))
           (inner (x y)
             (journaled (inner :version 1)
               (+ x y)))
           (outer-2 (x y)
             (journaled (outer :version 1)
               (with-replay-filter (:skip `((:name inner)))
                 (+ x y)))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (outer 1 2)
        (is
         (equal (list-events)
                '((:in outer :version 1)
                  (:in inner :version 1)
                  (:out inner :version 1 :values (3))
                  (:out outer :version 1 :values (3))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (outer-2 1 2)
        (is (eq (journal-state (record-journal)) :recording))
        (is
         (equal (list-events)
                '((:in outer :version 1)
                  (:out outer :version 1 :values (3)))))))))

;;; This the first of the at-end-of-replay tests, designed to test
;;; that the :REPLAYING -> :RECORDING transition takes place. With no
;;; child blocks, this particular case is handled by the
;;; SKIP-EVENTS-AND-MAYBE-SET-STATE->RECORDING call in
;;; WITH-REPLAY-FILTER itself.
(deftest test-with-replay-filter-with-no-children-left-at-end-of-replay ()
  (let ((journal-1 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (checked (inner)
        3)
      (is (equal (list-events)
                     '((:in inner :version 1)
                       (:out inner :version 1 :values (3))))))
    (with-journaling (:replay journal-1 :record (funcall *make-journal*))
      (with-replay-filter (:skip '((:name inner)))
        (is (eq (journal-state (record-journal)) :replaying))
        (+ 1 2))
      (is (eq (journal-state (record-journal)) :recording))
      (is (equal (list-events) '())))))

(deftest test-with-replay-filter-eat-full-frames ()
  (let ((journal-1 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (checked (skipped) 2)
      (framed (log)
        (checked (skipped) 3))
      (is (equal (list-events)
                     '((:in skipped :version 1)
                       (:out skipped :version 1 :values (2))
                       (:in log)
                       (:in skipped :version 1)
                       (:out skipped :version 1 :values (3))
                       (:out log :values (3))))))
    (with-journaling (:replay journal-1 :record (funcall *make-journal*))
      (with-replay-filter (:skip '((:name skipped)))
        (is (eq (journal-state (record-journal)) :replaying))
        (is (eq (first (peek-replay-event)) :indeterminate)))
      (is (eq (journal-state (record-journal)) :recording))
      (is (equal (list-events) '()))
      (is (equal (journal-replay-mismatch (record-journal))
                     (if (typep journal-1 'in-memory-journal)
                         '(0 0)
                         '(1 0)))))))

(deftest test-with-replay-filter-eat-full-frames/non-full ()
  (let ((journal-1 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (checked (skipped)
        (checked (c)))
      (is (equal (list-events)
                     '((:in skipped :version 1)
                       (:in c :version 1)
                       (:out c :version 1 :values (nil))
                       (:out skipped :version 1 :values (nil))))))
    (with-journaling (:replay journal-1 :record (funcall *make-journal*))
      (with-replay-filter (:skip '((:name skipped)))
        (is (eq (journal-state (record-journal)) :replaying))
        (is (eq (first (peek-replay-event)) :indeterminate)))
      (is (eq (journal-state (record-journal)) :replaying))
      (is (equal (peek-replay-event) '(:in skipped :version 1)))
      (checked (skipped)
        (checked (c)))
      (is (equal (list-events) '((:in skipped :version 1)
                                     (:in c :version 1)
                                     (:out c :version 1 :values (nil))
                                     (:out skipped :version 1 :values (nil)))))
      (is (eq (journal-state (record-journal)) :recording))
      (is (null (journal-replay-mismatch (record-journal)))))))

(deftest test-with-replay-filter-eat-full-frames/2 ()
  (let ((journal-1 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (checked (other) 2)
      (checked (skipped) 3)
      (is (equal (list-events)
                     '((:in other :version 1)
                       (:out other :version 1 :values (2))
                       (:in skipped :version 1)
                       (:out skipped :version 1 :values (3))))))
    (with-journaling (:replay journal-1 :record (funcall *make-journal*))
      (with-replay-filter (:skip '((:name skipped)))
        (is (eq (journal-state (record-journal)) :replaying))
        (checked (other) 2))
      (is (eq (journal-state (record-journal)) :recording))
      (is (equal (list-events) '((:in other :version 1)
                                     (:out other :version 1 :values (2)))))
      (destructuring-bind (record-position replay-position)
          (journal-replay-mismatch (record-journal))
        (with-open-journal (streamlet (record-journal))
          (setf (read-position streamlet) record-position)
          (is (null (read-event streamlet))))
        (with-open-journal (streamlet (replay-journal))
          (setf (read-position streamlet) replay-position)
          (is (equal (read-event streamlet) '(:in skipped :version 1))))))))

(deftest test-with-replay-filter-with-insert ()
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
               (with-replay-filter (:skip `((:name inner)))
                 (inserting x y)))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (outer 1 2)
        (is
         (equal (list-events)
                '((:in outer :version 1)
                  (:in inner :version 1)
                  (:out inner :version 1 :values (3))
                  (:out outer :version 1 :values (3))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (outer-2 1 2)
        (is (eq (journal-state (record-journal)) :recording))
        (is
         (equal (list-events)
                '((:in outer :version 1)
                  (:in inserting :version 1)
                  (:out inserting :version 1 :values (3))
                  (:out outer :version 1 :values (3)))))))))

;;; This case is also handled by the
;;; SKIP-EVENTS-AND-MAYBE->TO-RECORDING in WITH-REPLAY-FILTER, we just
;;; test the :INSERTABLE does not screw things up.
(deftest test-with-replay-filter-with-insert-at-end-of-replay ()
  (labels ((inner (x y)
             (journaled (inner :version 1)
               (+ x y)))
           (inserting (x y)
             (journaled (inserting :version 1 :insertable t)
               (+ x y))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (inner 1 2)
        (is
         (equal (list-events)
                '((:in inner :version 1)
                  (:out inner :version 1 :values (3))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (with-replay-filter (:skip `((:name inner)))
          (inserting 1 2))
        (is (eq (journal-state (record-journal)) :recording))
        (is
         (equal (list-events)
                '((:in inserting :version 1)
                  (:out inserting :version 1 :values (3)))))))))

;;; Handled by the :UPGRADE case in HANDLE-OUT-EVENT.
(deftest test-with-replay-filter-with-upgrade-at-end-of-replay ()
  (let ((journal-1 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (journaled (outer :version 1)
        (journaled (inner :version 1)
          42))
      (is
       (equal (list-events)
              '((:in outer :version 1)
                (:in inner :version 1)
                (:out inner :version 1 :values (42))
                (:out outer :version 1 :values (42))))))
    (with-journaling (:replay journal-1 :record (funcall *make-journal*))
      (with-replay-filter (:skip '((:name outer)))
        (journaled (inner :version 2)
          7))
      (is (eq (journal-state (record-journal)) :recording))
      (is
       (equal (list-events)
              '((:in inner :version 2)
                (:out inner :version 2 :values (7))))))))

(deftest test-with-replay-filter-simple-unwrap ()
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
               (with-replay-filter (:skip '((:name foo :version< 2)))
                 (foo-2 x y)))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (outer 1 2)
        (is (equal (list-events)
                       '((:in outer :version 1)
                         (:in foo :version 1)
                         (:in foo :version 2)
                         (:out foo :version 2 :values (3))
                         (:out foo :version 1 :values (3))
                         (:out outer :version 1 :values (3))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (filtered-outer 1 2)
        (is (eq (journal-state (record-journal)) :recording))
        (is (equal (list-events)
                       '((:in outer :version 1)
                         (:in foo :version 2)
                         (:out foo :version 2 :values (3))
                         (:out outer :version 1 :values (3)))))))))

;;; Handled by the :MATCH case in HANDLE-OUT-EVENT.
(deftest test-with-replay-filter-simple-unwrap-at-end-of-replay ()
  (labels ((foo-1 (x y)
             (journaled (foo :version 1)
               (foo-2 x y)))
           (foo-2 (x y)
             (journaled (foo :version 2)
               (+ x y))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (foo-1 1 2)
        (is (equal (list-events)
                       '((:in foo :version 1)
                         (:in foo :version 2)
                         (:out foo :version 2 :values (3))
                         (:out foo :version 1 :values (3))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (with-replay-filter (:skip '((:name foo :version< 2)))
          (foo-2 1 2))
        (is (eq (journal-state (record-journal)) :recording))
        (is (equal (list-events)
                       '((:in foo :version 2)
                         (:out foo :version 2 :values (3)))))))))

(deftest test-with-replay-filter-external-unwrap ()
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
               (with-replay-filter (:skip '((:name foo :version< 2)))
                 (foo-2 x y)))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (outer 1 2)
        (is (equal (list-events)
                       '((:in outer :version 1)
                         (:in foo :version 1)
                         (:in foo :version :infinity)
                         (:out foo :version :infinity :values (3))
                         (:out foo :version 1 :values (3))
                         (:out outer :version 1 :values (3))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (filtered-outer 1 2)
        (is (eq (journal-state (record-journal)) :recording))
        (is (equal (list-events)
                       '((:in outer :version 1)
                         (:in foo :version :infinity)
                         (:out foo :version :infinity :values (3))
                         (:out outer :version 1 :values (3)))))))))

;;; Handled by the :MATCH or :UPGRADE case in HANDLE-IN-EVENT.
(deftest test-with-replay-filter-after-in-event ()
  (let ((journal-1 (funcall *make-journal*)))
    (signals (some-error)
      (with-journaling (:record journal-1)
        (checked (foo)
          (checked (bar)
            (error 'some-error)))))
    (with-journaling (:replay journal-1 :record (funcall *make-journal*))
      (with-replay-filter (:skip '((:name bar)))
        (checked (foo)
          (is (eq (journal-state (record-journal)) :recording))))
      (is (eq (journal-state (record-journal)) :recording)))))

;;; Handled by the VALUES-EVENT-P branch in MAYBE-REPLAY-OUTCOME.
(deftest test-with-replay-filter-external-unwrap-at-end-of-replay ()
  (labels ((foo-1 (x y)
             (journaled (foo :version 1)
               (foo-2 x y)))
           (foo-2 (x y)
             (journaled (foo :version :infinity)
               (+ x y))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (foo-1 1 2)
        (is (equal (list-events)
                       '((:in foo :version 1)
                         (:in foo :version :infinity)
                         (:out foo :version :infinity :values (3))
                         (:out foo :version 1 :values (3))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (with-replay-filter (:skip '((:name foo :version< 2)))
          (foo-2 1 2))
        (is (eq (journal-state (record-journal)) :recording))
        (is (equal (list-events)
                       '((:in foo :version :infinity)
                         (:out foo :version :infinity :values (3)))))))))

;;; Handled by the CONDITION-EVENT-P branch in MAYBE-REPLAY-OUTCOME.
(deftest test-with-replay-filter-external-condition-unwrap-at-end-of-replay ()
  (flet ((bar ()
           (signals (some-error)
             (replayed (bar :condition (expected-type 'some-error)
                            :replay-condition (lambda (string)
                                                (declare (ignore string))
                                                (error 'some-error)))
               (error 'some-error)))
           nil))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (checked (foo)
          (bar))
        (is (equal (list-events)
                       '((:in foo :version 1)
                         (:in bar :version :infinity)
                         (:out bar :version :infinity
                          :condition "JOURNAL-TEST::SOME-ERROR")
                         (:out foo :version 1 :values (nil))))))
      (with-journaling (:replay journal-1 :record (funcall *make-journal*))
        (with-replay-filter (:skip '((:name foo)))
          (bar))
        (is (eq (journal-state (record-journal)) :recording))
        (is (equal (list-events)
                       '((:in bar :version :infinity)
                         (:out bar :version :infinity
                          :condition "JOURNAL-TEST::SOME-ERROR"))))))))

(deftest test-with-replay-filter-unwrap ()
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
                 (with-replay-filter (:skip `((:name foo :version< ,v)))
                   (foo-3 x y)
                   (foo-inf x y))))))
    (let ((journal-1 (funcall *make-journal*)))
      (with-journaling (:record journal-1)
        (outer 1 2)
        (is (equal (list-events)
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
        (is (eq (journal-state (record-journal)) :recording))
        (is (equal (list-events)
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
(deftest test-with-replay-filter-parent-intact ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (journaled (outer :version 1)
        3)
      (is (equal (list-events)
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
        (with-replay-filter (:skip `((:name outer)))
          3))
      (is (eq (journal-state (record-journal)) :recording))
      (is (equal (list-events)
                     '((:in outer :version 1)
                       (:out outer :version 1 :values (3))))))))

;;; When it's undecidable whether the events that could be filtered
;;; actually fall within the dynamic extent of replay, choose to
;;; filter them.
(deftest test-with-replay-filter-greed ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (journaled (j2 :version 2))
      (journaled (j2 :version 1)))
    (with-journaling (:replay journal :replay-eoj-error-p t)
      (with-replay-filter (:skip '((:name j2)))))))

(deftest test-with-replay-filter-nesting ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (journaled (j1 :version 1)
        (journaled (j2 :version 3))
        (journaled (j2 :version 2)
          (journaled (j3 :version 1)))
        (journaled (j2 :version 1))))
    (with-journaling (:replay journal :replay-eoj-error-p t)
      (with-replay-filter (:skip '((:name j1)))
        (journaled (j2 :version 3))
        ;; Due to greediness J2/1 is consumed, too.
        (with-replay-filter (:skip '((:name j2)))
          (journaled (j3 :version 1)))))))

(deftest test-with-replay-filter-around-with-journaling ()
  (with-replay-filter (:skip '((:name foo)))
    (is (null jrn::*skip-events*))
    (with-journaling (:replay (make-in-memory-journal :events ()))
      (is (null jrn::*skip-events*))
      (with-replay-filter (:skip '((:name foo)))
        (is jrn::*skip-events*)
        (with-journaling (:replay (make-in-memory-journal :events ()))
          (is (null jrn::*skip-events*)))))))

(deftest test-with-replay-filter-recursive ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (journaled (j1 :version 1)
        (journaled (j2 :version 1)
          (journaled (j1 :version 2)
            (journaled (j1 :version 3)
              (journaled (j1 :version 3)))))))
    (with-journaling (:replay journal :replay-eoj-error-p t)
      (with-replay-filter (:skip '((:name j1)))
        (journaled (j2 :version 1))))))

(deftest test-with-replay-filter-with-imbalanced-log-events ()
  (let ((journal-1 (funcall *make-journal*))
        (journal-2 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (signals (some-error)
        ;; J1's in-event is a VERSIONED-EVENT, but the out-event is a
        ;; LOG-EVENT because we switch to :INSERT replay strategy on
        ;; :ERROR.
        (checked (j1)
          (checked (j2))
          (error 'some-error))))
    (with-journaling (:replay journal-1 :record journal-2)
      (with-replay-filter (:skip '())
        (checked (j1)
          (checked (j2)))))))

(deftest test-no-replay-outcome ()
  (let ((journal-1 (funcall *make-journal*))
        (journal-2 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (replayed (r1)
        (+ 1 (replayed (r2)
               2))))
    (let ((r1-run nil)
          (r2-run nil))
      (with-journaling (:replay journal-1 :record journal-2)
        (with-replay-filter (:no-replay-outcome '(r1))
          (replayed (r1)
            (setq r1-run t)
            (+ 1 (replayed (r2)
                   (setq r2-run t)
                   2)))))
      (is r1-run)
      (is (not r2-run)))))

(deftest test-with-replay-filter-map ()
  (let ((journal-1 (funcall *make-journal*)))
    (with-journaling (:record journal-1)
      (checked (c)))
    ;; Identical
    (let ((journal-2 (funcall *make-journal*)))
      (with-journaling (:replay journal-1 :record journal-2)
        (with-replay-filter (:map (lambda (e)
                                    (copy-seq e)))
          (checked (c))))
      (is (not (journal-divergent-p journal-2))))
    ;; Renamed
    (let ((journal-2 (funcall *make-journal*)))
      (with-journaling (:replay journal-1 :record journal-2)
        (with-replay-filter (:map (lambda (e)
                                    (let ((e (copy-seq e)))
                                      (setf (second e) 'd)
                                      e)))
          (checked (d))))
      (is (journal-divergent-p journal-2)))))


;;;; Test replay restarts

(deftest test-replay-restarts ()
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
        (is (equal (list-events)
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
        (is (equal (list-events)
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
        (is (equal (list-events)
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
        (is (equal (list-events)
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
        (is (equal (list-events)
                       '((:in c1 :version 2 :args (1))
                         (:out c1 :version 2 :values (7)))))))))


(deftest test-events ()
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
  (test-recording-after-replay-failure)
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
  (test-external-event-in-mismatched-triggered-on-in-event)
  (test-external-event-in-logging-triggered-on-in-event)
  (test-unwinding-from-record-unexpected-outcome)
  (test-nested-external-events-with-unexpected-outcome)
  (test-expected-type)
  ;; Replay mismatch
  (test-replay-mismatch)
  ;; Inserting
  (test-insertable-next-vs-prev-event)
  (test-force-insertable)
  (test-insert-before-unexpected)
  (test-force-insertable-vs-external)
  (test-force-insertable-vs-external)
  (test-with-replay-streamlet)
  (test-peek-replay-event)
  ;; Upgrades
  (test-with-replay-filter-with-no-children-left)
  (test-with-replay-filter-with-no-children-left-at-end-of-replay)
  (test-with-replay-filter-eat-full-frames)
  (test-with-replay-filter-eat-full-frames/non-full)
  (test-with-replay-filter-eat-full-frames/2)
  (test-with-replay-filter-with-insert)
  (test-with-replay-filter-with-insert-at-end-of-replay)
  (test-with-replay-filter-with-upgrade-at-end-of-replay)
  (test-with-replay-filter-simple-unwrap)
  (test-with-replay-filter-simple-unwrap-at-end-of-replay)
  (test-with-replay-filter-external-unwrap)
  (test-with-replay-filter-after-in-event)
  (test-with-replay-filter-external-unwrap-at-end-of-replay)
  (test-with-replay-filter-external-condition-unwrap-at-end-of-replay)
  (test-with-replay-filter-unwrap)
  (test-with-replay-filter-parent-intact)
  (test-with-replay-filter-greed)
  (test-with-replay-filter-nesting)
  (test-with-replay-filter-around-with-journaling)
  (test-with-replay-filter-recursive)
  (test-with-replay-filter-with-imbalanced-log-events)
  (test-no-replay-outcome)
  (test-with-replay-filter-map)
  ;; Replay restarts
  (test-replay-restarts))

(deftest test-in-memory-journal ()
  (test-events))

(defun test-depth ()
  (let ((journal (funcall *make-journal*)))
    (with-open-journal (streamlet journal :direction :io)
      (write-event (make-in-event) streamlet)
      (is (= (journal::%in-depth streamlet) 0))
      (is (= (journal::%out-depth streamlet) 1))
      (is (equal (read-event streamlet t) '(:in nil)))
      (is (= (journal::%in-depth streamlet) 1))
      (is (= (journal::%out-depth streamlet) 1))
      (write-event (make-in-event) streamlet)
      (is (= (journal::%in-depth streamlet) 1))
      (is (= (journal::%out-depth streamlet) 2)))))

(deftest test-file-journal ()
  (call-with-file-journal-settings #'test-events)
  (call-with-file-journal-settings #'test-depth :sync '(nil)))

(defun call-with-file-journal-settings (fn &key (sync '(nil t)))
  (dolist (sync sync)
    (let ((files-created ()))
      (flet ((make-temp-file-journal ()
               (let ((pathname (uiop/stream:with-temporary-file
                                   (:pathname pathname)
                                 pathname)))
                 (push pathname files-created)
                 (make-file-journal pathname :sync sync))))
        (let ((*make-journal* #'make-temp-file-journal))
          (unwind-protect
               (funcall fn)
            (dolist (file files-created)
              (ignore-errors (delete-file file)))))))))


(deftest test-io-direction ()
  (let ((journal (make-in-memory-journal :state :completed)))
    (with-open-journal (streamlet journal :direction :input)))
  (signals (file-error)
    ;; This file does not exist.
    (let ((journal (make-file-journal "sd8f76876dsaf,cv")))
      (with-open-journal (streamlet journal :direction :input))))
  (signals (streamlet-error :pred "is not an output streamlet")
    (let ((journal (make-in-memory-journal)))
      (with-open-journal (streamlet journal :direction :input)
        (write-event (make-in-event) streamlet))))
  (signals (streamlet-error :pred "is not an input streamlet")
    (let ((journal (make-in-memory-journal)))
      (with-open-journal (streamlet journal :direction :output)
        (read-event streamlet)))))


(deftest test-in-memory-bundle ()
  (let ((bundle (make-in-memory-bundle :max-n-failed 1)))
    (is (= 0 (length (jrn::journals bundle))))

    ;; Do a quick successful run.
    (with-bundle (bundle)
      (check-in-memory-bundle bundle '(:recording))
      (journaled (foo :version 1 :args '(1 2))
        (check-in-memory-bundle bundle '(:recording))
        42))
    (check-in-memory-bundle bundle '(:completed))

    ;; Now fail.
    (signals (replay-name-mismatch)
      (with-bundle (bundle)
        (check-in-memory-bundle bundle '(:replaying :completed))
        (unwind-protect
             (journaled (bar :version 1 :args '(1 2))
               (is nil))
          (check-in-memory-bundle bundle '(:mismatched :completed)))))
    (check-in-memory-bundle bundle '(:failed :completed))

    ;; Fail again, differently, and hit MAX-N-FAILED.
    (let ((journals-before (coerce (copy-seq (jrn::journals bundle)) 'list)))
      (signals (replay-outcome-mismatch)
        (with-bundle (bundle)
          (check-in-memory-bundle bundle '(:replaying :failed :completed))
          (unwind-protect
               (journaled (foo :version 1 :args '(1 2))
                 7)
            (check-in-memory-bundle
             bundle '(:mismatched :failed :completed)))))
      (check-in-memory-bundle bundle '(:failed :completed))
      ;; Check that the least recent one was removed.
      (is (not (equal journals-before
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
  (is (equal (mapcar #'journal-state (jrn::journals bundle))
                 states)))

(deftest test-file-bundle ()
  (call-with-file-bundle #'test-file-bundle*))

(defun call-with-file-bundle (fn)
  (let* ((random-string (format nil "~:@(~36,8,'0R~)"
                                (random (expt 36 8) (make-random-state t))))
         (directory (merge-pathnames
                     (format nil "journal-test-~A/" random-string)
                     (uiop:temporary-directory)))
         (bundle (make-file-bundle directory :max-n-failed 1
                                   :max-n-completed nil)))
    (unwind-protect
         (funcall fn bundle)
      (delete-file-bundle directory))))

(deftest test-file-bundle* (bundle)
  (is (= 0 (length (jrn::journals bundle))))

  ;; Do a quick successful run.
  (with-bundle (bundle)
    (check-file-bundle bundle '((0 :recording)) '((0 :completed)))
    (journaled (foo :version 1 :args '(1 2))
      (check-file-bundle bundle '((0 :recording)) '((0 :completed)))
      42))
  (check-file-bundle bundle '((0 :completed)) '((0 :completed)))

  ;; Now fail.
  (signals (replay-name-mismatch)
    (with-bundle (bundle)
      (check-file-bundle bundle '((1 :replaying) (0 :completed))
                         '((1 :failed) (0 :completed)))
      (unwind-protect
           (journaled (bar :version 1 :args '(1 2))
             (is nil))
        (check-file-bundle bundle '((1 :mismatched) (0 :completed))
                           '((1 :failed) (0 :completed))))))
  (check-file-bundle bundle '((1 :failed) (0 :completed))
                     '((1 :failed) (0 :completed)))

  ;; Fail again (but differently to avoid the identical branch in
  ;; JRN::REAP-IDENTICAL-OR-NON-DIVERGENT-JOURNALS) and hit MAX-N-FAILED.
  (signals (replay-outcome-mismatch)
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
    (is (equal (ids-and-states bundle) ids-and-states))
    (let ((reloaded-bundle
            ;; KLUDGE: We want a new instance pointing to the same
            ;; directory, and to populate it with new FILE-JOURNALS.
            (let ((jrn::*truename-to-file-bundle* (make-hash-table
                                                   :test #'equal))
                  (jrn::*truename-to-file-journal* (make-hash-table
                                                    :test #'equal)))
              (make-file-bundle (directory-of bundle)))))
      (is (equal (ids-and-states reloaded-bundle)
                     reloaded-ids-and-states)))))


;;;; Synchronization

(deftest test-file-sync ()
  (let* ((random-string (format nil "~:@(~36,8,'0R~)"
                                (random (expt 36 8) (make-random-state t))))
         (filename (merge-pathnames
                    (format nil "file-sync-test-~A" random-string)
                    (uiop:temporary-directory))))
    (jrn::with-standard-io-syntax*
      (with-open-file (stream filename :direction :output)
        ;; :COMPLETED
        (write-char #\Newline stream)
        (prin1 '(:in a :version 1) stream)
        ;; Committed transaction
        (write-char #\Ack stream)
        (prin1 '(:in b :version 1) stream)
        (prin1 '(:in c :version 1) stream)
        ;; Unfinished transaction
        (write-char #\Rubout stream)
        (prin1 '(:in d :version 1) stream)))
    (is (equal (list-events (make-file-journal filename :sync t))
                   '((:in a :version 1)
                     (:in b :version 1)
                     (:in c :version 1))))))

(deftest test-file-sync-garbage ()
  (let* ((random-string (format nil "~:@(~36,8,'0R~)"
                                (random (expt 36 8) (make-random-state t))))
         (filename (merge-pathnames
                    (format nil "file-sync-test-~A" random-string)
                    (uiop:temporary-directory))))
    (jrn::with-standard-io-syntax*
      (with-open-file (stream filename :direction :output)
        ;; :COMPLETED
        (write-char #\Newline stream)
        (prin1 '(:in a :version 1) stream)
        ;; Committed transaction
        (write-char #\Ack stream)
        (prin1 '(:in b :version 1) stream)
        (prin1 '(:in c :version 1) stream)
        ;; Unfinished transaction
        (write-char #\Rubout stream)
        (format stream "(:in d :ve")))
    (is (equal (list-events (make-file-journal filename :sync t))
                   '((:in a :version 1)
                     (:in b :version 1)
                     (:in c :version 1))))))

(deftest test-sync-t ()
  (let ((db ())
        (db-since-prev-sync ())
        (sync-pos nil)
        (n-syncs 0)
        (sync-pos-in-invoked nil)
        (n-syncs-in-invoked 0))
    (flet-invoked ((invoked (x) ("inv")
                            (setq sync-pos-in-invoked sync-pos)
                            (setq n-syncs-in-invoked n-syncs)
                            (1+ x)))
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
          (framed (log))
          (is (= n-syncs 0))
          (checked (versioned))
          (is (= n-syncs 0))
          (invoked 1)
          (is (= sync-pos-in-invoked 0))
          (is (= n-syncs-in-invoked 1))
          (is (equal db-since-prev-sync
                         '((:in log)
                           (:out log :values (nil))
                           (:in versioned :version 1)
                           (:out versioned :version 1 :values (nil))
                           (:in "inv" :version 1 :args (1)))))
          (is (= sync-pos 0))
          (is (= n-syncs 1))
          (replayed (a)
            (is (= n-syncs 1))
            2)
          (is (equal db-since-prev-sync
                         '((:out "inv" :version 1 :values (2))
                           (:in a :version :infinity)
                           (:out a :version :infinity :values (2)))))
          (is (= sync-pos 5))
          (is (= n-syncs 2))
          (signals (some-error)
            (replayed (b)
              (is (= n-syncs 2))
              (error 'some-error)))
          (is (= n-syncs 2)))
        ;; SYNC-STREAMLET is called in CLOSE-STREAMLET, but it does
        ;; nothing because no events were written since the previous
        ;; sync.
        (is (= n-syncs 3))

        ;; Replay
        (setq db-since-prev-sync ()
              sync-pos nil
              n-syncs 0
              sync-pos-in-invoked nil
              n-syncs-in-invoked 0)
        (with-journaling (:record (make-db-backed-record-journal)
                                  :replay (make-db-backed-replay-journal))
          ;; We don't repeat the log block, it doesn't matter.
          (checked (versioned))
          ;; INVOKED gets invoked before this.
          (is (= n-syncs 0))
          (replayed (a) 2)
          (is (= n-syncs 0))
          ;; This fixes up the error in the previous run and must
          ;; trigger a sync.
          (replayed (b)
            3)
          (is (= n-syncs 1))
          (is (= sync-pos 0))
          (is (equal db-since-prev-sync
                         '((:in versioned :version 1)
                           (:out versioned :version 1 :values (nil))
                           (:in "inv" :version 1 :args (1))
                           (:out "inv" :version 1 :values (2))
                           (:in a :version :infinity)
                           (:out a :version :infinity :values (2))
                           (:in b :version :infinity)
                           (:out b :version :infinity :values (3)))))
          ;; Extend the replay with events that don't trigger syncing ...
          (journaled (c :version 1))
          (is (= n-syncs 1)))
        ;; ... and check that CLOSE-STREAMLET syncs.
        (is (= n-syncs 2))

        ;; Second replay
        (setq db-since-prev-sync ()
              sync-pos nil
              n-syncs 0
              sync-pos-in-invoked nil
              n-syncs-in-invoked 0)
        (with-journaling (:record (make-db-backed-record-journal)
                                  :replay (make-db-backed-replay-journal))
          (checked (versioned))
          (is (= n-syncs 0))
          (replayed (a) 2)
          (is (= n-syncs 0))
          (replayed (b) 3)
          (is (= n-syncs 0))
          (checked (c))
          (is (not (journal-divergent-p (record-journal))))
          ;; No sync upon :REPLAYING -> :RECORDING.
          (is (= n-syncs 0)))
        ;; No sync in CLOSE-STREAMLET.
        (is (= n-syncs 0))))))


(deftest test-fsync ()
  (with-failure-expected ((alexandria:featurep :abcl))
    (signals (error)
      (jrn::fsync 32433))))


(defun foo (x)
  (1+ x))

(defun bar (x)
  (foo (+ x 2))
  (error "xxx"))

(deftest test-jtrace-basic ()
  (let ((*package* (find-package :journal-test)))
    (juntrace)
    (jtrace foo bar)
    (is (equal (sort (jtrace) #'string< :key #'symbol-name) '(bar foo)))
    (is (equal (with-output-to-string (*trace-output*)
                 (foo 1))
               (format nil "~%(FOO 1)~%=> 2")))
    (is (equal (let ((*trace-pretty* nil))
                 (with-output-to-string (*trace-output*)
                   (foo 1)))
               (format nil "~%(:IN FOO :ARGS (1))~%~
                         (:OUT FOO :VALUES (2))")))
    (is (equal (with-output-to-string (*trace-output*)
                 (signals (simple-error :pred "xxx")
                   (bar 1)))
               (format nil "~%(BAR 1)~%  (FOO 3)~%  => 4~%~
                            =E \"SIMPLE-ERROR\" \"xxx\"")))
    (juntrace bar)
    (is (equal (sort (jtrace) #'string< :key #'symbol-name) '(foo)))
    (juntrace)
    (is (endp (jtrace)))))

(defun muck-with-user (user)
  user)

(deftest test-jtrace-with-print-readably ()
  (let ((*print-readably* t))
    (jtrace muck-with-user)
    ;; This used to fail with *PRINT-READABLY*.
    (with-output-to-string (*trace-output*)
      (muck-with-user *user7*))))

(deftest test-jtrace ()
  (test-jtrace-basic)
  (test-jtrace-with-print-readably))


;;;; Single-writer

(deftest test-nested-with-journaling ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (is (eq (journal-state journal) :recording))
      (signals (journal-error :pred "is not in state :NEW.")
        (with-journaling (:record journal)
          (journaled (foo)))))))

(deftest test-concurrent-writers ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (with-open-journal (streamlet journal :direction :input))
      (signals (journal-error :pred "Concurrent write access")
        (with-open-journal (streamlet journal :direction :io)))
      (signals (journal-error :pred "Concurrent write access")
        (with-open-journal (streamlet journal :direction :output))))))

(deftest test-nested-log-record ()
  (let ((journal (funcall *make-journal*)))
    (with-journaling (:record journal)
      (journaled (foo :log-record journal)))))

(deftest test-log-record-before-journaled ()
  (let ((journal (funcall *make-journal*)))
    (journaled (foo :log-record journal))
    (signals (journal-error :pred "Concurrent write access")
      (with-journaling (:record journal)))))

(deftest test-concurrent-log-record ()
  (when bt:*supports-threads-p*
    (let ((journal (funcall *make-journal*))
          (in-sem (bt:make-semaphore))
          (out-sem (bt:make-semaphore)))
      (bt:make-thread (lambda ()
                        (bt:wait-on-semaphore in-sem)
                        (unwind-protect
                             (signals (journal-error
                                       :pred "Concurrent write access")
                               (journaled (foo :log-record journal)))
                          (bt:signal-semaphore out-sem))))
      (with-journaling (:record journal)
        (bt:signal-semaphore in-sem)
        (bt:wait-on-semaphore out-sem)))))

(deftest test-nested-with-bundle ()
  (let ((bundle (make-in-memory-bundle)))
    (with-bundle (bundle)
      (signals (journal-error :pred "Concurrent write access")
        (with-bundle (bundle))))))

(deftest test-concurrent-with-bundle ()
  (when bt:*supports-threads-p*
    (let ((bundle (make-in-memory-bundle))
          (in-sem (bt:make-semaphore))
          (out-sem (bt:make-semaphore)))
      (bt:make-thread (lambda ()
                        (bt:wait-on-semaphore in-sem)
                        (unwind-protect
                             (signals (journal-error
                                       :pred "Concurrent write access")
                               (with-bundle (bundle)))
                          (bt:signal-semaphore out-sem))))
      (with-bundle (bundle)
        (bt:signal-semaphore in-sem)
        (bt:wait-on-semaphore out-sem)))))

(deftest test-single-writer ()
  (test-nested-with-journaling)
  (test-concurrent-writers)
  (test-nested-log-record)
  (test-log-record-before-journaled)
  ;; KLUDGE: We get "Function with declared result type NIL returned:
  ;; BORDEAUX-THREADS:CONDITION-WAIT".
  (with-skip ((alexandria:featurep :cmucl))
    (test-concurrent-log-record))
  (test-nested-with-bundle)
  (with-skip ((alexandria:featurep :cmucl))
    (test-concurrent-with-bundle)))


;;;; Error handling

;;; Check that WITH-JOURNALING doesn't care about errors and such if
;;; the events are right.
(deftest test-nlx-from-with-journaling ()
  (let ((journal-1 (funcall *make-journal*)))
    (catch 'foo
      (with-journaling (:record journal-1)
        (journaled (b1 :version 1)
          1)
        (throw 'foo nil)))
    (is (equal (list-events journal-1)
                   '((:in b1 :version 1)
                     (:out b1 :version 1 :values (1)))))
    ;; Replay
    (let ((journal-2 (funcall *make-journal*)))
      (signals (some-error)
        (with-journaling (:replay journal-1 :record journal-2)
          (journaled (b1 :version 1)
            1)
          (error 'some-error)))
      (is (eq (journal-state journal-2) :completed))
      (is (equal (list-events journal-2)
                     '((:in b1 :version 1)
                       (:out b1 :version 1 :values (1))))))))

(deftest test-error-two-deep ()
  (let ((journal-1 (funcall *make-journal*)))
    (signals (some-error)
      (with-journaling (:record journal-1)
        (journaled (b1 :version 1)
          (journaled (b2 :version 1)
            (error 'some-error))
          1)))
    (is (equal (list-events journal-1)
                   '((:in b1 :version 1)
                     (:in b2 :version 1)
                     (:out b2
                      :error ("SOME-ERROR" "SOME-ERROR was signalled."))
                     (:out b1
                      :error ("SOME-ERROR" "SOME-ERROR was signalled.")))))))

(deftest test-replay-failure-two-deep ()
  (let ((journal-1 (funcall *make-journal*)))
    ;; Record
    (with-journaling (:record journal-1)
      (checked (b1)
        (checked (b2)
          2)
        1))
    ;; Fail the replay in B2.
    (dolist (journal-2 (list (funcall *make-journal*) nil))
      (signals (replay-outcome-mismatch)
        (with-journaling (:replay journal-1 :record journal-2)
          (checked (b1)
            (checked (b2)
              7)
            1)))
      (when journal-2
        (is (eq (journal-state journal-2) :failed)))
      ;; The last event recorded is what caused the replay error. B1's
      ;; out-event is not recorded.
      (when (typep journal-2 'in-memory-journal)
        (is (equal (list-events journal-2)
                       '((:in b1 :version 1)
                         (:in b2 :version 1)
                         (:out b2 :version 1 :values (7))
                         (:out b1 :error
                          ("REPLAY-OUTCOME-MISMATCH"
                           "The EXITs and OUTCOMEs of the new event (:OUT JOURNAL-TEST::B2 :VERSION 1 :VALUES (7)) and the REPLAY-EVENT (:OUT JOURNAL-TEST::B2 :VERSION 1 :VALUES (2)) (at position 2) are not equal.")))))))))

(deftest test-replay-failure-two-deep-with-error ()
  (let ((journal-1 (funcall *make-journal*)))
    ;; Record
    (with-journaling (:record journal-1)
      (journaled (b1 :version 1)
        (journaled (b2 :version 1)
          2)
        1))
    ;; Fail the replay in B2, but this time by signalling an ERROR.
    (dolist (journal-2 (list (funcall *make-journal*) nil))
      (signals (replay-unexpected-outcome)
        (unwind-but-dont-receive some-error
          (with-journaling (:replay journal-1 :record journal-2)
            (journaled (b1 :version 1)
              (journaled (b2 :version 1)
                (error 'some-error))
              1))))
      (when journal-2
        (is (eq (journal-state journal-2) :failed)))
      ;; The last event recorded is what caused the replay error. B1's
      ;; out-event is not recorded.
      (when (typep journal-2 'in-memory-journal)
        (is (equal (list-events journal-2)
                       '((:in b1 :version 1)
                         (:in b2 :version 1)
                         (:out b2 :version 1
                          :error ("SOME-ERROR" "SOME-ERROR was signalled."))
                         (:out b1 :error
                          ("REPLAY-UNEXPECTED-OUTCOME"
                           "The new event (:OUT JOURNAL-TEST::B2 :VERSION 1 :ERROR (\"SOME-ERROR\" \"SOME-ERROR was signalled.\")) has an unexpected outcome while the REPLAY-EVENT (:OUT JOURNAL-TEST::B2 :VERSION 1 :VALUES (2)) (at position 2) has not.")))))))))

(deftest test-values-fn-failure-recording ()
  (let* ((journal-1 (funcall *make-journal*))
         (sync (journal-sync journal-1)))
    (signals (journaling-failure :pred "SOME-ERROR")
      (with-journaling (:record journal-1)
        (unwind-but-dont-receive some-error
          (journaled (b1 :version 1 :values (lambda (list)
                                              (declare (ignore list))
                                              (error 'some-error)))))))
    (is (equal (list-events journal-1)
                   (if sync () '((:in b1 :version 1)))))
    (is (eq (journal-state journal-1) :completed))
    ;; Replay is the same.
    (let ((journal-2 (funcall *make-journal*)))
      (signals (journaling-failure :pred "SOME-ERROR")
        (with-journaling (:replay journal-1 :record journal-2)
          (unwind-but-dont-receive some-error
            (journaled (b1 :version 1 :values (lambda (list)
                                                (declare (ignore list))
                                                (error 'some-error)))))))
      (is (eq (journal-state journal-2) :completed))
      (is (equal (list-events journal-2)
                     (if sync () '((:in b1 :version 1))))))))

(deftest test-values-fn-failure-replaying ()
  (let* ((journal-1 (funcall *make-journal*))
         (sync (journal-sync journal-1)))
    (with-journaling (:record journal-1)
      (journaled (b1 :version 1)
        1))
    (is (equal (list-events journal-1)
                   '((:in b1 :version 1)
                     (:out b1 :version 1 :values (1)))))
    (is (eq (journal-state journal-1) :completed))
    ;; Replay
    (let ((journal-2 (funcall *make-journal*)))
      (signals (journaling-failure :pred "SOME-ERROR")
        (with-journaling (:replay journal-1 :record journal-2)
          (unwind-protect
               (unwind-but-dont-receive some-error
                 (journaled (b1 :version 1 :values (lambda (list)
                                                     (declare (ignore list))
                                                     (error 'some-error)))
                   1))
            (is (eq (journal-state journal-2) :failed))
            ;; It is an error to use JOURNALED in state :FAILED.
            (signals (journaling-failure :pred "Resignalling previous")
              (journaled (b2))))))
      (is (eq (journal-state journal-2) :failed))
      (is (equal (list-events journal-2)
                     (if sync () '((:in b1 :version 1))))))))

(deftest test-journaling-failure ()
  (let* ((journal-1 (funcall *make-journal*))
         (sync (journal-sync journal-1)))
    (signals (journaling-failure :pred "SOME-ERROR")
      (with-journaling (:record journal-1)
        (unwind-but-dont-receive some-error
          (journaled (b1 :version 1 :values (lambda (list)
                                              (declare (ignore list))
                                              (error 'some-error)))))
        (journaled (b2 :version 1))))
    (is (equal (list-events journal-1)
                   (if sync
                       ()
                       '((:in b1 :version 1)))))
    (is (eq (journal-state journal-1) :completed))

    (dolist (make-journal (list *make-journal* (constantly nil)))
      ;; Error in WRITE-EVENT when :REPLAYING in replay
      (let ((journal-2 (funcall make-journal)))
        (signals (journaling-failure :pred "SOME-ERROR")
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
                (is (eq (journal-state journal-2)
                            (if sync :completed :failed))))
              (signals (journaling-failure :pred "Resignal")
                (journaled (b2 :version 1))))))
        (when journal-2
          (is (equal (list-events journal-2) ()))
          (is (eq (journal-state journal-2)
                      (if sync :completed :failed)))))

      ;; THROW in WRITE-EVENT when :REPLAYING in replay
      (let ((journal-2 (funcall make-journal)))
        (catch 'foo
          (signals (journaling-failure
                    :pred "JOURNALED failed with an unidentified")
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
                  (is (eq (journal-state journal-2)
                              (if sync :completed :failed))))
                (signals (journaling-failure :pred "Resignal")
                  (journaled (b2 :version 1)))))))
        (when journal-2
          (is (equal (list-events journal-2) ()))
          (is (eq (journal-state journal-2)
                      (if sync :completed :failed))))))

    (dolist (commit (if sync '(nil t) '(nil)))
      (dolist (make-journal (if commit
                                (list *make-journal*)
                                (list *make-journal* (constantly nil))))

        ;; Error in WRITE-EVENT when :RECORDING in replay
        (let ((journal-2 (funcall make-journal)))
          (signals (journaling-failure :pred "SOME-ERROR")
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
                  (is (eq (journal-state journal-2) :completed)))
                (signals (journaling-failure :pred "Resignal")
                  (journaled (b2 :version 1))))))
          (when journal-2
            (is (equal (list-events journal-2)
                           (if (and sync (not commit))
                               ()
                               '((:in b1 :version 1)
                                 (:out b1 :version 1 :values (nil))))))
            (is (eq (journal-state journal-2) :completed))))

        ;; THROW in WRITE-EVENT when :RECORDING in replay
        (let ((journal-2 (funcall make-journal)))
          (catch 'foo
            (signals (journaling-failure
                      :pred "JOURNALED failed with an unidentified")
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
                    (is (eq (journal-state journal-2) :completed)))
                  (signals (journaling-failure :pred "Resignal")
                    (journaled (b2 :version 1)))))))
          (when journal-2
            (is (equal (list-events journal-2)
                           (if (and sync (not commit))
                               ()
                               '((:in b1 :version 1)
                                 (:out b1 :version 1 :values (nil))))))
            (is (eq (journal-state journal-2) :completed))))))))

(deftest test-error-handling ()
  (test-nlx-from-with-journaling)
  (test-error-two-deep)
  (test-replay-failure-two-deep)
  (test-replay-failure-two-deep-with-error)
  (test-values-fn-failure-recording)
  (test-values-fn-failure-replaying)
  (test-journaling-failure))

(deftest test-in-memory-journal-error-handling ()
  (test-error-handling))

(deftest test-file-journal-error-handling ()
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
      (is (get-key username)))))

(deftest test-define-file-bundle-test ()
  (test-user-registration)
  (signals (simple-error :pred "not equivalent")
    (let ((*n-registrations* 3))
      (test-user-registration))))


(deftest test-make-file-journal ()
  (call-with-file-journal-settings
   (lambda ()
     ;; :NEW
     (let ((journal (funcall *make-journal*)))
       (is (eq (journal-state journal) :new))
       (dolist (sync '(nil t))
         (if (eq sync (journal-sync journal))
             (is (eq (make-file-journal (pathname-of journal) :sync sync)
                         journal))
             (signals (journal-error :pred "Incompatible options")
               (make-file-journal (pathname-of journal) :sync sync)))))
     ;; :COMPLETED
     (let ((journal (funcall *make-journal*)))
       (with-journaling (:record journal))
       (is (eq (journal-state journal) :completed))
       (dolist (sync '(nil t))
         (if (eq sync (journal-sync journal))
             (is (eq (make-file-journal (pathname-of journal) :sync sync)
                         journal))
             (signals (journal-error :pred "Incompatible options")
               (make-file-journal (pathname-of journal) :sync sync)))))
     ;; :COMPLETED, deleted
     (let ((journal (funcall *make-journal*)))
       (with-journaling (:record journal))
       (is (eq (journal-state journal) :completed))
       (delete-file (pathname-of journal))
       (is (not (eq (make-file-journal (pathname-of journal))
                        journal)))
       (is (eq (slot-value journal 'jrn::n-writers) :invalidated))
       (signals (journal-error :pred "unlinked")
         (with-open-journal (streamlet journal :direction :output)))))))

(deftest test-make-file-bundle ()
  (call-with-file-bundle
   (lambda (bundle)
     (is (eq (make-file-bundle (directory-of bundle)
                                   :max-n-failed (max-n-failed bundle)
                                   :max-n-completed (max-n-completed bundle)
                                   :sync (slot-value bundle 'jrn::sync))
                 bundle))
     (signals (journal-error :pred "Incompatible options")
       (make-file-bundle (directory-of bundle)
                         :max-n-failed (max-n-failed bundle)
                         :max-n-completed (max-n-completed bundle)
                         :sync (not (slot-value bundle 'jrn::sync)))))))


(deftest test-invoked ()
  (test-invoked-without-external)
  (test-error-in-invoked-without-external)
  (test-throw-in-invoked-without-external)
  (test-invoked-without-external/two)
  (test-invoked-without-external/two-separated)
  (test-invoked-successful-external)
  (test-invoked-in-failed-external/first-child)
  (test-invoked-in-failed-external/second-child)
  (test-invoked-in-failed-external/nested-in-child)
  (test-invoked-in-mismatched-triggered-on-in-event)
  (test-invoked-in-logging-triggered-on-in-event))

(defvar *var-for-invoked*)

(define-invoked invoked-0 (x) ("name-invoked-0")
  (setq *var-for-invoked* (+ x 0)))

(define-invoked invoked-1 (x) ("name-invoked-1")
  (setq *var-for-invoked* (+ x 1)))

(deftest test-invoked-without-external ()
  (let ((bundle (make-in-memory-bundle)))
    (let ((*var-for-invoked* nil))
      (with-bundle (bundle)
        (is (endp
                 (multiple-value-list
                  (if (zerop (framed (input :log-record :record) (random 2)))
                      (invoked-0 3)
                      (invoked-1 4)))))
        (checked (c :args `(,*var-for-invoked*)))))
    (let ((*var-for-invoked* nil))
      (with-bundle (bundle)
        (checked (c :args `(,*var-for-invoked*)))))))

(defvar *fail-in-invoked* t)

(define-invoked erroring-invoked (x) ("erroring-invoked")
  (if *fail-in-invoked*
      (error 'some-error)
      (setq *var-for-invoked* x)))

(define-invoked throwing-invoked (x) ("throwing-invoked")
  (if *fail-in-invoked*
      (throw 'catch-invoked nil)
      (setq *var-for-invoked* x)))

(deftest test-error-in-invoked-without-external ()
  (let ((journal-1 (funcall *make-journal*)))
    ;; Fail in INVOKED.
    (with-journaling (:record journal-1)
      (signals (some-error)
        (erroring-invoked 42)))
    (is (equal (list-events journal-1)
                   '((:in "erroring-invoked" :version 1 :args (42))
                     (:out "erroring-invoked"
                      :error ("SOME-ERROR" "SOME-ERROR was signalled.")))))
    ;; Not yet fixed. Replay triggers the same error.
    (let ((journal-2 (funcall *make-journal*)))
      (signals (some-error)
        (with-journaling (:record journal-2 :replay journal-1)))
      (is (eq (journal-state journal-2) :completed))
      (is (equal (list-events journal-2)
                     '((:in "erroring-invoked" :version 1 :args (42))
                       (:out "erroring-invoked"
                        :error ("SOME-ERROR" "SOME-ERROR was signalled."))))))
    ;; Fixed.
    (let ((journal-3 (funcall *make-journal*)))
      (let ((*var-for-invoked* nil)
            (*fail-in-invoked* nil))
        (with-journaling (:record journal-3 :replay journal-1)
          (is (eql *var-for-invoked* 42)))
        (is (eq (journal-state journal-3) :completed))
        (is (equal (list-events journal-3)
                       '((:in "erroring-invoked" :version 1 :args (42))
                         (:out "erroring-invoked" :version 1 :values (42))))))
      ;; Fail again.
      (let ((*var-for-invoked* nil)
            (journal-4 (funcall *make-journal*)))
        (signals (replay-unexpected-outcome)
          (unwind-but-dont-receive some-error
            (with-journaling (:record journal-4 :replay journal-3))))
        (is (eq (journal-state journal-4) :failed))
        (is (equal (list-events journal-4)
                       '((:in "erroring-invoked" :version 1 :args (42))
                         (:out "erroring-invoked" :version 1
                          :error ("SOME-ERROR"
                                  "SOME-ERROR was signalled.")))))))))

(deftest test-throw-in-invoked-without-external ()
  (let ((journal-1 (funcall *make-journal*)))
    ;; Fail in INVOKED.
    (with-journaling (:record journal-1)
      (catch 'catch-invoked
        (throwing-invoked 42)))
    (is (equal (list-events journal-1)
                   '((:in "throwing-invoked" :version 1 :args (42))
                     (:out "throwing-invoked" :nlx nil))))
    ;; Not yet fixed. Replay triggers the same error.
    (let ((journal-2 (funcall *make-journal*)))
      (catch 'catch-invoked
        (with-journaling (:record journal-2 :replay journal-1)))
      (is (eq (journal-state journal-2) :completed))
      (is (equal (list-events journal-2)
                     '((:in "throwing-invoked" :version 1 :args (42))
                       (:out "throwing-invoked" :nlx nil)))))
    ;; Fixed.
    (let ((journal-3 (funcall *make-journal*)))
      (let ((*var-for-invoked* nil)
            (*fail-in-invoked* nil))
        (with-journaling (:record journal-3 :replay journal-1)
          (is (eql *var-for-invoked* 42)))
        (is (eq (journal-state journal-3) :completed))
        (is (equal (list-events journal-3)
                       '((:in "throwing-invoked" :version 1 :args (42))
                         (:out "throwing-invoked" :version 1 :values (42))))))
      ;; Fail again.
      (let ((*var-for-invoked* nil)
            (journal-4 (funcall *make-journal*)))
        (signals (replay-unexpected-outcome)
          (catch 'catch-invoked
            (with-journaling (:record journal-4 :replay journal-3))))
        (is (eq (journal-state journal-4) :failed))
        (is (equal (list-events journal-4)
                       '((:in "throwing-invoked" :version 1 :args (42))
                         (:out "throwing-invoked" :version 1 :nlx nil))))))))

(deftest test-invoked-without-external/two ()
  (let ((bundle (make-in-memory-bundle)))
    (let ((*var-for-invoked* nil))
      (with-bundle (bundle)
        (invoked-0 3)
        (invoked-1 *var-for-invoked*)
        (checked (c :args `(,*var-for-invoked*)))))
    (let ((*var-for-invoked* nil))
      (with-bundle (bundle)
        (checked (c :args `(,*var-for-invoked*)))
        (is (equal (list-events)
                       '((:in "name-invoked-0" :version 1 :args (3))
                         (:out "name-invoked-0" :version 1 :values (3))
                         (:in "name-invoked-1" :version 1 :args (3))
                         (:out "name-invoked-1" :version 1 :values (4))
                         (:in c :version 1 :args (4))
                         (:out c :version 1 :values (nil)))))))))

(deftest test-invoked-without-external/two-separated ()
  (let ((bundle (make-in-memory-bundle)))
    (let ((*var-for-invoked* nil))
      (with-bundle (bundle)
        (invoked-0 3)
        (checked (c :args `(,*var-for-invoked*)))
        (invoked-1 *var-for-invoked*)
        (checked (c :args `(,*var-for-invoked*)))))
    (let ((*var-for-invoked* nil))
      (with-bundle (bundle)
        (checked (c :args `(,*var-for-invoked*)))
        (checked (c :args `(,*var-for-invoked*)))
        (is (equal (list-events)
                       '((:in "name-invoked-0" :version 1 :args (3))
                         (:out "name-invoked-0" :version 1 :values (3))
                         (:in c :version 1 :args (3))
                         (:out c :version 1 :values (nil))
                         (:in "name-invoked-1" :version 1 :args (3))
                         (:out "name-invoked-1" :version 1 :values (4))
                         (:in c :version 1 :args (4))
                         (:out c :version 1 :values (nil)))))))))

(deftest test-invoked-successful-external ()
  (let ((bundle (make-in-memory-bundle)))
    (with-bundle (bundle)
      (let ((*var-for-invoked* nil))
        (replayed ("accept-input")
          (if (zerop (framed (input :log-record :record) (random 2)))
              (invoked-0 3)
              (invoked-1 4))
          42)
        (checked (c :args `(,*var-for-invoked*)))))
    (with-bundle (bundle)
      (let ((*var-for-invoked* nil))
        (replayed ("accept-input")
          (is nil))
        (checked (c :args `(,*var-for-invoked*)))))))

;;; Check that INVOKED events are replayed when the enclosing
;;; REPLAYED is not replayed by outcome, and the INVOKED is the first
;;; child.
(deftest test-invoked-in-failed-external/first-child ()
  (let ((bundle (make-in-memory-bundle)))
    (with-bundle (bundle)
      (let ((*var-for-invoked* nil))
        (catch 'some
          (replayed ("accept-input")
            (invoked-0 3)
            (throw 'some nil)))
        (checked (c :args `(,*var-for-invoked*)))))
    (with-bundle (bundle)
      (let ((*var-for-invoked* nil))
        (replayed ("accept-input")
          (is (eql *var-for-invoked* 3)))
        (checked (c :args `(,*var-for-invoked*)))))))

(deftest test-invoked-in-failed-external/second-child ()
  (let ((bundle (make-in-memory-bundle)))
    (with-bundle (bundle)
      (let ((*var-for-invoked* nil))
        (catch 'some
          (replayed ("accept-input")
            (checked (c1))
            (invoked-0 3)
            (throw 'some nil)))
        (checked (c :args `(,*var-for-invoked*)))))
    (with-bundle (bundle)
      (let ((*var-for-invoked* nil))
        (replayed ("accept-input")
          (is (null *var-for-invoked*))
          (checked (c1)
            (is (null *var-for-invoked*))
            nil)
          (is (eql *var-for-invoked* 3)))
        (checked (c :args `(,*var-for-invoked*)))))))

(deftest test-invoked-in-failed-external/nested-in-child ()
  (let ((bundle (make-in-memory-bundle)))
    (with-bundle (bundle)
      (let ((*var-for-invoked* nil))
        (catch 'some
          (replayed ("accept-input")
            (checked (c1)
              (invoked-0 3)
              nil)
            (throw 'some nil)))
        (checked (c :args `(,*var-for-invoked*)))))
    (with-bundle (bundle)
      (let ((*var-for-invoked* nil))
        (replayed ("accept-input")
          (is (null *var-for-invoked*))
          (checked (c1)
            (is (= *var-for-invoked* 3))
            nil))
        (checked (c :args `(,*var-for-invoked*)))))))

(deftest test-invoked-in-mismatched-triggered-on-in-event ()
  (flet-invoked ((invoked () ("inv")))
    (let ((bundle (make-in-memory-bundle)))
      (with-bundle (bundle)
        (checked (c :args '(1))))
      (with-bundle (bundle)
        (signals (replay-args-mismatch)
          (checked (c :args '(2))))
        (is (eq (journal-state (record-journal)) :mismatched))
        (is (equal (peek-replay-event) '(:in c :version 1 :args (1))))
        (signals (data-event-lossage)
          (invoked))))))

(deftest test-invoked-in-logging-triggered-on-in-event ()
  (flet-invoked ((invoked () ("inv")))
    (let ((bundle (make-in-memory-bundle)))
      (with-bundle (bundle)
        (catch 'not-finished
          (checked (c)
            (throw 'not-finished nil)))
        (is (eq (journal-state (record-journal)) :logging))
        (signals (data-event-lossage)
          (invoked))))))


(deftest test-all ()
  ;; Tests which would fail due to the lack of WITHOUT-INTERRUPTS are
  ;; skipped. Let's leave a reminder.
  (with-failure-expected ((alexandria:featurep '(:or :clisp :abcl)))
    (is jrn::*without-interrupts-available*)
    (is jrn::*with-interrupts-available*))
  (test-events-to-frames)
  (test-in-memory-journal)
  (test-file-journal)
  (test-io-direction)
  (test-in-memory-bundle)
  (test-file-bundle)
  (test-file-sync)
  (test-file-sync-garbage)
  (test-sync-t)
  (test-fsync)
  (test-jtrace)
  (test-single-writer)
  (test-in-memory-journal-error-handling)
  (test-file-journal-error-handling)
  (test-define-file-bundle-test)
  (test-make-file-journal)
  (test-make-file-bundle)
  (test-invoked))

(defun test (&key (debug nil) (print 'unexpected) (describe 'unexpected))
  ;; Bind *PACKAGE* so that names of tests printed have package names,
  ;; and M-. works on them in Slime.
  (let ((*package* (find-package :common-lisp))
        (*print-duration* nil)
        (*print-compactly* nil)
        (*defer-describe* nil)
        (jrn::*testing* t))
    (warn-on-tests-not-run ((find-package :journal-test))
      (print (try 'test-all :debug debug :print print :describe describe)))))

#+nil
(test)
