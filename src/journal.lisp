(in-package :journal)

(in-readtable pythonic-string-syntax)

(defsection @journal-manual (:title "Journal manual")
  (journal asdf:system)
  (@journal-links section)
  (@journal-background section)
  (@journal-features section)
  (@journal-basics section)
  (@logging section)
  (@tracing section)
  (@replay section)
  (@testing section)
  (@persistence section)
  (@safety section)
  (@events-reference section)
  (@journals-reference section)
  (@bundles-reference section)
  (@streamlets-reference section)
  (@glossary section))


(defsection @journal-links (:title "Links")
  "Here is the [official repository](https://github.com/melisgl/journal)
  and the [HTML
  documentation](http://melisgl.github.io/mgl-pax-world/journal-manual.html)
  for the latest version.")

(defsection @journal-background (:title "Background")
  "Logging, tracing, testing, and persistence are about what happened
  during code execution. Recording machine-readable logs and traces
  can be repurposed for white-box testing. More, when the code is
  rerun, selected frames may return their recorded values without
  executing the code, which could serve as a [mock][mock-object]
  framework for writing tests. This ability to isolate external
  interactions and to reexecute traces is sufficient to reconstruct
  the state of a program, achieving simple persistence not unlike a
  [journaling filesystem][journaling-fs] or [Event
  Sourcing][event-sourcing].

    [mock-object]: https://en.wikipedia.org/wiki/Mock_object

    [journaling-fs]: https://en.wikipedia.org/wiki/Journaling_file_system

    [event-sourcing]: https://martinfowler.com/eaaDev/EventSourcing.html

  Journal is the library to log, trace, test and persist. It has a
  single macro at its heart: JOURNALED, which does pretty much what
  was described. It can be thought of as generating two events around
  its body: one that records the name and an argument list (as in a
  function call), and another that records the return values. In
  Lisp-like pseudocode:

  ```
  (defmacro journaled (name args &body body)
    `(progn
       (record-event `(:in ,name :args ,args))
       (let ((,return-values (multiple-value-list (progn ,@body))))
         (record-event `(:out ,name :values ,return-values))
         (values-list ,return-values))))
  ```

  The Journal library is this idea taken to its logical conclusion.")

(defsection @journal-features (:title "Distinguishing features")
  """##### As a logging facility

  - Nested contexts and single messages

  - Customizable content and format

  - Human- or machine-readable output

  ```
  #68200.234: ("some-context")
  #68200.234:   Informative log message
  #68200.250: => NIL
  ```

  See @LOGGING for a complete example.

  ##### Compared to CL:TRACE

  - Ability to handle @NON-LOCAL-EXITs

  - Customizable content and format

  - Optional timestamps, internal real- and run-time

  ```
  (FOO 2.1)
    (1+ 2.1)
    => 3.1
  =E "SIMPLE-ERROR" "The assertion (INTEGERP 3.1) failed."
  ```

  See @TRACING for a complete example.

  ##### As a test framework

  - White-box testing based on execution traces

  - Isolation of external dependencies

  - Record-and-replay testing

  ```
  (define-file-bundle-test (test-user-registration :directory "registration")
    (let ((username (replayed ("ask-username")
                      (format t "Please type your username: ")
                      (read-line))))
      (add-user username)
      (assert (user-exists-p username))))
  ```

  See @TESTING for a complete example.

  ##### As a solution for persistence

  - Event Sourcing: replay interactions with the external world

  - Unchanged control flow

  - Easy to implement history, undo

  ```
  (defun my-resumable-autosaving-game-with-history ()
    (with-bundle (bundle)
      (play-guess-my-number)))
  ```

  See @PERSISTENCE for a complete example.
  """)


(defsection @events-reference (:title "Events reference")
  """Events are normally triggered upon entering and leaving the
  dynamic extent of a JOURNALED @BLOCK (see @IN-EVENTS and
  @OUT-EVENTS) and also by LOGGED. Apart from being part of the
  low-level substrate of the Journal library, working with events
  directly is sometimes useful when writing tests that inspect
  recorded events. Otherwise, skip this entire section.

  All EVENTs have EVENT-NAME and EVENT-VERSION, which feature
  prominently in @THE-REPLAY-STRATEGY. After the examples in
  @IN-EVENTS and @OUT-EVENTS, the following example is a reminder of
  how events look in the simplest case.

  ```
  (with-journaling (:record t)
    (journaled (foo :version 1 :args '(1 2))
      (+ 1 2))
    (logged () "Oops")
    (list-events))
  => ((:IN FOO :VERSION 1 :ARGS (1 2))
      (:OUT FOO :VERSION 1 :VALUES (3))
      (:LEAF "Oops"))
  ```

  So a JOURNALED @BLOCK generates an IN-EVENT and an OUT-EVENT, which
  are simple property lists. The following reference lists these
  properties, there semantics and the functions to read them."""
  (event type)
  (event= function)
  (event-name function)
  (@event-versions section)
  (@in-events-reference section)
  (@out-events-reference section)
  (@leaf-events-reference section))

(deftype event ()
  "An event is either an IN-EVENT, an OUT-EVENT or a LEAF-EVENT."
  '(or in-event out-event leaf-event))

(declaim (inline event-name))
(defun event-name (event)
  "The name of an event can be of any type. It is often a symbol or a
  string. When replaying, names may be compared with EQUAL. All EVENTs
  have names. The names of the in- and out-events belonging to the
  same @FRAME are the same."
  (second event))


(defsection @event-versions (:title "Event versions")
  (event-version function)
  (log-event-p function)
  (versioned-event-p function)
  (external-event-p function))

;;; Stripped down GETF.
(declaim (inline getf*))
(defun getf* (list indicator &optional default)
  (declare (type list list)
           (optimize speed (safety 0) (debug 0)))
  (do ((plist list (cddr plist)))
      ((null plist) default)
    (when (eq (car plist) indicator)
      (return (cadr plist)))))

(declaim (inline event-version))
(defun event-version (event)
  "Return the version of EVENT of type EVENT-VERSION."
  (getf* (cddr event) :version))

(deftype event-version ()
  "An event's version is either NIL, a positive fixnum, or :INFINITY,
  which correspond to LOG-EVENTs, VERSIONED-EVENTs, and
  EXTERNAL-EVENTs, respectively, and have an increasingly strict
  behaviour with regards to @REPLAY. All EVENTs have versions. The
  versions of the in- and out-events belonging to the same @FRAME are
  the same."
  `(or (member nil :infinity) (integer 1 ,most-positive-fixnum)))

(declaim (inline version=))
(defun version= (version-1 version-2)
  (declare (type event-version version-1 version-2)
           (optimize speed))
  (eql version-1 version-2))

(declaim (inline version<))
(defun version< (version-1 version-2)
  (assert version-1)
  (assert version-2)
  (or (and (integerp version-1) (eq version-2 :infinity))
      (and (integerp version-1) (integerp version-2) (< version-1 version-2))))

(declaim (inline log-event-p))
(defun log-event-p (event)
  "See if EVENT is a LOG-EVENT."
  (null (event-version event)))

(declaim (inline versioned-event-p))
(defun versioned-event-p (event)
  "See if EVENT is a VERSIONED-EVENT."
  (integerp (event-version event)))

(declaim (inline external-event-p))
(defun external-event-p (event)
  "See if EVENT is an EXTERNAL-EVENT."
  (eq (event-version event) :infinity))


(defsection @in-events-reference (:title "In-events")
  (in-event type)
  (in-event-p function)
  (make-in-event function)
  (event-args function))

(deftype in-event ()
  "IN-EVENTs are triggered upon entering the dynamic extent of a
  JOURNALED @BLOCK. IN-EVENTs have EVENT-NAME,
  [EVENT-VERSION][function], and EVENT-ARGS. See @IN-EVENTS for a more
  introductory treatment."
 '(satisfies in-event-p))

(declaim (inline in-event-p))
(defun in-event-p (event)
  "See if EVENT is a IN-EVENT."
  (eq (first event) :in))

;;; We want to print events readably into FILE-JOURNALS. They could be
;;; structs, but the name of struct and especially its package would
;;; expose more of the implementation than necessary if written to a
;;; journal, or would need to be translated to this format.
(defun make-in-event (&key name version args)
  "Create an IN-EVENT with NAME, VERSION (of type EVENT-VERSION) and
  ARGS as its EVENT-NAME, [EVENT-VERSION][function] and EVENT-ARGS."
  (check-type version event-version)
  ;; Could be written with a single backtick, but this conses less.
  (cond ((and version args)
         `(:in ,name :version ,version :args ,args))
        (version
         `(:in ,name :version ,version))
        (args
         `(:in ,name :args ,args))
        (t
         `(:in ,name))))

(declaim (inline event-args))
(defun event-args (in-event)
  "Return the arguments of IN-EVENT, normally populated with the ARGS
  form in JOURNALED."
  (getf* (cddr in-event) :args))


(defsection @out-events-reference (:title "Out-events")
  (out-event type)
  (out-event-p function)
  (make-out-event function)
  (event-exit function)
  (expected-outcome-p function)
  (unexpected-outcome-p function)
  (event-outcome function))

(deftype out-event ()
  "OUT-EVENTs are triggered upon leaving the dynamic extent of the
  JOURNALED @BLOCK. OUT-EVENTs have EVENT-NAME,
  [EVENT-VERSION][function], [EVENT-EXIT][function] and EVENT-OUTCOME.
  See @OUT-EVENTS for a more introductory treatment."
  '(satisfies out-event-p))

(declaim (inline out-event-p))
(defun out-event-p (event)
  "See if EVENT is an OUT-EVENT."
  (eq (first event) :out))

(declaim (inline make-out-event))
(defun make-out-event (&key name version exit outcome)
  "Create an OUT-EVENT with NAME, VERSION (of type EVENT-VERSION),
  EXIT (of type EVENT-EXIT), and OUTCOME as its EVENT-NAME,
  [EVENT-VERSION][function], [EVENT-EXIT][function] and
  EVENT-OUTCOME."
  (check-type version event-version)
  (check-type exit event-exit)
  (assert (or (not (eq exit :nlx)) (null outcome)))
  ;; As in MAKE-IN-EVENT, this is a single backtick expression
  ;; expanded by hand to reduce consing.
  (if version
      `(:out ,name :version ,version ,exit ,outcome)
      `(:out ,name ,exit ,outcome)))

(declaim (inline event-exit))
(defun event-exit (out-event)
  "Return how the journaled @BLOCK finished. See [EVENT-EXIT][type]
  for the possible types."
  (let ((cd2r (cddr out-event)))
    (if (eq (first cd2r) :version)
        (third cd2r)
        (first cd2r))))

(declaim (inline values-event-p))
(defun values-event-p (out-event)
  (eq (event-exit out-event) :values))

(declaim (inline condition-event-p))
(defun condition-event-p (out-event)
  (eq (event-exit out-event) :condition))

(declaim (inline error-event-p))
(defun error-event-p (out-event)
  (eq (event-exit out-event) :error))

(declaim (inline nlx-event-p))
(defun nlx-event-p (out-event)
  (eq (event-exit out-event) :nlx))

(declaim (inline expected-outcome-p))
(defun expected-outcome-p (out-event)
  "See if OUT-EVENT has an EXPECTED-OUTCOME."
  (let ((exit (event-exit out-event)))
    (or (eq exit :values) (eq exit :condition))))

(declaim (inline unexpected-outcome-p))
(defun unexpected-outcome-p (out-event)
  "See if OUT-EVENT has an UNEXPECTED-OUTCOME."
  (let ((exit (event-exit out-event)))
    (or (eq exit :error) (eq exit :nlx))))

(declaim (inline event-outcome))
(defun event-outcome (out-event)
  "Return the outcome of the @FRAME (or loosely speaking of a @BLOCK)
  to which OUT-EVENT belongs. "
  (let ((cd2r (cddr out-event)))
    (if (eq (first cd2r) :version)
        (fourth cd2r)
        (second cd2r))))


(defsection @leaf-events-reference (:title "Leaf-events")
  (leaf-event type)
  (leaf-event-p function)
  (make-leaf-event function))

(deftype leaf-event ()
  "Leaf events are triggered by LOGGED. Unlike IN-EVENTs and
  OUT-EVENTs, which represent a @FRAME, leaf events stand alone and
  thus cannot have children. They are also the poorest of their kind:
  they only have an EVENT-NAME. Their VERSION is always NIL, which
  makes them LOG-EVENTs."
  '(satisfies leaf-event-p))

(defun leaf-event-p (event)
  "See if EVENT is a LEAF-EVENT."
  (eq (first event) :leaf))

(defun make-leaf-event (name)
  "Create a LEAF-EVENT with NAME."
  `(:leaf ,name))


(defun event= (event-1 event-2)
  "Return whether EVENT-1 and EVENT-2 represent the same event. In-
  and out-events belonging to the same @FRAME are _not_ the same
  event. EVENT-OUTCOME is not compared when EVENT-EXIT is :ERROR to
  avoid undue dependence on implementation specific string
  representations. This function is useful in conjunction with
  MAKE-IN-EVENT and MAKE-OUT-EVENT to write tests."
  (or (equal event-1 event-2)
      (and (eq (first event-1) (first event-2))
           (equal (event-name event-1) (event-name event-2))
           (eql (event-version event-1) (event-version event-2))
           (eq (event-exit event-1) :error)
           (eq (event-exit event-2) :error))))


(defsection @journals-reference (:title "Journals reference")
  "In @JOURNAL-BASICS, we covered the bare minimum needed to work with
  journals. Here we go into the details."
  (journal class)
  (journal-state (reader journal))
  (journal-sync (reader journal))
  (sync-journal function)
  (journal-replay-mismatch (reader journal))
  (journal-divergent-p function)
  (@comparing-journals section)
  "The rest of section is about concrete subclasses of [JOURNAL][class]."
  (@in-memory-journals section)
  (@file-journals section)
  (@pprint-journals section))

(defclass journal ()
  ((state
    :initarg :state :type journal-state :reader journal-state :accessor %state
    :documentation "Return the state of JOURNAL, which is of type
    [JOURNAL-STATE][TYPE].")
   (sync
    :initform nil :initarg :sync :reader journal-sync
    :documentation "The SYNC argument specified at instantiation. See
    @SYNCHRONIZATION-STRATEGIES.")
   (output-streamlet :initform nil :accessor %output-streamlet-of)
   (log-decorator
    :initform nil :initarg :log-decorator :accessor journal-log-decorator
    :documentation "If non-NIL, a function to add @DECORATION to
    LOG-EVENTs before they are written to a journal. The only allowed
    transformation is to _append_ a plist to the event, which is a
    plist itself. The keys can be anything.")
   (lock :initform (bt:make-lock))
   (n-readers :initform 0)
   (n-writers :initform 0)
   (replay-mismatch
    :initform nil :reader journal-replay-mismatch
    :documentation "If JOURNAL-DIVERGENT-P, then this is a list of two
    elements: the READ-POSITIONs in the RECORD-JOURNAL and
    REPLAY-JOURNAL of the first events that were different (ignoring
    LOG-EVENTs). It is NIL, otherwise."))
  (:documentation "A journal is, conceptually, a sequence of events.
  JOURNAL is an abstract base class. In case of FILE-JOURNALs, the
  events are stored in a file, while for IN-MEMORY-JOURNALs, they are
  in a Lisp array. When a journal is opened, it is possible to perform
  I/O on it (see @STREAMLETS-REFERENCE), which is normally taken care
  of by WITH-JOURNALING. For this reason, the user's involvement with
  journals normally only consists of creating and using them in
  WITH-JOURNALING."))

(defun journal-divergent-p (journal)
  "See if WITH-JOURNALING recorded any event so far in this journal
  which was not EQUAL to its REPLAY-EVENT or it had no corresponding
  replay event. This completely ignores LOG-EVENTs in both journals
  being compared, and is updated continuously during @REPLAY. It plays
  a role in WITH-BUNDLE deciding when a journal is important enough to
  keep, and also in @SYNCHRONIZATION-WITH-IN-MEMORY-JOURNALS.

  The position of the first mismatch that is available via
  JOURNAL-REPLAY-MISMATCH."
  (not (null (journal-replay-mismatch journal))))

(defmethod print-object ((journal journal) stream)
  (print-unreadable-object (journal stream :type t)
    (format stream "~S~A" (ignore-errors (journal-state journal))
            (ignore-errors (if (journal-sync journal) " SYNC" "")))))

(defmacro with-journal-locked ((journal) &body body)
  `(bt:with-recursive-lock-held ((slot-value ,journal 'lock))
     ,@body))


(defsection @comparing-journals (:title "Comparing journals")
  "After replay finished (i.e. WITH-JOURNALING completed), we can ask
  the question whether there were any changes produced. This is
  answered in the strictest sense by IDENTICAL-JOURNALS-P, and
  somewhat more functionally by EQUIVALENT-REPLAY-JOURNALS-P.

  Also see JOURNAL-DIVERGENT-P."
  (identical-journals-p generic-function)
  (equivalent-replay-journals-p generic-function))

(defgeneric identical-journals-p (journal-1 journal-2)
  (:documentation "Compare two journals in a strict sense: whether
  they have the same JOURNAL-STATE and the lists of their events (as
  in LIST-EVENTS) are EQUAL.")
  (:method :around (journal-1 journal-2)
    (and (eq (journal-state journal-1) (journal-state journal-2))
         (call-next-method)))
  ;; Generic, inefficient version based on LIST-EVENTS.
  (:method (journal-1 journal-2)
    (let ((events-1 (list-events journal-1))
          (events-2 (list-events journal-2)))
      (and (= (length events-1) (length events-2))
           (equal events-1 events-2)))))

(defun equivalent-states-p (state-1 state-2)
  (let ((will-be-completed-1 (member state-1
                                     '(:recording :logging :completed)))
        (will-be-completed-2 (member state-2
                                     '(:recording :logging :completed))))
    (eq (not (not will-be-completed-1))
        (not (not will-be-completed-2)))))

(defgeneric equivalent-replay-journals-p (journal-1 journal-2)
  (:documentation "See if two journals are equivalent when used the
  for REPLAY in WITH-JOURNALING. EQUIVALENT-REPLAY-JOURNALS-P is like
  IDENTICAL-JOURNALS-P, but it ignores LOG-EVENTs and allows events
  with EVENT-EXIT :ERROR to differ in their outcomes, which may very
  well be implementation specific, anyway. Also, it considers two
  groups of states as different :NEW, :REPLAYING, :MISMATCHED, :FAILED
  vs :RECORDING, :LOGGING, COMPLETED.")
  (:method :around (journal-1 journal-2)
    (and (equivalent-states-p (journal-state journal-1)
                              (journal-state journal-2))
         (call-next-method)))
  ;; Generic, inefficient version based on LIST-EVENTS.
  (:method (journal-1 journal-2)
    (let ((events-1 (remove-if #'log-event-p (list-events journal-1)))
          (events-2 (remove-if #'log-event-p (list-events journal-2))))
      (and (= (length events-1) (length events-2))
           (every #'event= events-1 events-2)))))


(defsection @streamlets-reference (:title "Streamlets reference")
  "This section is relevant mostly for implementing new kinds of
  JOURNALs in addition to FILE-JOURNALs and IN-MEMORY-JOURNALs. In
  normal operation, STREAMLETs are not worked with directly."
  (@opening-and-closing section)
  (@reading-from-streamlets section)
  (@writing-to-streamlets section))

(defsection @opening-and-closing (:title "Opening and closing")
  (streamlet class)
  (journal (reader streamlet))
  (open-streamlet generic-function)
  (close-streamlet generic-function)
  (make-streamlet-finalizer generic-function)
  (open-streamlet-p generic-function)
  (input-streamlet-p function)
  (output-streamlet-p function)
  (with-open-journal macro)
  (streamlet-error condition))

(defclass streamlet ()
  ((journal
    :initarg :journal :reader journal
    :documentation "The JOURNAL that was passed to OPEN-STREAMLET.
    This is the journal [STREAMLET][dislocated] operates on.")
   ;; This is set to NIL when the streamlet is closed.
   (direction :initarg :direction :type (or direction null)
              :accessor %direction)
   ;; N-IN-EVENTS - N-OUT-EVENTS read from the streamlet. Not
   ;; meaningful if the read position is altered manually (i.e. not
   ;; via SAVE-EXCURSION).
   (depth :initform 0 :accessor %depth)
   ;; Same as this streamlet's JOURNAL's SYNC.
   (sync :initform t :initarg :sync)
   ;; Suppresses checks for DIRECTION and being open.
   (%trusted :initform nil)
   (completed-on-abort-deferred-p))
  (:documentation "A STREAMLET is a handle to perform I/O on a
  JOURNAL. The high-level stuff (WITH-JOURNALING, JOURNALED, etc) is
  built on top of streamlets."))

(defmethod print-object ((streamlet streamlet) stream)
  (print-unreadable-object (streamlet stream :type t)
    (format stream "~S ~S ~S" :direction (ignore-errors (%direction streamlet))
            (ignore-errors (journal streamlet)))))

(deftype direction ()
  '(member :input :output :io))

(defun input-direction-p (direction)
  (or (eq direction :io) (eq direction :input)))

(defun input-streamlet-p (streamlet)
  "See if STREAMLET was opened for input (the DIRECTION argument of
  OPEN-STREAMLET was :INPUT or :IO)."
  (input-direction-p (%direction streamlet)))

(defun output-direction-p (direction)
  (or (eq direction :io) (eq direction :output)))

(defun output-streamlet-p (streamlet)
  "See if STREAMLET was opened for input (the DIRECTION argument of
  OPEN-STREAMLET was :OUTPUT or :IO)."
  (output-direction-p (%direction streamlet)))

(defgeneric open-streamlet (journal &key direction)
  (:documentation "Return a STREAMLET suitable for performing I/O on
  JOURNAL. DIRECTION (defaults to :INPUT) is one of :INPUT, :OUTPUT,
  :IO and it has the same purpose as the similarly named argument of
  CL:OPEN.")
  (:method :around (journal &key (direction :input))
    (let ((outputp (output-direction-p direction)))
      (with-journal-locked (journal)
        (when outputp
          (check-concurrent-write-access journal))
        (multiple-value-prog1
            (call-next-method journal :direction direction)
          (if outputp
              (incf (slot-value journal 'n-writers))
              (incf (slot-value journal 'n-readers))))))))

(defun check-concurrent-write-access (journal)
  (with-slots (n-writers) journal
    (when (eq n-writers :invalidated)
      (error 'journal-error :journal journal
             :format-control "~@<Attempt to write ~S whose file was ~
                             unlinked.~:@>"
             :format-args (list journal)))
    (when (plusp n-writers)
      (error 'journal-error :journal journal
             :format-control "~@<Concurrent write access detected on ~S.~:@>"
             :format-args (list journal)))))

;;; Prevent further writes. Called when a FILE-JOURNAL needs to be
;;; recreated because its file was deleted.
(defun invalidate-journal (journal)
  (with-journal-locked (journal)
    (check-concurrent-write-access journal)
    (setf (slot-value journal 'n-writers) :invalidated)))

(defun check-open-streamlet-p (streamlet)
  (unless (open-streamlet-p streamlet)
    (error 'streamlet-error :streamlet streamlet
           :format-control "~@<~A is closed.~:@>"
           :format-args (list streamlet))))

(defgeneric close-streamlet (streamlet)
  (:documentation "Close STREAMLET, which was returned by
  OPEN-STREAMLET. After closing, STREAMLET may not longer be used for
  IO.")
  (:method :around ((streamlet streamlet))
    (let ((journal (journal streamlet)))
      (with-journal-locked (journal)
        (when (open-streamlet-p streamlet)
          (if (output-direction-p (%direction streamlet))
              (decf (slot-value journal 'n-writers))
              (decf (slot-value journal 'n-readers)))
          (setf (%direction streamlet) nil)
          (call-next-method))))))

(defgeneric make-streamlet-finalizer (streamlet)
  (:documentation "Return NIL or a function of no arguments suitable
  as a finalizer for STREAMLET. That is, the function closes
  STREAMLET, but holds no reference to it. This is intended for
  streamlets which are not dynamic-extent, so using WITH-OPEN-JOURNAL
  is not appropriate."))

(defgeneric open-streamlet-p (streamlet)
  (:documentation "Return true if STREAMLET is open. STREAMLETs are
  open until they have been explicitly closed with CLOSE-STREAMLET.")
  (:method ((streamlet streamlet))
    (not (null (%direction streamlet)))))

(defmacro with-open-journal ((var journal &key (direction :input))
                             &body body)
  "This is like WITH-OPEN-FILE. Open the journal designated by
  JOURNAL (see TO-JOURNAL) with OPEN-STREAMLET, passing DIRECTION
  along, and bind VAR to the resulting STREAMLET. Call CLOSE-STREAMLET
  after BODY finishes. If JOURNAL is NIL, then VAR is bound to NIL and
  no streamlet is created."
  (alexandria:once-only (journal)
    ;; Use a gensym to remember to the streamlet. VAR may be changed
    ;; by BODY.
    (alexandria:with-gensyms (streamlet)
      `(let* ((,streamlet (if ,journal
                              (open-streamlet (to-journal ,journal)
                                              :direction ,direction)
                              nil))
              (,var ,streamlet))
         ;; KLUDGE: The proper solution is to parse BODY for DECLARE
         ;; expressions.
         (declare (ignorable ,var))
         (unwind-protect*
             (progn ,@body)
           (when ,streamlet
             (close-streamlet ,streamlet)))))))

(defun call-with-open-journal (journal direction fn)
  (with-open-journal (streamlet journal :direction direction)
    (funcall fn streamlet)))

(define-condition streamlet-error (error)
  ((streamlet :initarg :streamlet :reader streamlet)
   (format-control :initarg :format-control :reader format-control)
   (format-args :initform () :initarg :format-args :reader format-args))
  (:documentation "Like CL:STREAM-ERROR: failures regarding trying to
  perform I/O on a closed STREAMLET or of the wrong DIRECTION. Actual
  I/O errors are _not_ encapsulated in STREAMLET-ERROR.")
  (:report (lambda (condition stream)
             (apply #'format stream (format-control condition)
                    (format-args condition)))))

(defun check-input-streamlet-p (streamlet)
  (unless (input-streamlet-p streamlet)
    (error 'streamlet-error :streamlet streamlet
           :format-control "~@<~A is not an input streamlet.~:@>"
           :format-args (list streamlet))))

(defun check-output-streamlet-p (streamlet)
  (unless (output-streamlet-p streamlet)
    (error 'streamlet-error :streamlet streamlet
           :format-control "~@<~A is not an output streamlet.~:@>"
           :format-args (list streamlet))))


(defsection @reading-from-streamlets (:title "Reading from streamlets")
  (read-event generic-function)
  (read-position generic-function)
  (set-read-position generic-function)
  (save-excursion macro)
  (peek-event generic-function)
  (peek-event (method () (streamlet))))

(defgeneric read-event (streamlet &optional eoj-error-p)
  (:documentation "Read the event at the current read position from
  STREAMLET and move the read position to the event after. If there
  are no more events, signal END-OF-JOURNAL or return NIL depending on
  EOJ-ERROR-P. Signals STREAMLET-ERROR if STREAMLET is not
  INPUT-STREAMLET-P or not OPEN-STREAMLET-P.")
  (:method :around ((streamlet streamlet) &optional eoj-error-p)
    (unless (slot-value streamlet '%trusted)
      (check-input-streamlet-p streamlet)
      (check-open-streamlet-p streamlet))
    (check-okay-for-input streamlet)
    (let ((event (call-next-method streamlet eoj-error-p)))
      (with-slots (depth) streamlet
        (cond ((in-event-p event) (incf depth))
              ((out-event-p event) (decf depth))))
      event)))

;;; Although we rely on READ-POSITION (and FILE-POSITION behind the
;;; scenes), implementing socket-based journals is possible with the
;;; introduction of a WITH-RANDOM-ACCESS macro.
(defgeneric read-position (streamlet)
  (:documentation "Return an integer that identifies the position of
  the next event to be read from STREAMLET. `SETF`able, see
  SET-READ-POSITION."))

(defgeneric set-read-position (streamlet position)
  (:documentation "Set the read position of STREAMLET to POSITION,
  which must have been acquired from READ-POSITION."))

(defsetf read-position set-read-position)

(defmacro save-excursion ((streamlet) &body body)
  "Save READ-POSITION of STREAMLET, execute BODY, and make sure to
  restore the saved read position."
  (alexandria:once-only (streamlet)
    (alexandria:with-gensyms (read-position depth)
      `(let ((,read-position (read-position ,streamlet))
             (,depth (%depth ,streamlet)))
         (unwind-protect
              (progn ,@body)
           (setf (read-position ,streamlet) ,read-position)
           (setf (%depth ,streamlet) ,depth))))))

(defgeneric peek-event (streamlet)
  (:documentation "Read the next event from STREAMLET without changing
  the read position, or return NIL if there is no event to be read.")
  (:method ((streamlet streamlet))
    "This is a slow default implementation, which relies on
    SAVE-EXCURSION and READ-EVENT."
    (save-excursion (streamlet)
      (read-event streamlet nil))))


(defsection @writing-to-streamlets (:title "Writing to streamlets")
  (write-event generic-function)
  (write-event (method () (t journal)))
  (write-position generic-function)
  (request-completed-on-abort generic-function)
  (sync-streamlet generic-function))

(defgeneric write-event (event streamlet)
  (:documentation "Write EVENT to STREAMLET. Writing always happens at
  the end of STREAMLET's journal regardless of the READ-POSITION and
  the read position is not changed. Signals STREAMLET-ERROR if
  STREAMLET is not OUTPUT-STREAMLET-P or not OPEN-STREAMLET-P.")
  (:method :around (event (streamlet streamlet))
    (unless (slot-value streamlet '%trusted)
      (check-output-streamlet-p streamlet)
      (check-open-streamlet-p streamlet))
    (check-okay-for-output streamlet)
    (call-next-method)
    event)
  (:method (event (journal journal))
    "For convenience, it is possible to write directly to a JOURNAL,
    in which case the journal's internal output streamlet is used.
    This internal streamlet is opened for :OUTPUT and may be used by
    LOG-RECORD."
    (with-journal-locked (journal)
      ;; JOURNAL-OUTPUT-STREAMLET creates the streamlet within the
      ;; lock. Good, logging via LOG-RECORD is thread-safe with
      ;; respect to other LOG-RECORDs.
      (write-event event (journal-output-streamlet journal)))))

(defun journal-output-streamlet (journal)
  (or (%output-streamlet-of journal)
      (setf (%output-streamlet-of journal)
            (let* ((streamlet (open-streamlet journal :direction :output))
                   (finalizer (make-streamlet-finalizer streamlet)))
              (when finalizer
                (trivial-garbage:finalize streamlet finalizer))
              streamlet))))

(defgeneric write-position (streamlet)
  (:documentation "Return an integer that identifies the position of
  the next event to be written to STREAMLET.")
  (:method :around ((streamlet streamlet))
    (unless (slot-value streamlet '%trusted)
      (check-output-streamlet-p streamlet)
      (check-open-streamlet-p streamlet))
    (check-okay-for-output streamlet)
    (call-next-method)))

(defgeneric request-completed-on-abort (streamlet)
  (:documentation "Make it so that upon ABORTED-EXECUTION STREAMLET's
  JOURNAL will be in JOURNAL-STATE :COMPLETED when loaded fresh (e.g.
  FILE-JOURNAL from a file). Any previously written events must be
  persisted before making this change. Before
  REQUEST-COMPLETED-ON-ABORT is called, a journal must be reloaded in
  state :FAILED.

  It is permissible to defer carrying out this request until the next
  SYNC-STREAMLET call. If the request was carried out, return true. If
  it was deferred, return NIL.")
  (:method :around ((streamlet streamlet))
    (assert (not (slot-boundp streamlet 'completed-on-abort-deferred-p)))
    (setf (slot-value streamlet 'completed-on-abort-deferred-p)
          (not (call-next-method)))))

(defgeneric sync-streamlet (streamlet)
  (:documentation "Durably persist the effects of all preceding
  WRITE-EVENT calls made via STREAMLET to its journal and any deferred
  REQUEST-COMPLETED-ON-ABORT in this order.")
  (:method :around ((streamlet streamlet))
    (check-okay-for-output streamlet)
    (call-next-method)))


(defsection @journal-basics (:title "Basics")
  "The JOURNALED macro does both recording and replaying of events,
  possibly at the same time. Recording is easy: events generated by
  JOURNALED are simply written to a journal, which is a sequence of
  events much like a file. What events are generated is described in
  JOURNALED. @REPLAY is much more involved, thus it gets its own
  section. The journals used for recording and replaying are specified
  by WITH-JOURNALING or by WITH-BUNDLE.

  The @JOURNALS-REFERENCE is presented later, but for most purposes
  creating them (e.g. with MAKE-IN-MEMORY-JOURNAL, MAKE-FILE-JOURNAL)
  and maybe querying their contents with LIST-EVENTS will suffice.
  Some common cases of journal creation are handled by the convenience
  function TO-JOURNAL.

  Built on top of journals, @BUNDLES juggle repeated replay-and-record
  cycles focussing on persistence."
  (to-journal generic-function)
  (with-journaling macro)
  (@block glossary-term)
  (@frame glossary-term)
  (record-journal function)
  (replay-journal function)
  (journaled macro)
  (@in-events section)
  (@out-events section)
  (@working-with-unreadable-values section)
  (@journal-utilities section)
  (@pretty-printing section)
  (@journal-error-handling section))

(defgeneric to-journal (designator)
  (:documentation "Return the journal designated by DESIGNATOR or
  signal an error. The default implementation:

  - returns DESIGNATOR itself if it is of type JOURNAL,
  - returns a new IN-MEMORY-JOURNAL if DESIGNATOR is T,
  - returns a new FILE-JOURNAL if DESIGNATOR is a PATHNAME.")
  (:method ((designator journal))
    designator)
  (:method ((designator (eql t)))
    (make-in-memory-journal))
  (:method ((designator pathname))
    (make-file-journal designator))
  (:method (designator)
    (error "~@<~S is not a journal designator.~:@>" designator)))

(defmacro with-journaling ((&key record replay replay-eoj-error-p)
                           &body body)
  "Turn recording and/or replaying of events on or off for the
  duration of BODY. Both RECORD and REPLAY should be a JOURNAL
  designator (in the sense of TO-JOURNAL) or NIL.

  If RECORD designates a JOURNAL, then events generated by enclosed
  JOURNALED @BLOCKs are written to that journal (with exceptions, see
  the LOG-RECORD argument of JOURNALED). If REPLAY designates a
  JOURNAL, then the generated events are matched against events from
  that journal according to the rules of @REPLAY.

  A JOURNAL-ERROR is signalled, if RECORD is a JOURNAL that has been
  previously recorded to by another WITH-JOURNALING (that is, if its
  JOURNAL-STATE is not :NEW), or if REPLAY is a JOURNAL that is not a
  complete recording of successful replay (i.e. its JOURNAL-STATE is
  not :COMPLETED). These checks are intended to catch mistakes that
  would render the new or existing records unusable for replay. When
  WITH-JOURNALING finishes, the RECORD journal is marked :COMPLETED or
  :FAILED in its JOURNAL-STATE.

  REPLAY-EOJ-ERROR-P controls whether an END-OF-JOURNAL is signalled
  when a new event is being matched to the replay journal from which
  there are no more events to read. If there was a JOURNALING-FAILURE
  or a REPLAY-FAILURE during execution, then END-OF-JOURNAL is not
  signalled.

  If BODY completes successfully, but REPLAY has unprocessed events,
  then signal REPLAY-INCOMPLETE.

  WITH-JOURNALING for different RECORD journals can be nested and run
  independently."
  (alexandria:with-gensyms (with-journaling-body)
    `(flet ((,with-journaling-body () ,@body))
       (declare (dynamic-extent #',with-journaling-body)
                (inline ,with-journaling-body))
       (call-with-journaling #',with-journaling-body ,record ,replay
                             ,replay-eoj-error-p))))

(define-glossary-term @block (:title "block")
  "A journaled block, or simply block, is a number of forms wrapped in
  JOURNALED. When a block is executed, a @FRAME is created.")

(define-glossary-term @frame (:title "frame")
  "A frame is an IN-EVENT, OUT-EVENT pair, which are created when a
  @BLOCK is entered and left, respectively.")

;;; A STREAMLET is to a JOURNAL what a CL:STREAM is to a file.
;;; Streamlets are part of the low-level interface and are never
;;; exposed directly.
(defvar *record-streamlet* nil)
(defvar *replay-streamlet* nil)
(defvar *replay-eoj-error-p* nil)
#+sbcl
(declaim (sb-ext:always-bound *record-streamlet* *replay-streamlet*
                              *replay-eoj-error-p*))

(defun replay-journal ()
  "Return the journal from which events are currently being
  replayed (see WITH-JOURNALING and WITH-BUNDLE) or NIL."
  (when *replay-streamlet*
    (journal *replay-streamlet*)))

(defun record-journal ()
  "Return the journal in which events are currently being
  recorded (see WITH-JOURNALING and WITH-BUNDLE) or NIL."
  (when *record-streamlet*
    (journal *record-streamlet*)))

;;; To avoid (JOURNAL-STATE (JOURNAL *RECORD-STREAMLET*)), which is a
;;; hotspot. Updated by SET-JOURNAL-STATE.
(defvar *record-journal-state* nil)
(defvar *replay-failure* nil)
#+sbcl
(declaim (sb-ext:always-bound *replay-failure* *record-journal-state*))

(defvar *journaling-failure*)

(declaim (inline in-with-journaling-p))
(defun in-with-journaling-p ()
  (boundp '*journaling-failure*))

(defun set-journal-state (streamlet state)
  (check-okay-for-output streamlet)
  (let ((current-state (journal-state (journal streamlet))))
    (unless (eq state current-state)
      (when (eq current-state :new)
        (assert (eq state :replaying)))
      (when (eq current-state :replaying)
        (assert (member state '(:mismatched :failed :recording))))
      (when (eq current-state :mismatched)
        (assert (eq state :failed)))
      (when (eq current-state :recording)
        (assert (member state '(:logging :completed))))
      (when (eq current-state :logging)
        (assert (eq state :completed)))
      (setf (%state (journal streamlet)) state)
      (when (eq streamlet *record-streamlet*)
        (setq *record-journal-state* state))
      (when (eq state :recording)
        (request-completed-on-abort streamlet)))))

;;; An override mechanism for SKIP-LOG-EVENTS for WITH-REPLAY-FILTER.
(defvar *skip-events* nil)
;;; Infinite recursion protection for DEFINE-INVOKED, when triggered
;;; from SKIP-EVENTS.
(defvar *skipped-events-until* nil)

(defun call-with-journaling (fn record replay replay-eoj-error-p)
  (let ((record (and record (to-journal record)))
        (replay (and replay (to-journal replay)))
        (*journaling-failure* nil)
        (*replay-failure* nil)
        (*record-journal-state* :new)
        (*skip-events* nil)
        (*skipped-events-until* nil))
    (check-journal-state "Record" record :new)
    (check-journal-state "Replay" replay :completed)
    (with-open-journal (*record-streamlet* record :direction :output)
      (with-open-journal (*replay-streamlet* replay)
        ;; *RECORD-STREAMLET* and *REPLAY-STREAMLET* are internal to
        ;; this library. No need to check whether they are open and
        ;; have the right DIRECTION.
        (when *record-streamlet*
          (setf (slot-value *record-streamlet* '%trusted) t))
        (when *replay-streamlet*
          (setf (slot-value *replay-streamlet* '%trusted) t))
        (let ((*replay-eoj-error-p* replay-eoj-error-p)
              (completedp nil))
          (initialize-journal-state *record-streamlet* *replay-streamlet*)
          (unwind-protect*
              (multiple-value-prog1 (funcall fn)
                (setq completedp t))
            ;; If there was a JOURNAL-FAILURE, we must have tried to
            ;; finalize the state already. Let's not try again.
            (unless *journaling-failure*
              (call-with-journaling-failure-on-nlx
               (lambda ()
                 (finalize-journal-state *record-streamlet* nil))))
            ;; Signal REPLAY-INCOMPLETE if completed normally, there
            ;; was no failure reported so far and replay is actually
            ;; incomplete.
            (when (and completedp
                       (null *journaling-failure*)
                       (null *replay-failure*)
                       *replay-streamlet*
                       (peek-event *replay-streamlet*))
              (let ((replay-event (peek-event *replay-streamlet*)))
                (assert (not (log-event-p replay-event)))
                (replay-failure 'replay-incomplete
                                :new-event nil
                                :replay-event replay-event)))))))))

(defun check-journal-state (name journal expected-state)
  (when journal
    (unless (eq (journal-state journal) expected-state)
      (error 'journal-error :journal journal
             :format-control "~@<~A ~S is not in state ~S.~:@>"
             :format-args (list name journal expected-state)))))

(defun initialize-journal-state (record-streamlet replay-streamlet)
  (call-with-journaling-failure-on-nlx
   (lambda ()
     (when record-streamlet
       (set-journal-state record-streamlet :replaying))
     ;; This is to set record journal state to :RECORDING if there is
     ;; no replay journal or it has only log events. Also, this sets
     ;; up the invariant which is that the REPLAY-EVENT is
     ;; PEEK-REPLAY-EVENT.
     (skip-events-and-maybe->recording record-streamlet replay-streamlet))))

(defmacro nlx-protect ((&key on-return on-nlx) &body body)
  (alexandria:with-gensyms (values completep condition last-condition)
    `(let ((,values nil)
           (,completep nil)
           (,last-condition nil))
       (unwind-protect
            (handler-bind ((serious-condition
                             (lambda (,condition)
                               (setq ,last-condition ,condition))))
              (setq ,values (multiple-value-list (progn ,@body)))
              (setq ,completep t)
              (values-list ,values))
         (if ,completep
             ,(when on-return `(funcall ,on-return ,values))
             ,(when on-nlx `(funcall ,on-nlx ,last-condition)))))))

(defun finalize-journal-state (record-streamlet abort)
  (when record-streamlet
    (let* ((journal (journal record-streamlet))
           (state (journal-state journal))
           (coa-requested (slot-boundp record-streamlet
                                       'completed-on-abort-deferred-p))
           (coa-deferred (and coa-requested
                              (slot-value record-streamlet
                                          'completed-on-abort-deferred-p))))
      (if coa-requested
          (assert (member state '(:recording :logging :completed)))
          (assert (member state '(:new :replaying :mismatched :failed))))
      (flet ((fail (&optional values)
               (declare (ignore values))
               (set-journal-state record-streamlet :failed))
             (complete (&optional values)
               (declare (ignore values))
               (set-journal-state record-streamlet :completed))
             (sync ()
               (sync-streamlet record-streamlet)))
        (cond ((or (eq state :failed) (eq state :completed))
               nil)
              ((not (journal-sync journal))
               (assert (not coa-deferred))
               (ecase state
                 ((:new :replaying :mismatched) (fail))
                 ((:recording :logging) (complete))))
              ;; If we haven't reached :RECORDING ...
              ((not coa-requested)
               (fail)
               (unless abort
                 (sync)))
              ;; We have reached :RECORDING, but it wasn't persisted.
              ((and coa-requested coa-deferred)
               (if abort
                   (fail)
                   (nlx-protect (:on-return #'complete :on-nlx #'fail)
                     (sync))))
              ;; We have reached :RECORDING, and it was persisted.
              ((and coa-requested (not coa-deferred))
               (complete)
               ;; @SYNCHRONIZATION-STRATEGIES mandates sync till the
               ;; end of the journal, but as this may fail most
               ;; easily, do it after SET-JOURNAL-STATE. Even if it
               ;; does fail, COA implies that its state will be
               ;; :COMPLETED upon reading it back.
               (unless abort
                 (sync)))
              (t
               (assert nil)))))))

(defmacro journaled
    ((name &key (log-record :record) version args values condition
      insertable replay-values replay-condition)
     &body body)
  "JOURNALED generates events upon entering and leaving the dynamic
  extent of BODY (also known as the journaled @BLOCK), which we call
  the @IN-EVENTS and @OUT-EVENTS. Between generating the two events,
  BODY is typically executed normally (except for
  @REPLAYING-THE-OUTCOME).

  Where the generated events are written is determined by the :RECORD
  argument of the enclosing WITH-JOURNALING. If there is no enclosing
  WITH-JOURNALING and LOG-RECORD is NIL, then event recording is
  turned off and JOURNALED imposes minimal overhead.

  - NAME can be of any type except NULL, not evaluated. For names, and
    for anything that gets written to a journal, a non-keyword symbol
    is a reasonable choice as it can be easily made unique. However,
    it also exposes the package structure, which might make reading
    stuff back more difficult. Keywords and strings do not have this
    problem.

  - ARGS can be of any type, but is typically a list.

  Also see in @LOG-RECORD in the @LOGGING section. For a description
  of VERSION, INSERTABLE, REPLAY-VALUES and REPLAY-CONDITION, see
  @JOURNALED-FOR-REPLAY."
  (check-type name (not null))
  (alexandria:once-only (log-record version)
    (alexandria:with-gensyms (%log-record)
      (let ((journaled-body (gensym (format nil "~S-JOURNALED-BLOCK" name))))
        `(let ((,%log-record (and (null ,version) ,log-record
                                  (resolve-log-record ,log-record))))
           (flet ((,journaled-body () ,@body))
             (declare (dynamic-extent #',journaled-body)
                      (inline ,journaled-body))
             (if (or *record-streamlet* *replay-streamlet* ,%log-record)
                 (call-journaled
                  #',journaled-body
                  *record-streamlet* ,%log-record
                  ',name ,version ,args ,values ,condition
                  *replay-streamlet* ,insertable ,replay-values
                  ,replay-condition *replay-eoj-error-p*)
                 (,journaled-body))))))))



(defsection @in-events (:title "In-events")
  """Upon entering a @BLOCK, JOURNALED generates an IN-EVENT,
  which conceptually opens a new @FRAME. These in-events are created
  from the NAME, VERSION and ARGS arguments of JOURNALED. For example,

  ```
  (journaled (name :version version :args args) ...)
  ```

  creates an event like this:

  ```
  `(:in ,name :version ,version :args ,args)
  ```

  where :VERSION and :ARGS may be omitted if they are NIL. Versions
  are used for @REPLAY.
  """)


(defsection @out-events (:title "Out-events")
  """Upon leaving a @BLOCK, JOURNALED generates and OUT-EVENT, closing
  the @FRAME opened by the corresponding in-event. These out-events
  are property lists like this:

  ```
  (:out foo :version 1 :values (42))
  ```

  Their NAME and VERSION (`FOO` and `1` in the example) are the same
  as in the in-event: they are the corresponding arguments of
  JOURNALED. EXIT and OUTCOME are filled in differently depending on
  how the block finished its execution.
  """
  (event-exit type)
  (values-outcome glossary-term)
  (condition-outcome glossary-term)
  (error-outcome glossary-term)
  (nlx-outcome glossary-term)
  "There is a further grouping of outcomes into expected and unexpected."
  (expected-outcome glossary-term)
  (unexpected-outcome glossary-term))

(deftype event-exit ()
  "One of :VALUES, :CONDITION, :ERROR and :NLX. Indicates whether a
  journaled @BLOCK:

  - returned normally (:VALUES, see VALUES-OUTCOME),

  - unwound on an expected condition (:CONDITION, see CONDITION-OUTCOME),

  - unwound on an unexpected condition (:ERROR, see ERROR-OUTCOME),

  - unwound by performing a @NON-LOCAL-EXIT of some other kind such as
    a throw (:NLX, see NLX-OUTCOME).

  The first two are EXPECTED-OUTCOMEs, while the latter two are
  UNEXPECTED-OUTCOMEs."
  '(member :values :condition :error :nlx))

(define-glossary-term values-outcome (:title "values outcome")
  """If the JOURNALED @BLOCK returns normally, [EVENT-EXIT][type] is
  :VALUES and the outcome is the list of values returned:

  ```
  (journaled (foo) (values 7 t))
  ;; generates the out-event
  (:out foo :values (7 t))
  ```

  The list of return values of the block is transformed by the VALUES
  argument of JOURNALED, whose default is `#'IDENTITY`. Also see
  @WORKING-WITH-UNREADABLE-VALUES).
  """)

(define-glossary-term condition-outcome (:title "condition outcome")
  """If the @BLOCK unwound due to a condition, and JOURNALED's
  CONDITION argument (a function whose default is `(CONSTANTLY NIL)`)
  returns non-NIL when invoked on it, then [EVENT-EXIT][type] is
  :CONDITION and the outcome is this return value:

  ```
  (journaled (foo :condition (lambda (c) (prin1-to-string c)))
    (error "xxx"))
  ;; generates the out-event
  (:out foo :condition "xxx")
  ```

  Conditions thus recognized are those that can be considered to be
  part of normal execution. Just like return values, these expected
  conditions may be required to match what's in the replay journal.
  Furthermore, given a suitable REPLAY-CONDITION in JOURNALED, they
  may be replayed without running the @BLOCK.""")

(define-glossary-term error-outcome (:title "error outcome")
  """If the JOURNALED @BLOCK unwound due to a condition, but
  JOURNALED's CONDITION argument returns NIL when invoked on it, then
  [EVENT-EXIT][type] is :ERROR and the outcome the string
  representations of the type of the condition and the condition
  itself.

  ```
  (journaled (foo)
    (error "xxx"))
  ;; generates the out-event
  (:out foo :error ("simple-error" "xxx"))
  ```

  The conversion to string is performed with PRINC in
  WITH-STANDARD-IO-SYNTAX. This scheme is intended to avoid leaking
  random implementation details into the journal, which would make
  `READ`ing it back difficult.

  In contrast with CONDITION-OUTCOMEs, error outcomes are what the
  code is not prepared to handle or replay in a meaningful way.""")

(define-glossary-term nlx-outcome (:title "nlx outcome")
  """If the JOURNALED @BLOCK performed a @NON-LOCAL-EXIT that was not
  due to a condition, then [EVENT-EXIT][type] is :NLX and the outcome
  is NIL.

  ```
  (catch 'xxx
    (journaled (foo)
      (throw 'xxx nil)))
  ;; generates the out-event
  (:out foo :nlx nil)
  ```

  Note that CONDITION-OUTCOMEs and ERROR-OUTCOMEs are also due to
  @NON-LOCAL-EXITs, but are distinct from nlx outcomes.

  Currently, nlx outcomes are detected rather heuristically as there
  is no portable way to detect what really caused the unwinding of the
  stack.""")

(define-glossary-term expected-outcome (:title "expected outcome")
  "An OUT-EVENT is said to have an expected outcome if it had a
  VALUES-OUTCOME or a CONDITION-OUTCOME, or equivalently, when its
  [EVENT-EXIT][type] is :VALUES or :CONDITION.")

(define-glossary-term unexpected-outcome (:title "unexpected outcome")
  "An OUT-EVENT is said to have an unexpected outcome if it had an
  ERROR-OUTCOME or an NLX-OUTCOME, or equivalently, when its
  [EVENT-EXIT][type] is :ERROR or :NLX.")


(defsection @working-with-unreadable-values
    (:title "Working with unreadable values")
  """The events recorded often need to be @READABLE. This is always
  required with FILE-JOURNALs, and often with
  @SYNCHRONIZATION-WITH-IN-MEMORY-JOURNALS. By choosing an appropriate
  identifier or string representation of the unreadable object to
  journal, this is not a problem in practice. JOURNALED provides the
  VALUES hook for this purpose.

  With EXTERNAL-EVENTs, whose outcome is replayed (see
  @REPLAYING-THE-OUTCOME), we also need to be able to reverse the
  transformation of VALUES, and this is what the REPLAY-VALUES
  argument of JOURNALED is for.

  Let's see a complete example.

  ```
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

  (defvar *user7* (add-user 7))

  (defun get-message ()
    (replayed (listen :values (values-> #'user-id)
                      :replay-values (values<- #'find-user))
      (values *user7* "hello")))

  (jtrace user-id find-user get-message)

  (let ((bundle (make-file-bundle "/tmp/user-example/")))
    (format t "Recording")
    (with-bundle (bundle)
      (get-message))
    (format t "~%Replaying")
    (with-bundle (bundle)
      (get-message)))
  .. Recording
  .. (GET-MESSAGE)
  ..   (USER-ID #<USER 7>)
  ..   => 7
  .. => #<USER 7>, "hello"
  .. Replaying
  .. (GET-MESSAGE)
  ..   (FIND-USER 7)
  ..   => #<USER 7>, T
  .. => #<USER 7>, "hello"
  ==> #<USER 7>
  => "hello"
  ```

  The point of this example that for to be able to journal the return
  values of `GET-MESSAGE`, the `USER` object must be transformed to
  something @READABLE. On the `Recording` run, `(VALUES-> #'USER-ID)`
  replaces the user object with its id in the EVENT-OUTCOME recorded,
  but the original user object is returned.

  When `Replaying`, the journaled OUT-EVENT is replayed (see
  @REPLAYING-THE-OUTCOME):

  ```
  (:OUT GET-MESSAGE :VERSION :INFINITY :VALUES (7 "hello"))
  ```

  The user object is looked up according to :REPLAY-VALUES and is
  returned along with `"hello"`.
  """
  (values-> function)
  (values<- function))

(defun values-> (&rest fns)
  """A utility to create a function suitable as the VALUES argument of
  JOURNALED. The VALUES function is called with the list of values
  returned by the @BLOCK and returns a transformed set of values that
  may be recorded in a journal. While arbitrary transformations are
  allowed, `VALUES->` handles the common case of transforming
  individual elements of the list independently by calling the
  functions in FN with the values of the list of the same position.

  ```
  (funcall (values-> #'1+) '(7 :something))
  => (8 :SOMETHING)
  ```

  Note how `#'1+` is applied only to the first element of the values
  list. The list of functions is shorter than the values list, so
  `:SOMETHING` is not transformed. A value can be left explicitly
  untransformed by specifying #'IDENTITY or NIL as the function:

  ```
  (funcall (values-> #'1+ nil #'symbol-name)
           '(7 :something :another))
  => (8 :SOMETHING "ANOTHER")
  ```
  """
  (lambda (list)
    (loop for x in list
          collect (let ((fn (pop fns)))
                    (if fn (funcall fn x) x)))))

(defun values<- (&rest fns)
  """The inverse of `VALUES->`, this returns a function suitable as
  the REPLAY-VALUES argument of JOURNALED. It does pretty much what
  `VALUES->` does, but the function returned returns the transformed
  list as multiple values instead of as a list.

  ```
  (funcall (values<- #'1-) '(8 :something))
  => 7
  => :SOMETHING
  ```
  """
  (lambda (list)
    (values-list (funcall (apply #'values-> fns) list))))


(defsection @journal-utilities (:title "Utilities")
  (list-events function)
  (events-to-frames function)
  (expected-type function))

(defun list-events (&optional (journal (record-journal)))
  "Return a list of all the events in the journal designated by
  JOURNAL. Calls SYNC-JOURNAL first, to make sure that all writes are
  taken into account."
  (let ((journal (to-journal journal)))
    (sync-journal journal)
    (call-with-open-journal journal :input
                            (lambda (streamlet)
                              (loop for event = (read-event streamlet nil)
                                    while event
                                    collect event)))))
(defun events-to-frames (events)
  """Convert a flat list of events, such as those returned by LIST-EVENTS,
  to a nested list representing the @FRAMEs. Each frame is a list of
  the form `(<in-event> <nested-frames>* <out-event>?)`. Like in
  PRINT-EVENTS, EVENTS may be a JOURNAL.

  ```
  (events-to-frames '((:in foo :args (1 2))
                      (:in bar :args (7))
                      (:leaf "leaf")
                      (:out bar :values (8))
                      (:out foo :values (2))
                      (:in foo :args (3 4))
                      (:in bar :args (8))))
  => (((:IN FOO :ARGS (1 2))
       ((:IN BAR :ARGS (7))
        (:LEAF "leaf")
        (:OUT BAR :VALUES (8)))
       (:OUT FOO :VALUES (2)))
      ((:IN FOO :ARGS (3 4)) ((:IN BAR :ARGS (8)))))
  ```

  Note that, as in the above example, incomplete frames (those without
  an OUT-EVENT) are included in the output."""
  (let ((events (if (typep events '(or journal bundle))
                    (list-events events)
                    events)))
    (labels ((to-frames ()
               (loop
                 for event = (first events)
                 while (and event (not (out-event-p event)))
                 collect (if (in-event-p event)
                             (append (list (pop events))
                                     (to-frames)
                                     ;; The out-event may be missing either
                                     ;; because it hasn't run yet or because
                                     ;; the program crashed without an
                                     ;; unwind, or the journal was
                                     ;; truncated.
                                     (let ((out (pop events)))
                                       (if out (list out) ())))
                             ;; This is a LEAF-EVENT.
                             (pop events)))))
      (to-frames))))

(defun expected-type (type)
  "Return a function suitable as the CONDITION argument of JOURNALED,
  which returns the type of its single argument as a string if it is
  of TYPE, else NIL."
  (lambda (object)
    (if (typep object type)
        (with-standard-io-syntax
          (cleanup (prin1-to-string (type-of object))))
        nil)))


(defsection @journal-error-handling (:title "Error handling")
  (journaling-failure condition)
  (journaling-failure-embedded-condition (reader journaling-failure))
  (record-unexpected-outcome condition)
  (data-event-lossage condition)
  (journal-error condition)
  (end-of-journal condition))

(define-condition journaling-failure (serious-condition)
  ((embedded-condition :initarg :embedded-condition
                       :reader journaling-failure-embedded-condition)
   (n-resignallings :initform 0 :accessor journaling-failure-n-resignallings))
  (:documentation "Signalled during the dynamic extent of
  WITH-JOURNALING when an error threatens to leave the journaling
  mechanism in an inconsistent state. These include I/O errors
  encountered reading or writing journals by WITH-JOURNALING,
  JOURNALED, LOGGED, WITH-REPLAY-FILTER, SYNC-JOURNAL, but also
  STORAGE-CONDITIONs, assertion failures, and errors calling
  JOURNALED's VALUES and CONDITION function arguments. Crucially, this
  does not apply to non-local exits from other code, such as JOURNALED
  @BLOCKs, whose error handling is largely unaltered (see @OUT-EVENTS
  and @REPLAY-FAILURES).

  In general, any @NON-LOCAL-EXIT from critical parts of the code is
  turned into a JOURNALING-FAILURE to protect the integrity of the
  RECORD-JOURNAL. The condition that caused the unwinding is in
  JOURNALING-FAILURE-EMBEDDED-CONDITION, or NIL if it was a pure
  @NON-LOCAL-EXIT like THROW. This is a SERIOUS-CONDITION, not to be
  handled within WITH-JOURNALING.

  After a JOURNALING-FAILURE, the journaling mechanism cannot be
  trusted anymore. The REPLAY-JOURNAL might have failed a read and be
  out-of-sync. The RECORD-JOURNAL may have missing events (or even
  half-written events with FILE-JOURNALs without SYNC, see
  @SYNCHRONIZATION-STRATEGIES), and further writes to it would risk
  replayability, which is equivalent to database corruption. Thus,
  upon signalling JOURNALING-FAILURE, JOURNAL-STATE is set to

  - :COMPLETED if the journal is in state :RECORDING or :LOGGING and
    the transition to :RECORDING was reflected in storage,

  - else it is set to :FAILED.

  After a JOURNALING-FAILURE, any further attempt within the affected
  WITH-JOURNALING to use the critical machinery mentioned
  above (JOURNALED, LOGGED, etc) resignals the same journal failure
  condition. As a consequence, the record journal cannot be changed
  and the only way to recover is to leave WITH-JOURNALING. This does
  not affect processing in other threads, which by design cannot write
  to the record journal.

  Note that in contrast with JOURNALING-FAILURE and REPLAY-FAILURE,
  which necessitate leaving WITH-JOURNALING to recover from, the other
  conditions - JOURNAL-ERROR, and STREAMLET-ERROR - are subclasses of
  ERROR as the their handling need not be so heavy-handed.")
  (:report (lambda (condition stream)
             (maybe-print-resignalling-message condition stream)
             (let ((embedded (journaling-failure-embedded-condition condition)))
               (if embedded
                   (format stream "~@<JOURNALED failed with \"~A\".~:@>~%"
                           embedded)
                   (format stream "~@<JOURNALED failed with an unidentified ~
                                  non-local-exit.~:@>~%")))
             (print-record-closed stream))))

(defun maybe-print-resignalling-message (condition stream)
  (when (plusp (journaling-failure-n-resignallings condition))
    (format stream "~@<Resignalling previous JOURNALING-FAILURE ~
                   upon attempting to use JOURNALED on a failed ~
                   WITH-JOURNALING record journal.~:@>~%")
    (format stream "~@<Source location of the error may be off. ~
                   Original message follows.~:@>~%~%")))

(defun print-record-closed (stream)
  (format stream "~@<Record journal was closed.~:@>"))


;;;; Longish intermezzo: WITH-JOURNALING-FAILURE-ON-NLX

(declaim (inline check-within-with-journaling-failure-on-nlx))
(defun check-within-with-journaling-failure-on-nlx ()
  (assert (boundp '*with-journaling-failure-on-nlx-body-completed*) ()
          "Not in WITH-JOURNALING-FAILURE-ON-NLX."))

(defun check-okay-for-input (streamlet)
  (when (eq streamlet *replay-streamlet*)
    (check-within-with-journaling-failure-on-nlx)))

(defun check-okay-for-output (streamlet)
  (when (eq streamlet *record-streamlet*)
    (check-within-with-journaling-failure-on-nlx)))

;;; Signal JOURNALING-FAILURE and if within WITH-JOURNALING remember it
;;; for resignalling.
(defun journaling-failure (condition-type &rest args)
  (check-within-with-journaling-failure-on-nlx)
  (cond ((in-with-journaling-p)
         (assert (null *journaling-failure*))
         (setq *journaling-failure*
               (apply #'make-condition condition-type args))
         (error *journaling-failure*))
        (t
         (apply #'error condition-type args))))

(defun maybe-resignal-journaling-failure ()
  ;; JOURNALED is no longer functional after JOURNALING-FAILURE.
  ;; Resignal the original condition.
  (when (and (in-with-journaling-p) *journaling-failure*)
    (incf (journaling-failure-n-resignallings *journaling-failure*))
    (error *journaling-failure*)))

(defvar *with-journaling-failure-on-nlx-body-completed*)

(defun turn-off-with-journaling-failure-on-nlx ()
  (setq *with-journaling-failure-on-nlx-body-completed* t))

;;; This is wrapped around HANDLE-IN-EVENT and HANDLE-OUT-EVENT and
;;; basically wherever I/O is performed. If it catches any nastiness
;;; that threatens to corrupt the internal state, it transititions to
;;; :FAILED or :COMPLETED and signals a JOURNALING-FAILURE. There is
;;; considerable hair to this because we must recongize safe
;;; conditions signalled under normal operation and let throws to
;;; REPLAY-VALUES-HAPPENED through.
(defmacro with-journaling-failure-on-nlx (&body body)
  (alexandria:with-gensyms (condition last-condition)
    `(let ((*with-journaling-failure-on-nlx-body-completed* nil)
           (,last-condition nil))
       (maybe-resignal-journaling-failure)
       (unwind-protect*
           (handler-bind ((safe-condition
                            (lambda (,condition)
                              (declare (ignore ,condition))
                              (setq ,last-condition :safe)))
                          (unsafe-condition
                            (lambda (,condition)
                              (setq ,last-condition ,condition))))
             (multiple-value-prog1
                 (progn ,@body)
               (setq *with-journaling-failure-on-nlx-body-completed* t)))
         (unless (or *with-journaling-failure-on-nlx-body-completed*
                     (eq ,last-condition :safe))
           (fail-with-journaling-failure *record-streamlet*
                                         ,last-condition))))))

(defun call-with-journaling-failure-on-nlx (fn)
  (with-journaling-failure-on-nlx
    (funcall fn)))

;;; These are conditions we know about and if they happen in
;;; HANDLE-IN-EVENT or HANDLE-OUT-EVENT, JOURNALED remains
;;; operational.
(deftype safe-condition ()
  `(or replay-failure journal-error streamlet-error
       ;; This is not a serious condition, but one can still unwind
       ;; handling it.
       record-unexpected-outcome))

(deftype unsafe-condition ()
  `(and serious-condition
        (not replay-failure)
        (not journal-error)
        (not streamlet-error)))

(defmacro with-nlx-cancelled ((datum &rest arguments) &body body)
  (alexandria:with-gensyms (name completedp)
    `(block ,name
       (let ((,completedp nil))
         (unwind-protect
              (multiple-value-prog1 (progn,@body)
                (setq ,completedp t))
           (unless ,completedp
             (warn ,datum ,@arguments)
             (return-from ,name nil)))))))

(defun fail-with-journaling-failure (record-streamlet condition)
  ;; We are already performing an nlx in an unrecoverable situation.
  ;; There is no sane way to handle errors here.
  (with-nlx-cancelled ("Could not finalize journal state when triggering ~
                       JOURNALING-FAILURE for ~A."
                       condition)
    (finalize-journal-state record-streamlet t))
  (cond ((typep condition 'journaling-failure)
         (assert (typep condition 'data-event-lossage))
         (when (in-with-journaling-p)
           (setq *journaling-failure* condition)))
        (t
         (journaling-failure 'journaling-failure
                             :embedded-condition condition))))


;;;; Continuing @JOURNAL-ERROR-HANDLING

(define-condition record-unexpected-outcome (condition)
  ((new-event
    :initarg :new-event :reader new-event
    :documentation "The event that triggered this condition."))
  (:documentation "Signalled (with SIGNAL: this is not an ERROR) by
  JOURNALED when a VERSIONED-EVENT or an EXTERNAL-EVENT had an
  UNEXPECTED-OUTCOME while in JOURNAL-STATE :RECORDING. Upon
  signalling this condition, JOURNAL-STATE is set to :LOGGING, thus no
  more events can be recorded that will affect replay of the journal
  being recorded. The event that triggered this condition is recorded
  in state :LOGGING, with its version downgraded. Since
  @REPLAY (except INVOKED) is built on the assumption that control
  flow is deterministic, an unexpected outcome is significant because
  it makes this assumption to hold unlikely.

  Also see REPLAY-UNEXPECTED-OUTCOME.")
  (:report (lambda (condition stream)
             (format stream "~@<Recorded event ~S with an ~
                            UNEXPECTED-OUTCOME.~:@>~%"
                     (new-event condition))
             (format stream "~@<Record journal was set to ~
                            JOURNAL-STATE :LOGGING.~:@>"))))

(define-condition data-event-lossage (journaling-failure)
  ()
  (:documentation "Signalled when a DATA-EVENT is about to be recorded
  in JOURNAL-STATE :MISMATCHED or :LOGGING. Since the data event will
  not be replayed that constitutes data loss.")
  (:report (lambda (condition stream)
             (maybe-print-resignalling-message condition stream)
             (format stream "~@<A DATA-EVENT is about to be recorded ~
                            in JOURNAL-STATE :MISMATCHED or :LOGGING.~:@>~%")
             (print-record-closed stream))))

(define-condition journal-error (error)
  ((journal
    :initarg :journal :reader journal
    :documentation "The JOURNAL in question.")
   (format-control :initarg :format-control :reader format-control)
   (format-args :initform () :initarg :format-args :reader format-args))
  (:documentation "Signalled by WITH-JOURNALING, WITH-BUNDLE and by
  @LOG-RECORD. It is also signalled by the low-level streamlet
  interface (see @STREAMLETS-REFERENCE).")
  (:report (lambda (condition stream)
             (apply #'format stream (format-control condition)
                    (format-args condition)))))

(define-condition end-of-journal (journal-error)
  ((journal :initarg :journal :reader journal))
  (:documentation "This might be signalled by the replay mechanism if
  WITH-JOURNALING's REPLAY-EOJ-ERROR-P is true. Unlike
  REPLAY-FAILUREs, this does not affect JOURNAL-STATE of
  RECORD-JOURNAL. At a lower level, it is signalled by READ-EVENT upon
  reading past the end of the JOURNAL if EOJ-ERROR-P.")
  (:report (lambda (condition stream)
             (format stream "~@<Attempt to read past the end of ~S.~:@>"
                     (journal condition)))))


(defsection @pretty-printing (:title "Pretty-printing")
  (print-events function)
  (pprint-events function)
  (prettify-event function)
  """Instead of collecting events and then printing them, events can
  be pretty-printed to a stream as they generated. This is
  accomplished with @PPRINT-JOURNALS, discussed in detail later, in
  the following way:

  ```
  (let ((journal (make-pprint-journal)))
    (with-journaling (:record journal)
      (journaled (foo) "Hello")))
  ..
  .. (FOO)
  .. => "Hello"
  ```

  Note that @PPRINT-JOURNALS are not tied to WITH-JOURNALING and are
  most often used for @LOGGING and @TRACING.""")

(defun print-events (events &key stream)
  """Print EVENTS to STREAM as lists, starting a new line for each
  event and indenting them according to their nesting structure.
  EVENTS may be a JOURNAL, in which case LIST-EVENTS is called on it
  first.

  ```
  (print-events '((:in log :args ("first arg" 2))
                  (:in versioned :version 1 :args (3))
                  (:out versioned :version 1 :values (42 t))
                  (:out log :condition "a :CONDITION outcome")
                  (:in log-2)
                  (:out log-2 :nlx nil)
                  (:in external :version :infinity)
                  (:out external :version :infinity
                   :error ("ERROR" "an :ERROR outcome"))))
  ..
  .. (:IN LOG :ARGS ("first arg" 2))
  ..   (:IN VERSIONED :VERSION 1 :ARGS (3))
  ..   (:OUT VERSIONED :VERSION 1 :VALUES (42 T))
  .. (:OUT LOG :CONDITION "a :CONDITION outcome")
  .. (:IN LOG-2)
  .. (:OUT LOG-2 :NLX NIL)
  .. (:IN EXTERNAL :VERSION :INFINITY)
  .. (:OUT EXTERNAL :VERSION :INFINITY :ERROR ("ERROR" "an :ERROR outcome"))
  => ; No value
  ```
  """
  (%print-events events stream nil))

(defun pprint-events (events &key stream (prettifier 'prettify-event))
  """Like PRINT-EVENTS, but produces terser, more human readable
  output.

  ```
  (pprint-events '((:in log :args ("first arg" 2))
                   (:in versioned :version 1 :args (3))
                   (:leaf "This is a leaf, not a frame.")
                   (:out versioned :version 1 :values (42 t))
                   (:out log :condition "a :CONDITION outcome")
                   (:in log-2)
                   (:out log-2 :nlx nil)
                   (:in external :version :infinity)
                   (:out external :version :infinity
                    :error ("ERROR" "an :ERROR outcome"))))
  ..
  .. (LOG "first arg" 2)
  ..   (VERSIONED 3) v1
  ..     This is a leaf, not a frame.
  ..   => 42, T
  .. =C "a :CONDITION outcome"
  .. (LOG-2)
  .. =X
  .. (EXTERNAL) ext
  .. =E "ERROR" "an :ERROR outcome"
  => ; No value
  ```

  The function given as the PRETTIFIER argument formats individual
  events. The above output was produced with PRETTIFY-EVENT. For a
  description of PRETTIFIER's arguments see PRETTIFY-EVENT.
  """
  (%print-events events stream prettifier)
  (values))

(defun %print-events (events stream prettifier)
  (let ((journal (make-pprint-journal :pretty (not (null prettifier))
                                      :prettifier prettifier
                                      :stream (or stream *standard-output*)))
        (events (if (typep events '(or journal bundle))
                    (list-events events)
                    events)))
    (call-with-open-journal journal :output
                            (lambda (streamlet)
                              (dolist (event events)
                                (write-event event streamlet)))))
  (values))

(defun prettify-event (event depth stream)
  """Write EVENT to STREAM in a somewhat human-friendly format. This
  is the function PPRINT-JOURNAL, PPRINT-EVENTS, and @TRACING use by
  default. In addition to the basic example in PPRINT-EVENTS,
  @DECORATION on events is printed before normal, indented output like
  this:

  ```
  (pprint-events '((:leaf "About to sleep" :time "19:57:00" :function "FOO")))
  ..
  .. 19:57:00 FOO: About to sleep
  ```

  DEPTH is the nesting level of the EVENT. Top-level events have depth
  0. PRETTIFY-EVENT prints indents the output, after printing the
  decorations, by 2 spaces per depth.
  """
  (format stream "~%")
  (let ((decoratedp nil))
    (loop for (k v) on event by #'cddr
          do (unless (member k '(:in :out :leaf :name :version :args
                                 :values :condition :error :nlx))
               (when decoratedp
                 (format stream " "))
               (case k
                 ((:real-time) (format stream "#~,3F" v))
                 ((:run-time) (format stream "!~,3F" v))
                 (t (format stream "~A" v)))
               (setq decoratedp t)))
    (when decoratedp
      (format stream ": "))
    (let ((indent (make-string (* 2 depth) :initial-element #\Space)))
      (format stream  "~A" indent))
    (cond ((leaf-event-p event)
           (format stream "~A" (event-name event)))
          ((in-event-p event)
           (format stream "~S~A" (cons (event-name event) (event-args event))
                   (let ((version (event-version event)))
                     (cond ((null version) "")
                           ((eq version :infinity) " ext")
                           (t (format nil " v~A" (event-version event)))))))
          ((eq (event-exit event) :values)
           (format stream "=> ~{~S~^, ~}" (event-outcome event)))
          ((eq (event-exit event) :condition)
           (format stream "=C ~S" (event-outcome event)))
          ((eq (event-exit event) :error)
           (format stream "=E~{ ~S~}" (event-outcome event)))
          ((eq (event-exit event) :nlx)
           (format stream "=X")))))


(defsection @pprint-journals (:title "Pretty-printing journals")
  (pprint-journal class)
  (make-pprint-journal function)
  (pprint-journal-stream (accessor pprint-journal))
  (pprint-journal-pretty (accessor pprint-journal))
  (pprint-journal-prettifier (accessor pprint-journal)))

(defclass pprint-journal (journal)
  ((stream
    :initform *standard-output* :initarg :stream :type stream
    :accessor pprint-journal-stream
    :documentation "The stream where events are dumped. May be set any
    time to another STREAM.")
   (pretty
    :initform t :initarg :pretty
    :accessor pprint-journal-pretty
    :documentation "Whether to use PPRINT-JOURNAL-PRETTIFIER or write
    events in as the property lists they are. A
    @BOOLEAN-VALUED-SYMBOL.")
   (prettifier
    :initform 'prettify-event :initarg :prettifier
    :accessor pprint-journal-prettifier
    :documentation "A function like PRETTIFY-EVENT that writes its an
    event to a stream. Only used when PPRINT-JOURNAL-PRETTY, this is
    the output format customization knob. Also see @DECORATIONs."))
  (:documentation "When an event is written to a PPRINT-JOURNAL it
  writes that event to a stream in a customizable format. They are
  intended for producing prettier output for @LOGGING and @TRACING,
  but they do not support reads so they cannot be used as a
  REPLAY-JOURNAL, or in LIST-EVENTS, for example."))

(defclass pprint-streamlet (streamlet)
  (;; For indenting the events in the file.
   (depth :initform 0 :accessor %depth)))

(defun make-pprint-journal
    (&key (stream (make-synonym-stream '*standard-output*))
     (pretty t) (prettifier 'prettify-event) log-decorator)
  "Creates a PPRINT-JOURNAL."
  (make-instance 'pprint-journal :state :new :stream stream :pretty pretty
                 :prettifier prettifier :log-decorator log-decorator))

(defmethod open-streamlet ((journal pprint-journal) &key direction)
  (unless (eq direction :output)
    (error 'journal-error "PPRINT-JOURNALs do not support reads."))
  (make-instance 'pprint-streamlet :journal journal :direction direction))

(defmethod close-streamlet ((streamlet pprint-streamlet)))

(defmethod make-streamlet-finalizer ((streamlet pprint-streamlet))
  nil)

(defmethod write-event (event (streamlet pprint-streamlet))
  (let* ((journal (journal streamlet))
         (stream (slot-value journal 'stream))
         (pretty (symbol-value (slot-value (journal streamlet) 'pretty))))
    (when (out-event-p event)
      (decf (%depth streamlet)))
    (cond (pretty
           (funcall (pprint-journal-prettifier journal)
                    event (%depth streamlet) stream))
          (t
           (let ((indent (make-string (* 2 (%depth streamlet))
                                      :initial-element #\Space)))
             (format stream "~%~A" indent))
           (prin1 event stream)))
    (force-output stream)
    (when (in-event-p event)
      (incf (%depth streamlet)))))

(defmethod request-completed-on-abort ((streamlet pprint-streamlet))
  t)

(defmethod sync-streamlet ((streamlet pprint-streamlet))
  nil)


(defsection @logging (:title "Logging")
  """Imagine a utility library called glib.

  ##### Default to muffling

  ```
  (defvar *glib-log* nil)
  (defvar *patience* 1)

  (defun sl33p (seconds)
    (logged (*glib-log*) "Sleeping for ~As." seconds)
    (sleep (* *patience* seconds)))
  ```

  Glib follows the recommendation to have a special variable globally
  bound to NIL by default. The value of `*GLIB-LOG*` is the journal to
  which glib log messages will be routed. Since it's NIL, the log
  messages are muffled, and to record any log message, we need to
  change its value.

  ##### Routing logs to a journal

  Let's send the logs to a PPRINT-JOURNAL:

  ```
  (setq *glib-log* (make-pprint-journal
                    :log-decorator (make-log-decorator :time t)))
  (sl33p 0.01)
  ..
  .. 2020-08-31T12:45:23.827172+02:00: Sleeping for 0.01s.
  ```

  That's a bit too wordy. For this tutorial, let's stick to less
  verbose output:

  ```
  (setq *glib-log* (make-pprint-journal))
  (sl33p 0.01)
  ..
  .. Sleeping for 0.01s.
  ```

  ##### Capturing logs in with-journaling record

  If we were recording a journal for replay and wanted to include glib
  logs in the journal, we would do something like this:

  ```
  (with-journaling (:record t)
    (let ((*glib-log* :record))
      (sl33p 0.01)
      (journaled (non-glib-stuff :version 1)))
    (list-events))
  => ((:LEAF "Sleeping for 0.01s.")
      (:IN NON-GLIB-STUFF :VERSION 1)
      (:OUT NON-GLIB-STUFF :VERSION 1 :VALUES (NIL)))
  ```

  We could even `(SETQ *GLIB-LOG* :RECORD)` to make it so that glib
  messages are included by default in the RECORD-JOURNAL. In this
  example, the special `*GLIB-LOG*` acts like a log category for all
  the log messages of the glib library (currently one).

  ##### Rerouting a category

  Next, we route `*GLIB-LOG*` to wherever `*APP-LOG*` is pointing by
  binding `*GLIB-LOG*` _to the symbol_ `*APP-LOG*`.

  ```
  (defvar *app-log* nil)

  (let ((*glib-log* '*app-log*)
        (*app-log* (make-pprint-journal :pretty nil)))
    (sl33p 0.01))
  ..
  .. (:LEAF "Sleeping for 0.01s.")
  ```

  Note how pretty-printing was turned off and we see the LEAF-EVENT
  generated by LOGGED in its raw plist form.

  ##### Conditional routing

  Finally, to make routing decisions conditional we need to change
  `SL33P`:

  ```
  (defvar *glib-log-level* 1)

  (defun sl33p (seconds)
    (logged ((and (<= 2 *glib-log-level*) *glib-log*))
            "Sleeping for ~As." (* *patience* seconds))
    (sleep seconds))

  ;;; Check that it all works:
  (let ((*glib-log-level* 1))
    (format t "~%With log-level ~A" *glib-log-level*)
    (sl33p 0.01)
    (setq *glib-log-level* 2)
    (format t "~%With log-level ~A" *glib-log-level*)
    (sl33p 0.01))
  ..
  .. With log-level 1
  .. With log-level 2
  .. Sleeping for 0.01s.
  ```

  ##### Nested log contexts

  LOGGED is for single messages. JOURNALED can provide nested context:

  ```
  (defun callv (var value symbol &rest args)
    "Call SYMBOL-FUNCTION of SYMBOL with VAR dynamically bound to VALUE."
    (framed ("glib:callv" :log-record *glib-log*
                          :args `(,var ,value ,symbol ,@args))
      (progv (list var) (list value)
        (apply (symbol-function symbol) args))))

  (callv '*print-base* 2 'print 10)
  ..
  .. ("glib:callv" *PRINT-BASE* 2 PRINT 10)
  .. 1010 
  .. => 10
  => 10

  (let ((*glib-log-level* 2))
    (callv '*patience* 7 'sl33p 0.01))
  ..
  .. ("glib:callv" *PATIENCE* 7 SL33P 0.01)
  ..   Sleeping for 0.07s.
  .. => NIL
  ```
  """
  (@customizing-logs section)
  (@log-record section)
  (@logging-with-leaves section))


(defsection @customizing-logs (:title "Customizing logs")
  """Customizing the output format is possible if we don't necessarily
  expect to be able to read the logs back programmatically. There is
  an example in @TRACING, which is built on @PPRINT-JOURNALs.

  Here, we discuss how to make logs more informative.
  """
  (@decoration glossary-term)
  (journal-log-decorator (accessor journal))
  (make-log-decorator function))

(define-glossary-term @decoration (:title "decoration")
  "JOURNAL-LOG-DECORATOR adds additional data to LOG-EVENTs as they
  are written to the journal. This data is called decoration and it is
  to capture the context in which the event was triggered. See
  MAKE-LOG-DECORATOR for a typical example. Decorations, since they
  can be on LOG-EVENTs only, do not affect @REPLAY. Decorations are
  most often used with @PRETTY-PRINTING.")

(defun make-log-decorator (&key thread time real-time run-time)
  """Return a function suitable as JOURNAL-LOG-DECORATOR that may add
  the name of the thread, a timestamp, the internal real-time or
  run-time (both in seconds) to events. THREAD, TIME, REAL-TIME and
  RUN-TIME are @BOOLEAN-VALUED-SYMBOLs.

  ```
  (funcall (make-log-decorator :thread t :time t :real-time t :run-time t)
           (make-leaf-event :foo))
  => (:LEAF :FOO :TIME "2020-08-31T13:38:58.129178+02:00"
      :REAL-TIME 66328.82 :RUN-TIME 98.663 :THREAD "worker")
  ```
  """
  (lambda (event)
    (when (symbol-value time)
      (setq event (append event `(:time ,(local-time:format-timestring
                                          nil (local-time:now))))))
    (when (symbol-value real-time)
      (setq event (append event `(:real-time
                                  ,(/ (float (get-internal-real-time))
                                      internal-time-units-per-second)))))
    (when (symbol-value run-time)
      (setq event (append event `(:run-time
                                  ,(/ (float (get-internal-run-time))
                                      internal-time-units-per-second)))))
    (when (symbol-value thread)
      (setq event (append event `(:thread ,(bt:thread-name
                                            (bt:current-thread))))))
    event))


(defsection @log-record (:title "Log record")
  """WITH-JOURNALING and WITH-BUNDLE control replaying and recording
  within their dynamic extent, which is rather a necessity because
  @REPLAY needs to read the events in the same order as the JOURNALED
  @BLOCKs are being executed. However, LOG-EVENTs do not affect replay
  so we can allow more flexibility in routing them.

  The LOG-RECORD argument of JOURNALED and LOGGED controls where
  LOG-EVENTs are written both within WITH-JOURNALING and without. The
  algorithm to determine the target journal is this:

  1. If LOG-RECORD is :RECORD, then `(RECORD-JOURNAL)` is returned.

  2. If LOG-RECORD is NIL, then it is returned.

  3. If LOG-RECORD is a JOURNAL, then it is returned.

  4. If LOG-RECORD is symbol (other than NIL), then the SYMBOL-VALUE
     of that symbol is assigned to LOG-RECORD and we go to step 1.

  If the return value is NIL, then the event will not be written
  anywhere, else it is written to the journal returned.

  This is reminiscent of SYNONYM-STREAMs, also in that it is possible
  end up in cycles in the resolution. For this reason, the algorithm
  stop with a JOURNAL-ERROR after 100 iterations.

  ##### Interactions

  Events may be written to LOG-RECORD even without an enclosing
  WITH-JOURNALING, and it does not affect the JOURNAL-STATE. However,
  it is a JOURNAL-ERROR to write to a :COMPLETED journal (see
  JOURNAL-STATE).

  When multiple threads log to the same journal it is guaranteed that
  individual events are written atomically, but frames from different
  threads do not necessarily nest. To keep the log informative, the
  name of thread may be added to the events as @DECORATION.

  Also see notes on thread @SAFETY.""")

(defun resolve-log-record (log-record)
  (loop repeat 100 do
    (cond ((eq log-record :record)
           (return-from resolve-log-record (record-journal)))
          ((null log-record)
           (return-from resolve-log-record nil))
          ((symbolp log-record)
           (setq log-record (symbol-value log-record)))
          (t
           (return-from resolve-log-record log-record))))
  (error 'journal-error
         :format-control "LOG-RECORD chain longer than 100. Stopped at ~S."
         :format-args (list log-record)))


(defsection @logging-with-leaves (:title "Logging with leaf-events")
  (logged macro))

(defmacro logged ((&optional (log-record :record))
                  format-control &rest format-args)
  """LOGGED creates a single LEAF-EVENT whose name is the string
  constructed by FORMAT. For example:

  ```
  (with-journaling (:record t)
    (logged () "Hello, ~A." "world")
    (list-events))
  => ((:LEAF "Hello, world."))
  ```

  LEAF-EVENTs are LOG-EVENTs with no separate in- and out-events. They
  have an EVENT-NAME and no other properties. Use LOGGED for
  point-in-time textual log messages, and JOURNALED with VERSION
  NIL (i.e. FRAMED) to provide context.

  Also see @LOG-RECORD."""
  (alexandria:once-only (log-record)
    ;; This checks that ROUTE-EVENT will write the event somewhere
    ;; before making a costly call to FORMAT.
    (alexandria:once-only (log-record)
      (alexandria:with-gensyms (%log-record)
        `(let ((,%log-record (and ,log-record
                                  (resolve-log-record ,log-record))))
           (when ,%log-record
             (handle-leaf-event *record-streamlet* ,%log-record
                                (format nil ,format-control ,@format-args))
             nil))))))

(defun handle-leaf-event (record-streamlet log-record name)
  (with-journaling-failure-on-nlx
    (route-event (make-leaf-event name) record-streamlet log-record)))


(defsection @tracing (:title "Tracing")
  """JTRACE behaves similarly to CL:TRACE, but deals @NON-LOCAL-EXITs
  gracefully.

  ##### Basic tracing

  ```
  (defun foo (x)
    (sleep 0.12)
    (1+ x))

  (defun bar (x)
    (foo (+ x 2))
    (error "xxx"))

  (jtrace foo bar)

  (ignore-errors (bar 1))
  ..
  .. (BAR 1)
  ..   (FOO 3)
  ..   => 4
  .. =E "SIMPLE-ERROR" "xxx"
  ```

  ##### Log-like output

  It can also include the name of the originating thread and
  timestamps in the output:

  ```
  (let ((*trace-thread* t)
        (*trace-time* t))
    (ignore-errors (bar 1)))
  ..
  .. 2020-09-02T19:58:19.415204+02:00 worker: (BAR 1)
  .. 2020-09-02T19:58:19.415547+02:00 worker:   (FOO 3)
  .. 2020-09-02T19:58:19.535766+02:00 worker:   => 4
  .. 2020-09-02T19:58:19.535908+02:00 worker: =E "SIMPLE-ERROR" "xxx"
  ```

  ##### Profiler-like output

  ```
  (let ((*trace-real-time* t)
        (*trace-run-time* t))
    (ignore-errors (bar 1)))
  ..
  .. #16735.736 !68.368: (BAR 1)
  .. #16735.736 !68.369:   (FOO 3)
  .. #16735.857 !68.369:   => 4
  .. #16735.857 !68.369: =E "SIMPLE-ERROR" "xxx"
  ```

  ##### Customizing the content and the format

  If these options are insufficient, the content and the format of the
  trace can be customized:

  ```
  (let ((*trace-journal*
         (make-pprint-journal :pretty '*trace-pretty*
                       :prettifier (lambda (event depth stream)
                                     (format stream "~%Depth: ~A, event: ~S"
                                             depth event))
                       :stream (make-synonym-stream '*error-output*)
                       :log-decorator (lambda (event)
                                        (append event '(:custom 7))))))
    (ignore-errors (bar 1)))
  ..
  .. Depth: 0, event: (:IN BAR :ARGS (1) :CUSTOM 7)
  .. Depth: 1, event: (:IN FOO :ARGS (3) :CUSTOM 7)
  .. Depth: 1, event: (:OUT FOO :VALUES (4) :CUSTOM 7)
  .. Depth: 0, event: (:OUT BAR :ERROR ("SIMPLE-ERROR" "xxx") :CUSTOM 7)
  ```

  In the above, *TRACE-JOURNAL* was bound locally to keep the example
  from wrecking the global default, but the same effect could be
  achieved by `SETF`ing PPRINT-JOURNAL-PRETTIFIER,
  PPRINT-JOURNAL-STREAM and JOURNAL-LOG-DECORATOR.
  """
  (jtrace macro)
  (juntrace macro)
  (*trace-pretty* variable)
  (*trace-thread* variable)
  (*trace-time* variable)
  (*trace-real-time* variable)
  (*trace-run-time* variable)
  (*trace-journal* variable)
  (@journal-slime-integration section))

(defvar *trace-pretty* t
  "If *TRACE-PRETTY* is true, then JTRACE produces output like
  PPRINT-EVENTS, else it's like PRINT-EVENTS.")

(defvar *trace-thread* nil
  "Controls whether to decorate the trace with the name of the
  originating thread. See MAKE-LOG-DECORATOR.")

(defvar *trace-time* nil
  "Controls whether to decorate the trace with a timestamp. See
  MAKE-LOG-DECORATOR.")

(defvar *trace-real-time* nil
  "Controls whether to decorate the trace with the internal real-time.
  See MAKE-LOG-DECORATOR.")

(defvar *trace-run-time* nil
  "Controls whether to decorate the trace with the internal run-time.
  See MAKE-LOG-DECORATOR.")

(defvar *trace-journal*
  (make-pprint-journal :pretty '*trace-pretty*
                       :stream (make-synonym-stream '*trace-output*)
                       :log-decorator (make-log-decorator
                                       :thread '*trace-thread*
                                       :time '*trace-time*
                                       :real-time '*trace-real-time*
                                       :run-time '*trace-run-time*))
  "The JOURNAL where JTRACE writes LOG-EVENTs. By default it is a
  PPRINT-JOURNAL that sets up a SYNONYM-STREAM to *TRACE-OUTPUT* and
  sends its output there. It pays attention to *TRACE-PRETTY*, and its
  log decorator is affected by *TRACE-TIME* and *TRACE-THREAD*.
  However, by changing JOURNAL-LOG-DECORATOR and
  PPRINT-JOURNAL-PRETTIFIER content and output can be customized.")

(defvar *traced-functions*
  #+sbcl (make-hash-table :synchronized t)
  #-sbcl (make-hash-table))

(defun list-traced-functions ()
  #+sbcl
  (loop for name being each hash-key in *traced-functions*
        collect name)
  #-sbcl
  (loop for name being each hash-key in *traced-functions*
        do (unless (jtracedp name)
             (eval `(trace name)))
        collect name))

(defmacro jtrace (&rest names)
  """Like CL:TRACE, JTRACE takes a list of symbols. When functions
  denoted by those NAMES are invoked, their names, arguments and
  outcomes are printed in human readable form to *TRACE-OUTPUT*. These
  values may not be @READABLE, JTRACE does not care.

  The format of the output is the same as that of PPRINT-EVENTS.
  Behind the scenes, JTRACE encapsulates the global functions with
  NAMES in wrapper that behaves as if `FOO` in the example above was
  defined like this:

  ```
  (defun foo (x)
    (framed (foo :args `(,x) :log-record *trace-journal*)
      (1+ x)))
  ```

  If JTRACE is invoked with no arguments, it returns the list of
  symbols currently traced.

  On Lisps other than SBCL, where a function encapsulation facility is
  not available or it is not used by Journal, JTRACE simply sets
  SYMBOL-FUNCTION. This solution loses the tracing encapsulation when
  the function is recompiled. On these platforms, `(JTRACE)` also
  retraces all functions that should be traced, but aren't.

  The main advantage of JTRACE over CL:TRACE is the ability to trace
  errors, not just normal return values. As it is built on JOURNALED,
  it can also detect - somewhat heuristically - THROWs and similar.
  """
  (if names
      `(progn
         ,@(loop for name in names
                 collect `(jtrace-1 ,name)))
      `(list-traced-functions)))

(defmacro jtrace-1 (name)
  #+sbcl
  (alexandria:with-gensyms (fn args)
    `(unless (sb-int:encapsulated-p ',name 'jtrace)
       (sb-int:encapsulate
        ',name 'jtrace
        (sb-int:named-lambda jtrace-encapsulation (,fn &rest ,args)
          (journaled (,name :args ,args :log-record *trace-journal*)
            (apply ,fn ,args))))
       (setf (gethash ',name *traced-functions*) t)))
  #-sbcl
  (alexandria:with-gensyms (fn args)
    `(unless (jtracedp ',name)
       (let ((,fn (symbol-function ',name)))
         (flet ((jtrace-encapsulation (&rest ,args)
                  (journaled (,name :args ,args :log-record *trace-journal*)
                    (apply ,fn ,args))))
           (setf (symbol-function ',name) #'jtrace-encapsulation)
           (setf (gethash ',name *traced-functions*)
                 (cons ,fn #'jtrace-encapsulation))))
       t)))

(defun jtracedp (name)
  #+sbcl
  (sb-int:encapsulated-p name 'jtrace)
  #-sbcl
  (let ((entry (gethash name *traced-functions*)))
    (when entry
      (destructuring-bind (original-fn . encapsulation-fn) entry
        (declare (ignore original-fn))
        ;; Recompiling the function replaces the encapsulation we put
        ;; there.
        (eq (symbol-function name) encapsulation-fn)))))

(defmacro juntrace (&rest names)
  "Like CL:UNTRACE, JUNTRACE makes it so that the global functions
  denoted by the symbols NAMES are no longer traced by JTRACE. When
  invoked with no arguments, it untraces all traced functions."
  (if names
      `(progn
         ,@(loop for name in names
                 collect `(juntrace-1 ',name)))
      `(untrace-all)))

(defun juntrace-1 (name)
  #+sbcl
  (when (sb-int:encapsulated-p name 'jtrace)
    (sb-int:unencapsulate name 'jtrace)
    (remhash name *traced-functions*))
  #-sbcl
  (let ((entry (gethash name *traced-functions*)))
    (when entry
      (destructuring-bind (original-fn . encapsulation-fn) entry
        (when (eq (symbol-function name) encapsulation-fn)
          (setf (symbol-function name) original-fn)))
      (remhash name *traced-functions*))))

(defun untrace-all ()
  (loop for name being each hash-key in *traced-functions*
        do (juntrace-1 name)))

(defsection @journal-slime-integration (:title "Slime integration")
  "[Slime](https://common-lisp.net/project/slime/) by default binds
  `C-c C-t` to toggling CL:TRACE. To integrate JTRACE into Slime, add
  the following ELisp snippet to your Emacs initialization file or
  load `src/journal.el`:"
  (journal.el (include #.(asdf:system-relative-pathname
                          :journal "src/journal.el")
                       :header-nl "```elisp" :footer-nl "```"))
  "Since JTRACE lacks some features of CL:TRACE, most notably that of
  tracing non-global functions, it is assigned a separate binding,
  `C-c C-j`.")

;;; For Slime
(defun swank-toggle-jtrace (spec-string)
  (let ((spec (swank::from-string spec-string)))
    (cond ((jtracedp spec)
           (eval `(juntrace ,spec))
           (format nil "~S is now untraced." spec))
          (t
           (eval `(jtrace ,spec))
           (format nil "~S is now traced." spec)))))


(defsection @replay (:title "Replay")
  "During replay, code is executed normally with special rules for
  @BLOCKs. There are two modes for dealing with blocks: replaying the
  code or replaying the outcome. When code is replayed, upon entering
  and leaving a block, the events generated are matched to events read
  from the journal being replayed. If the events don't match, a
  REPLAY-FAILURE is signaled which marks the record journal as having
  failed the replay. This is intended to make sure that the state of
  the program during the replay matches the state at the time of
  recording. In the other mode, when the outcome is replayed, a block
  may not be executed at all, but its recorded outcome is
  reproduced (e.g. the recorded return values are returned).

  Replay can be only be initiated with WITH-JOURNALING (or its close
  kin WITH-BUNDLE). After the per-event processing described below,
  when WITH-JOURNALING finishes, it might signal REPLAY-INCOMPLETE if
  there are unprocessed non-log events left in the replay journal.

  Replay is deemed successful or failed depending on whether all
  events are replayed from the replay journal without a
  REPLAY-FAILURE. A journal that records events from a successful
  replay can be used in place of the journal that was replayed, and so
  on. The logic of replacing journals with their successful replays is
  automated by @BUNDLES. WITH-JOURNALING does not allow replay from
  journals that were failed replays themselves. The mechanism, in
  terms of which tracking success and failure of replays is
  implemented, revolves around [JOURNAL-STATE][type] and
  [EVENT-VERSION][type]s, which we discuss next."
  (journal-state type)
  (@journaled-for-replay section)
  (@bundles section)
  (@the-replay-strategy section)
  (@matching-in-events section)
  (@matching-out-events section)
  (@replay-failures section)
  (@upgrades-and-replay section))

(deftype journal-state ()
  "JOURNAL's state, with respect to replay, is updated during
  WITH-JOURNALING. This possible states are:

  - __:NEW__: This journal was just created, but never recorded to.

  - __:REPLAYING__: Replaying events has started, some events may have
    been replayed successfuly, but there are more, non-log events to
    replay.

  - __:MISMATCHED__: There was a REPLAY-FAILURE. In this state,
    VERSIONED-EVENTs generated are downgraded to LOG-EVENTs,
    EXTERNAL-EVENTs and INVOKED trigger a DATA-EVENT-LOSSAGE.

  - __:RECORDING__: All events from the replay journal were
    successfully replayed and now new events are being recorded
    without being matched to the replay journal.

  - __:LOGGING__: There was a RECORD-UNEXPECTED-OUTCOME. In this
    state, VERSIONED-EVENTs generated are downgraded to LOG-EVENTs,
    EXTERNAL-EVENTs and INVOKED trigger a DATA-EVENT-LOSSAGE.

  - __:FAILED__: The journal is to be discarded. It encountered a
    JOURNALING-FAILURE or a REPLAY-FAILURE without completing the
    replay and reaching :RECORDING.

  - __:COMPLETED__: All events were successfully replayed and
    WITH-JOURNALING finished or a JOURNALING-FAILURE occurred while
    :RECORDING or :LOGGING.

  The state transitions are:

      :NEW                -> :REPLAYING  (on entering WITH-JOURNALING)
      :REPLAYING          -> :MISMATCHED (on REPLAY-FAILURE)
      :REPLAYING          -> :FAILED     (on REPLAY-INCOMPLETE)
      :REPLAYING          -> :FAILED     (on JOURNALING-FAILURE)
      :REPLAYING          -> :RECORDING  (on successfully replaying all events)
      :MISMATCHED         -> :FAILED     (on leaving WITH-JOURNALING)
      :RECORDING          -> :LOGGING    (on RECORD-UNEXPECTED-OUTCOME)
      :RECORDING/:LOGGING -> :COMPLETED  (on leaving WITH-JOURNALING)
      :RECORDING/:LOGGING -> :COMPLETED  (on JOURNALING-FAILURE)

  :NEW is the starting state. It is a JOURNAL-ERROR to attempt to
  write to journals in :COMPLETED. Note that once in :RECORDING, the
  only possible terminal state is :COMPLETED."
  `(member :new :replaying :mismatched :recording :logging :failed :completed))

(defsection @journaled-for-replay (:title "Journaled for replay")
  "The following arguments of JOURNALED control behaviour under replay.

  - VERSION: see [EVENT-VERSION][type] below.

  - INSERTABLE controls whether VERSIONED-EVENTs and EXTERNAL-EVENTs
    may be replayed with the _insert_ replay strategy (see
    @THE-REPLAY-STRATEGY). Does not affect LOG-EVENTs, that are always
    _insert_ed. Note that inserting EXTERNAL-EVENTs while :REPLAYING
    is often not meaningful (e.g. asking the user for input may lead
    to a REPLAY-FAILURE). See PEEK-REPLAY-EVENT for an example on how
    to properly insert these kinds of EXTERNAL-EVENTs.

  - REPLAY-VALUES, a function or NIL, may be called with EVENT-OUTCOME
    when replaying and :VERSION :INFINITY. NIL is equivalent to
    VALUES-LIST. See `VALUES<-` for an example.

  - REPLAY-CONDITION, a function or NIL, may be called with
    EVENT-OUTCOME (the return value of the function provided as
    CONDITION) when replaying and :VERSION is :INFINITY. NIL is
    equivalent to ERROR. Replaying conditions is cumbersome and best
    avoided."
  (*force-insertable* variable)
  (event-version type)
  (log-event type)
  (versioned-event type)
  (external-event type)
  "Built on top of JOURNALED, the macros below record a pair of
  @IN-EVENTS and @OUT-EVENTS, but differ in how they are replayed and
  the requirements on their @BLOCKs. The following table names the
  type of EVENT produced (`Event`), how @IN-EVENTS are
  replayed (`In-e.`), whether the block is always run (`Run`), how
  @OUT-EVENTS are replayed (`Out-e.`), whether the block must be
  deterministic (`Det`) or side-effect free (`SEF`).

      |          | Event     | In-e.  | Run | Out-e. | Det | SEF |
      |----------+-----------+--------+-----+--------+-----+-----|
      | FRAMED   | log       | skip   | y   | skip   | n   | n   |
      | CHECKED  | versioned | match  | y   | match  | y   | n   |
      | REPLAYED | external  | match  | n   | replay | n   | y   |
      | INVOKED  | versioned | replay | y   | match  | y   | n   |

  Note that the replay-replay combination is not implemented because
  there is nowhere to return values from replay-triggered functions."
  (framed macro)
  (checked macro)
  (replayed macro)
  (invoked glossary-term)
  (define-invoked macro)
  (flet-invoked macro))

(defvar *force-insertable* nil
  "The default value of the INSERTABLE argument of JOURNALED for
  VERSIONED-EVENTs. Binding this to T allows en-masse structural
  upgrades in combination with WITH-REPLAY-FILTER. Does not affect
  EXTERNAL-EVENTs. See @UPGRADES-AND-REPLAY.")

(deftype log-event ()
  "Events with [EVENT-VERSION][type] NIL called log events. During @REPLAY,
  they are never matched to events from the replay journal, and log
  events in the replay do not affect events being recorded either.
  These properties allow log events to be recorded in arbitrary
  journals with JOURNALED's LOG-RECORD argument. The convenience macro
  FRAMED is creating frames of log-events, while the LOGGED generates
  a log-event that's a LEAF-EVENT."
  '(satisfies log-event-p))

(deftype versioned-event ()
  "Events with a positive integer [EVENT-VERSION][type] are called
  versioned events. In @REPLAY, they undergo consistency checks unlike
  LOG-EVENTs, but the rules for them are less strict than for
  EXTERNAL-EVENTs. In particular, higher versions are always
  considered compatible with lower versions, they become an _upgrade_
  in terms of the @THE-REPLAY-STRATEGY, and versioned events can be
  inserted into the record without a corresponding REPLAY-EVENT with
  JOURNALED's INSERTABLE.

  If a VERSIONED-EVENT has an UNEXPECTED-OUTCOME,
  RECORD-UNEXPECTED-OUTCOME is signalled."
 '(satisfies versioned-event-p))

(deftype external-event ()
  "Events with [EVENT-VERSION][type] :INFINITY are called external events.
  They are like VERSIONED-EVENTs whose version was bumped all the way
  to infinity, which rules out easy, non-matching upgrades. Also, they
  are never inserted to the record without a matching replay
  event (see @THE-REPLAY-STRATEGY).

  In return for these restrictions, external events can be replayed
  without running the corresponding @BLOCK (see
  @REPLAYING-THE-OUTCOME). This allows their out-event variety, called
  DATA-EVENTs, to be non-deterministic. Data events play a crucial
  role in @PERSISTENCE.

  If an EXTERNAL-EVENT has an UNEXPECTED-OUTCOME,
  RECORD-UNEXPECTED-OUTCOME is signalled."
  '(satisfies external-event-p))

(defmacro framed ((name &key log-record args values condition) &body body)
  "A wrapper around JOURNALED to produce @FRAMEs of LOG-EVENTs. That
  is, VERSION is always NIL, and some irrelevant arguments are
  omitted. The related LOGGED creates a single LEAF-EVENT.

  With FRAMED, BODY is always run and no REPLAY-FAILUREs are
  triggered. BODY is not required to be deterministic and it may have
  side-effects."
  `(journaled (,name :log-record ,log-record :args ,args
                     :values ,values :condition ,condition)
     ,@body))

(defmacro checked ((name &key (version 1) args values condition insertable)
                   &body body)
  "A wrapper around JOURNALED to produce @FRAMEs of VERSIONED-EVENTs.
  VERSION defaults to 1. CHECKED is for ensuring that supposedly
  deterministic processing does not veer off the replay.

  With CHECKED, BODY - which must be deterministic - is always run and
  REPLAY-FAILUREs are triggered when the events generated do not match
  the events in the replay journal. BODY may have side-effects.

  For further discussion of determinism, see REPLAYED."
  `(journaled (,name :version ,version :args ,args
                     :values ,values :condition ,condition
                     :insertable ,insertable)
     ,@body))

(defmacro replayed ((name &key args values condition insertable
                     replay-values replay-condition)
                    &body body)
  "A wrapper around JOURNALED to produce @FRAMEs of EXTERNAL-EVENTs.
  VERSION is :INFINITY. REPLAYED is for primarily for marking and
  isolating non-deterministic processing.

  With REPLAYED, the IN-EVENT is checked for consistency with the
  replay (as with CHECKED), but BODY is not run (assuming it has a
  recorded EXPECTED-OUTCOME) and the outcome in the OUT-EVENT is
  reproduced (see @REPLAYING-THE-OUTCOME). For this scheme to work,
  REPLAYED requires its BODY to be side-effect free, but it may be
  non-deterministic."
  `(journaled (,name :version :infinity :args ,args
                     :values ,values :condition ,condition
                     :insertable ,insertable
                     :replay-values ,replay-values
                     :replay-condition ,replay-condition)
     ,@body))

(define-glossary-term invoked (:title "Invoked")
  "Invoked refers to functions and blocks defined by DEFINE-INVOKED or
  FLET-INVOKED. Invoked frames may be recorded in response to
  asynchronous events, and at replay the presence of its in-event
  triggers the execution of the function associated with the name of
  the event.

  On one hand, FRAMED, CHECKED, REPLAYED or plain JOURNALED have
  @IN-EVENTS that are always predictable from the code and the
  preceding events. The control flow - on the level of recorded frames
  - is deterministic in this sense. On the other hand, Invoked encodes
  in its IN-EVENT what function to call next, introducing
  non-deterministic control flow.

  By letting events choose the code to run, Invoked resembles typical
  [Event Sourcing][event-sourcing] frameworks. When Invoked is used
  exclusively, the journal becomes a sequence of events. In contrast,
  JOURNALED and its wrappers put code first, and the journal will be a
  projection of the call tree.")

(defvar *invoked-event-name-to-function-name* (make-hash-table :test #'equal))
;;; Name-to-function alist
(defvar *local-invoked-event-name-to-function* ())
#+sbcl
(declaim (sb-ext:always-bound *invoked-event-name-to-function-name*
                              *local-invoked-event-name-to-function*))

(defmacro define-invoked (function-name args
                          (name &key (version 1) insertable)
                          &body body)
  """DEFINE-INVOKED is intended for recording asynchronous function
  invocations like event or signal handlers. It defines a function
  that records VERSIONED-EVENTs with ARGS set to the actual arguments.
  At replay, it is invoked whenever the recorded IN-EVENT becomes the
  REPLAY-EVENT.

  DEFUN and CHECKED rolled into one, DEFINE-INVOKED defines a
  top-level function with FUNCTION-NAME and ARGS (only simple
  positional arguments are allowed) and wraps CHECKED with NAME, the
  same ARGS and INSERTABLE around BODY. Whenever an IN-EVENT becomes
  the REPLAY-EVENT and it has a DEFINE-INVOKED defined with the name
  of the event, then FUNCTION-NAME is invoked with EVENT-ARGS.

  While BODY's return values are recorded as usual, the defined
  function returns no values to make it less likely to affect control
  flow in a way that's not possible to reproduce when the function is
  called by the replay mechanism.

  ```
  (defvar *state*)

  (define-invoked foo (x) ("foo")
    (setq *state* (1+ x)))

  (define-invoked bar (x) ("bar")
    (setq *state* (+ 2 x)))

  (if (zerop (random 2))
      (foo 0)
      (bar 1))
  ```

  The above can be alternatively implemented with REPLAYED explicitly
  encapsulating the non-determinism:

  ```
  (let ((x (replayed (choose) (random 2))))
    (if (zerop x)
        (checked (foo :args `(,x))
          (setq *state* (1+ x)))
        (checked (bar :args `(,x))
          (setq *state* (+ 2 x)))))
  ```
  """
  `(progn
     (setf (gethash ',name *invoked-event-name-to-function-name*)
           ',function-name)
     (defun ,@(invoked/1 function-name args name version insertable body))))

(defun invoked/1 (function-name args name version insertable body)
  `(,function-name
    ,args
    (assert (and ,version (not (eq ,version :infinity))))
    ;; KLUDGE: Prevent another call SKIP-EVENTS, which might trigger
    ;; this INVOKED again.
    (when *replay-streamlet*
      (setq *skipped-events-until* (read-position *replay-streamlet*)))
    (checked (,name :version ,version :args (list ,@args)
                    :insertable ,insertable)
      ,@body)
    (values)))

(defmacro flet-invoked (definitions &body body)
  """Like DEFINE-INVOKED, but with FLET instead of DEFUN. The event
  name and the function are associated in the dynamic extent of BODY.
  WITH-JOURNALING does not change the bindings. The example in
  DEFINE-INVOKED can be rewritten as:

  ```
  (let ((state nil))
    (flet-invoked ((foo (x) ("foo")
                     (setq state (1+ x)))
                   (bar (x) ("bar")
                     (setq state (+ 2 x))))
      (if (zerop (random 2))
        (foo 0)
        (bar 1))))
  ```
  """
  (let ((entries (loop for definition in definitions
                       collect (flet-invoked/1 definition))))
    `(flet ,(mapcar #'third entries)
       (let ((*local-invoked-event-name-to-function*
               *local-invoked-event-name-to-function*))
         ,@(mapcar (lambda (entry)
                     `(push (cons ,(first entry) #',(second entry))
                            *local-invoked-event-name-to-function*))
                   entries)
         ,@body))))

(defun flet-invoked/1 (definition)
  (destructuring-bind (function-name args
                       (name &key (version 1) insertable)
                       &body body)
      definition
    (list name function-name
          (invoked/1 function-name args name version insertable body))))

(defun invoked-function (event-name)
  (or (alexandria:assoc-value *local-invoked-event-name-to-function* event-name
                              :test #'equal)
      (let ((symbol
              (gethash event-name *invoked-event-name-to-function-name*)))
        (cond ((and (symbol-package symbol)
                    (fboundp symbol))
               symbol)
              ;; Handle UNINTERNed and MAKUNBOUND functions.
              (t
               (remhash event-name *invoked-event-name-to-function-name*)
               nil)))))

(defun maybe-handle-invoked (in-event)
  (when (versioned-event-p in-event)
    (let ((function (invoked-function (event-name in-event))))
      (when function
        (apply function (event-args in-event))
        t))))


(defsection @bundles (:title "Bundles")
  """Consider replaying the same code repeatedly, hoping to make
  progress in the processing. Maybe based on the availability of
  external input, the code may error out. After each run, one has to
  decide whether to keep the journal just recorded or stick with the
  replay journal. A typical solution to this would look like this:

  ```
  (let ((record nil))
    (loop
      (setq record (make-in-memory-journal))
      (with-journaling (:record record :replay replay)
        ...)
      (when (and
             ;; RECORD is a valid replay of REPLAY ...
             (eq (journal-state record) :completed)
             ;; ... and is also significantly different from it ...
             (journal-diverged-p record))
        ;; so use it for future replays.
        (setq replay record))))
  ```

  This is pretty much what bundles automate. The above becomes:

  ```
  (let ((bundle (make-in-memory-bundle)))
    (loop
      (with-bundle (bundle)
        ...)))
  ```

  With FILE-JOURNALs, the motivating example above would be even more
  complicated, but FILE-BUNDLEs work the same way as
  IN-MEMORY-BUNDLEs.
  """
  (with-bundle macro))

(defmacro with-bundle ((bundle) &body body)
  "This is like WITH-JOURNALING where the REPLAY-JOURNAL is the last
  successfully completed one in BUNDLE, and the RECORD-JOURNAL is a
  new one created in BUNDLE. When WITH-BUNDLE finishes, the record
  journal is in JOURNAL-STATE :FAILED or :COMPLETED.

  To avoid accumulating useless data, the new record is immediately
  deleted when WITH-BUNDLE finishes if it has not diverged from the
  replay journal (see JOURNAL-DIVERGENT-P). Because :FAILED journals
  are always divergent in this sense, they are deleted instead based
  on whether there is already a previous failed journal in the bundle
  and the new record is identical to that journal (see
  IDENTICAL-JOURNALS-P).

  It is a JOURNAL-ERROR to have concurrent or nested WITH-BUNDLEs on
  the same bundle."
  (alexandria:once-only (bundle)
    (alexandria:with-gensyms (record replay)
      `(progn
         (check-type ,bundle bundle)
         (multiple-value-bind (,record ,replay)
             (with-bundle-locked (,bundle)
               (check-concurrent-write-access ,bundle)
               (incf (slot-value ,bundle 'n-writers))
               (values (bundle-record-journal ,bundle)
                       (bundle-replay-journal ,bundle)))
           (unwind-protect*
               (with-journaling (:record ,record :replay ,replay)
                 ,@body)
             (with-bundle-locked (,bundle)
               (decf (slot-value ,bundle 'n-writers))
               (reap-identical-or-non-divergent-journals ,bundle)
               (reap-journals-in-bundle ,bundle))))))))


(defsection @the-replay-strategy (:title "The replay strategy")
  "The replay process for both @IN-EVENTs and @OUT-EVENTs starts by
  determining how the generated event (the _new_ event from now on)
  shall be replayed. Roughly, the decision is based on the NAME and
  VERSION of the new event and the REPLAY-EVENT (the next event to be
  read from the replay). There are four possible strategies:

  - **match**: A new in-event must match the replay event in its ARGS.
    See @MATCHING-IN-EVENTS for details. A new out-event must match
    the replay event's EXIT and OUTCOME, see @MATCHING-OUT-EVENTS.

  - **upgrade**: The new event is not matched to any replay event, but
    an event is consumed from the replay journal. This happens if next
    new event has the same name as the replay event, but its version
    is higher.

  - **insert**: The new event is not matched to any replay event, and
    no events are consumed from the replay journal, which may be
    empty. This is always the case for new LOG-EVENTs and when there
    are no more events to read from the replay journal (unless
    REPLAY-EOJ-ERROR-P). For VERSIONED-EVENTs, it is affected by
    setting JOURNALED's INSERTABLE to true (see
    @JOURNALED-FOR-REPLAY).

      The out-event's strategy is always _insert_ if the strategy for
      the corresponding in-event was _insert_.

  - Also, END-OF-JOURNAL, REPLAY-NAME-MISMATCH and
    REPLAY-VERSION-DOWNGRADE may be signalled. See the algorithm below
    details.

  The strategy is determined by the following algorithm, invoked
  whenever an event is generated by a journaled @BLOCK:

  1. Log events are not matched to the replay. If the new event is a
     log event or a REPLAY-FAILURE has been signalled before (i.e. the
     record journal's JOURNAL-STATE is :MISMATCHED), then __insert__
     is returned.

  2. Else, log events to be read in the replay journal are skipped,
     and the next unread, non-log event is peeked at (without
     advancing the replay journal).

      - __end of replay__: If there are no replay events left, then:

          - If REPLAY-EOJ-ERROR-P is NIL in WITH-JOURNALING (the
            default), __insert__ is returned.

          - If REPLAY-EOJ-ERROR-P is true, then __`END-OF-JOURNAL`__
            is signalled.

      - __mismatched name__: Else, if the next unread replay event's
        name is not EQUAL to the name of the new event, then:

          - For VERSIONED-EVENTs, __REPLAY-NAME-MISMATCH__ is
            signalled if INSERTABLE is NIL, else __insert__ is
            returned.

          - For EXTERNAL-EVENTs, __REPLAY-NAME-MISMATCH__ is
            signalled.

      - __matching name__: Else, if the name of the next unread event
        in the replay journal is EQUAL to the name of new event, then
        it is chosen as the _replay_ event.

          - If the replay event's version is higher than the new
            event's version, then __REPLAY-VERSION-DOWNGRADE__ is
            signalled.

          - If the two versions are equal, then __match__ is returned.

          - If the new event's version is higher, then __upgrade__ is
            returned.

          Where :INFINITY is considered higher than any integer and
          equal to itself.

  In summary:

       | new event | end-of-replay     | mismatched name   | matching name |
       |-----------+-------------------+-------------------+---------------|
       | Log       | insert            | insert            | insert        |
       | Versioned | insert/eoj-error  | insert/name-error | match-version |
       | External  | insert/eoj-error  | insert/name-error | match-version |

  Version matching (`match-version` above) is based which event has a
  higher version:

       | replay event    | =     | new event |
       |-----------------+-------+-----------|
       | downgrade-error | match | upgrade   |"
  (replay-event glossary-term))

(define-glossary-term replay-event (:title "replay event")
  "The replay event is the next event to be read from REPLAY-JOURNAL
  which is not to be skipped. There may be no replay event if there
  are no more unread events in the replay journal.

  An event in the replay journal is skipped if it is a LOG-EVENT or
  there is a WITH-REPLAY-FILTER with a matching :SKIP. If :SKIP is in
  effect, the replay event may be indeterminate.

  Events from the replay journal are read when they are `:MATCH`ed or
  `:UPGRADE`d (see @THE-REPLAY-STRATEGY), when nested events are
  echoed while @REPLAYING-THE-OUTCOME, or when there is an INVOKED
  defined with the same name as the replay event.

  The replay event is available via PEEK-REPLAY-EVENT.")

;;; Return one of :MATCH, :UPGRADE, and :INSERT, or signal
;;; REPLAY-NAME-MISMATCH, END-OF-JOURNAL, REPLAY-VERSION-DOWNGRADE.
(defun replay-strategy (event insertable record-streamlet replay-streamlet
                        replay-eoj-error-p)
  (when record-streamlet
    (assert (not (member *record-journal-state* '(:failed :completed)))))
  (if (or (log-event-p event)
          ;; Silence replay errors after the first one.
          *replay-failure*
          (and record-streamlet (eq *record-journal-state* :logging)))
      :insert
      (let ((replay-event (when replay-streamlet
                            (skip-events-and-maybe->recording
                             record-streamlet replay-streamlet)
                            (peek-event replay-streamlet))))
        (cond ((null replay-event)
               (if (and replay-streamlet replay-eoj-error-p)
                   (error 'end-of-journal :journal (journal replay-streamlet))
                   :insert))
              ((equal (event-name event) (event-name replay-event))
               (match-version event replay-event))
              (insertable
               :insert)
              (t
               (replay-failure 'replay-name-mismatch :new-event event
                               :replay-event replay-event
                               :upgrade-restart t
                               :insert-restart t))))))

(defun match-version (event replay-event)
  (cond ((version= (event-version replay-event) (event-version event))
         :match)
        ((version< (event-version replay-event) (event-version event))
         :upgrade)
        (t
         (replay-failure 'replay-version-downgrade
                         :new-event event :replay-event replay-event
                         :upgrade-restart t))))


(defsection @matching-in-events (:title "Matching in-events")
  "If the replay strategy is _match_, then, for in-events, the
  matching process continues like this:

  - If the [EVENT-ARGS] are not EQUAL, then __`REPLAY-ARGS-MISMATCH`__
    signalled.

  - At this point, two things might happen:

      - For VERSIONED-EVENTs, the @BLOCK will be executed as normal
        and its outcome will be matched to the REPLAY-EVENT (see
        @MATCHING-OUT-EVENTS).

      - For EXTERNAL-EVENTs, the corresponding replay OUT-EVENT is
        looked at. If there is one, meaning that the frame finished
        with an EXPECTED-OUTCOME, then its outcome will be
        replayed (see @REPLAYING-THE-OUTCOME). If the OUT-EVENT is
        missing, then EXTERNAL-EVENTs behave like VERSIONED-EVENTs,
        and the @BLOCK is executed."
  (@replaying-the-outcome section))

;;; Like MAKE-IN-EVENT, but with reduced consing and keeping track of
;;; divergence.
(declaim (inline make-in-event*))
(defun make-in-event* (replay-event name version args)
  (if (and (equal name (event-name replay-event))
           (eql version (event-version replay-event))
           (equal args (event-args replay-event)))
      replay-event
      (progn
        (check-type version event-version)
        (maybe-mark-record-as-divergent version)
        (cond ((and version args)
               `(:in ,name :version ,version :args ,args))
              (version
               `(:in ,name :version ,version))
              (args
               `(:in ,name :args ,args))
              (t
               `(:in ,name))))))

(defun maybe-mark-record-as-divergent (version)
  (when (and version *record-streamlet*)
    (unless (slot-value (journal *record-streamlet*) 'replay-mismatch)
      (setf (slot-value (journal *record-streamlet*) 'replay-mismatch)
            (list (write-position *record-streamlet*)
                  (if *replay-streamlet*
                      (read-position *replay-streamlet*)
                      nil))))))

(defun handle-in-event (record-streamlet log-record name version args
                        replay-streamlet insertable
                        replay-values replay-condition
                        replay-eoj-error-p)
  (let* ((replay-in-event (and replay-streamlet (peek-event replay-streamlet)))
         (in-event (make-in-event* replay-in-event name version args))
         (strategy (replay-strategy in-event insertable
                                    record-streamlet replay-streamlet
                                    replay-eoj-error-p)))
    (when (eq strategy :match)
      (check-args in-event replay-in-event))
    (route-event in-event record-streamlet log-record)
    (when (or (eq strategy :match) (eq strategy :upgrade))
      (read-event replay-streamlet)
      (unwind-protect
           (let ((replayed-out-event nil))
             (when (and (external-event-p in-event)
                        (setq replayed-out-event
                              (peek-at-out-event replay-streamlet)))
               ;; If :UPGRADE, then this is an upgrade to :INFINITY.
               ;; If :MATCH, then normal @REPLAYING-THE-OUTCOME.
               (maybe-replay-outcome record-streamlet replay-streamlet
                                     replayed-out-event
                                     replay-values replay-condition)))
        (skip-events-and-maybe->recording record-streamlet replay-streamlet)))
    (maybe-sync-after-in-event in-event record-streamlet)
    strategy))

(defun check-args (in-event replayed-in-event)
  (unless (equal (event-args in-event) (event-args replayed-in-event))
    (replay-failure 'replay-args-mismatch :new-event in-event
                    :replay-event replayed-in-event
                    :upgrade-restart t)))

(defun skip-events-and-maybe->recording (record-streamlet replay-streamlet)
  (skip-events replay-streamlet)
  (when (->recording-p record-streamlet replay-streamlet)
    (set-journal-state record-streamlet :recording)))

(defun skip-events (replay-streamlet)
  (when (and replay-streamlet
             ;; Prevent infinite recursion with MAYBE-HANDLE-INVOKED.
             (not (and *skipped-events-until*
                       (= *skipped-events-until*
                          (read-position replay-streamlet)))))
    (setq *skipped-events-until* nil)
    (if *skip-events*
        (funcall *skip-events*)
        (skip-log-events replay-streamlet))
    (let ((event (peek-event replay-streamlet)))
      (and event (in-event-p event)
           (maybe-handle-invoked event)))))

(defun skip-log-events (streamlet)
  (loop for event = (peek-event streamlet)
        while (and event (log-event-p event))
        do (read-event streamlet nil)))

;;; Do we need to transition to :RECORDING?
(defun ->recording-p (record-streamlet replay-streamlet)
  (and record-streamlet
       (eq :replaying *record-journal-state*)
       ;; Are we at the end of the replay journal? Having no replay is
       ;; the same as having an empty replay.
       (or (null replay-streamlet)
           (not (peek-event replay-streamlet)))))

(declaim (inline apply-key))
(defun apply-key (key-fn obj)
  (if key-fn
      (funcall key-fn obj)
      obj))

;;; For testing
(defvar *next-write-event-fn* nil)

;;; LOG-RECORD is a JOURNAL or NIL. LOG-RECORD, if non-NIL, takes
;;; precedence over RECORD-STREAMLET for LOG-EVENTs.
;;;
;;; This must be called before updating the state with
;;; SKIP-EVENTS-AND-MAYBE->RECORDING and MAYBE-SYNC.
(defun route-event (event record-streamlet log-record)
  (let ((logp (log-event-p event)))
    (labels ((decorate-if-log (journal)
               (if logp
                   (apply-key (journal-log-decorator journal) event)
                   event))
             ;; KLUDGE: We rely on (write-event (method () (t journal))),
             ;; but that writes through JOURNAL-OUTPUT-STREAMLET not
             ;; *RECORD-STREAMLET* even if JOURNAL is the same
             ;; journal *RECORD-STREAMLET* is writing.
             (write-log-event-to-journal (journal)
               (when (eq (journal-state journal) :completed)
                 (error 'journal-error :journal journal
                        :format-control "~@<Refusing to log to journal ~S ~
                                        which is in JOURNAL-STATE ~
                                        :COMPLETED.~:@>"
                        :format-args (list journal)))
               (if (and record-streamlet
                        (eq journal (journal record-streamlet)))
                   (write-event (decorate-if-log (journal record-streamlet))
                                record-streamlet)
                   (write-event (decorate-if-log journal) journal))))
      (cond ((or (not logp) (null log-record))
             ;; Handle test callback.
             (when *next-write-event-fn*
               (let ((fn *next-write-event-fn*))
                 (setq *next-write-event-fn* nil)
                 (return-from route-event (funcall fn event))))
             (when record-streamlet
               (let ((event (decorate-if-log (journal record-streamlet))))
                 (write-event event record-streamlet))))
            ((and logp (null log-record)))
            (t
             (assert (and logp log-record))
             (write-log-event-to-journal log-record))))))

;;; Assuming the frame's in-event has just been read, return the
;;; corresponding out-event without changing the read position. Return
;;; NIL if the out-event is not present."
(defun peek-at-out-event (streamlet)
  (let ((next-event (peek-event streamlet)))
    (if (out-event-p next-event)
        next-event
        (let ((end-of-frame-position (end-of-frame-position streamlet)))
          (when end-of-frame-position
            (save-excursion (streamlet)
              (setf (read-position streamlet) end-of-frame-position)
              (read-event streamlet nil)))))))

;;; Return the position of the corresponding out-event or NIL if it
;;; doesn't exist. Use DEPTH 1 if READ-POSITION points just after the
;;; in-event, DEPTH 0 if it points just after the in-event.
(defun end-of-frame-position (streamlet &key (depth 1))
  (save-excursion (streamlet)
    (or (loop for read-position = (read-position streamlet)
              for event = (read-event streamlet nil)
              while event
              do (cond ((in-event-p event)
                        (incf depth))
                       ((out-event-p event)
                        (decf depth)))
                 (when (minusp depth)
                   ;; The first event read was an out-event.
                   (return (values nil depth)))
                 (when (zerop depth)
                   (return read-position)))
        (values nil depth))))


(defsection @replaying-the-outcome (:title "Replaying the outcome")
  "So, if an in-event is triggered that matches the replay,
  EVENT-VERSION is :INFINITY, then normal execution is altered in the
  following manner:

  - The journaled @BLOCK is not executed.

  - To keep execution and the replay journal in sync, events of frames
    nested in the current one are skipped over in the replay journal.

  - All events (including LOG-EVENTs) skipped over are echoed to the
    record journal. This serves to keep a trail of what happened
    during the original recording. Note that functions corresponding
    to INVOKED frames are called when their IN-EVENT is skipped over.

  - The out-event corresponding to the in-event being processed is
    then read from the replay journal and is recorded again (to allow
    recording to function properly).

  To be able to reproduce the outcome in the replay journal, some
  assistance may be required from REPLAY-VALUES and REPLAY-CONDITION:

  - If the REPLAY-EVENT has normal return (i.e. EVENT-EXIT :VALUES),
    then the recorded return values (in EVENT-OUTCOME) are returned
    immediately as in `(VALUES-LIST (EVENT-OUTCOME REPLAY-EVENT))`. If
    REPLAY-VALUES is specified, it is called instead of VALUES-LIST.
    See @WORKING-WITH-UNREADABLE-VALUES for an example.

  - Similarly, if the replay event has unwound with an expected
    condition (has EVENT-EXIT :CONDITION), then the recorded
    condition (in EVENT-OUTCOME) is signalled as
    IN `(ERROR (EVENT-OUTCOME REPLAY-EVENT))`. If REPLAY-CONDITION is
    specified, it is called instead of ERROR. REPLAY-CONDITION must
    not return normally.")

;;; This performs a non-local exit if it replays the outcome, either
;;; by throwing to REPLAY-VALUES-HAPPENED or by signalling an error.
(defun maybe-replay-outcome (record-streamlet replay-streamlet
                             replayed-out-event replay-values replay-condition)
  (cond ((values-event-p replayed-out-event)
         (echo-current-frame replay-streamlet record-streamlet)
         (skip-events-and-maybe->recording record-streamlet replay-streamlet)
         (turn-off-with-journaling-failure-on-nlx)
         (throw 'replay-values-happened
           (funcall (or replay-values #'values-list)
                    (event-outcome replayed-out-event))))
        ((condition-event-p replayed-out-event)
         (echo-current-frame replay-streamlet record-streamlet)
         (skip-events-and-maybe->recording record-streamlet replay-streamlet)
         (turn-off-with-journaling-failure-on-nlx)
         (funcall (or replay-condition #'error)
                  (event-outcome replayed-out-event))
         (error 'journal-error "The REPLAY-CONDITION argument of JOURNALED ~
                               must not return normally."))))

;;; Assuming the frame's in-event has just been read from
;;; INPUT-STREAMLET and the frame's out-event is present in it, read
;;; all events up to and including the frame's out-event. If
;;; OUTPUT-STREAMLET is not NIL, write all events read to it.
(defun echo-current-frame (input-streamlet output-streamlet)
  (let ((next-event (peek-event input-streamlet)))
    (cond
      ((null next-event))
      ;; The most common case by far is for EXTERNAL-EVENTs not to
      ;; have children.
      ((out-event-p next-event)
       (read-event input-streamlet)
       (when output-streamlet
         (write-event next-event output-streamlet)))
      (t
       ;; General case
       (let ((end-of-frame-position (end-of-frame-position input-streamlet)))
         (when end-of-frame-position
           (loop for i upfrom 0
                 for event = (peek-event input-streamlet)
                 do (unless (maybe-handle-invoked event)
                      (read-event input-streamlet)
                      (when output-streamlet
                        (write-event event output-streamlet)))
                 while (<= (read-position input-streamlet)
                           end-of-frame-position))))))))


(defsection @matching-out-events (:title "Matching out-events")
  "If there were no @REPLAY-FAILURES during the matching of the
  IN-EVENT, and the conditions for @REPLAYING-THE-OUTCOME were not
  met, then the @BLOCK is executed. When the outcome of the block is
  determined, an OUT-EVENT is triggered and is matched to the replay
  journal. The matching of out-events starts out as in
  @THE-REPLAY-STRATEGY with checks for EVENT-NAME and
  [EVENT-VERSION][function].

  If the replay strategy is _insert_ or _upgrade_, then the out-event
  is written to RECORD-JOURNAL, consuming an event with a matching
  name from the REPLAY-JOURNAL in the latter case. If the strategy is
  _match_, then:

  - If the new event has an UNEXPECTED-OUTCOME, then
    __REPLAY-UNEXPECTED-OUTCOME__ is signalled. Note that the replay
    event always has an EXPECTED-OUTCOME due to the handling of
    RECORD-UNEXPECTED-OUTCOME.

  - If the new event has an EXPECTED-OUTCOME, then unless the new and
    REPLAY-EVENT's EVENT-EXITs are `EQ` and their EVENT-OUTCOMEs are
    EQUAL, __REPLAY-OUTCOME-MISMATCH__ is signalled.

  - Else, the replay event is consumed and the new event is written
    the RECORD-JOURNAL.

  Note that @THE-REPLAY-STRATEGY for the in-event and the out-event of
  the same @FRAME may differ if the corresponding out-event is not
  present in REPLAY-JOURNAL, which may be the case when the recording
  process failed hard without unwinding properly, or when an
  UNEXPECTED-OUTCOME triggered the transition to JOURNAL-STATE
  :LOGGING.")

;;; Like MAKE-OUT-EVENT, but with reduced consing and keeping track of
;;; divergence.
(declaim (inline make-out-event*))
(defun make-out-event* (replay-event name version exit outcome)
  (if (and (equal name (event-name replay-event))
           (version= version (event-version replay-event))
           (eq exit (event-exit replay-event))
           (equal outcome (event-outcome replay-event)))
      replay-event
      (progn
        (check-type version event-version)
        (check-type exit event-exit)
        (assert (or (not (eq exit :nlx)) (null outcome)))
        (maybe-mark-record-as-divergent version)
        (if version
            `(:out ,name :version ,version ,exit ,outcome)
            `(:out ,name ,exit ,outcome)))))

(defun call-journaled
    (function
     record-streamlet log-record name version args values-key condition-key
     replay-streamlet insertable replay-values replay-condition
     replay-eoj-error-p)
  (unless (eq version :infinity)
    (when *force-insertable*
      (setq insertable t)))
  (catch 'replay-values-happened
    (let ((in-event-strategy
            (with-journaling-failure-on-nlx
              ;; This may be the version for the out-event, too. But
              ;; downgrades may happen later in HANDLE-NLX below.
              (setq version (maybe-downgrade-version version name
                                                     record-streamlet))
              (handle-in-event record-streamlet log-record
                               name version args
                               replay-streamlet insertable replay-values
                               replay-condition replay-eoj-error-p))))
      (labels
          ((peek-replay-event ()
             (and replay-streamlet
                  (peek-event replay-streamlet)))
           (handle-return (values)
             (with-journaling-failure-on-nlx
               (handle-out (make-out-event* (peek-replay-event)
                                            name version :values
                                            (apply-key values-key values)))))
           (handle-nlx (condition)
             (with-journaling-failure-on-nlx
               ;; Determine EXIT, transition to :LOGGING, downgrade
               ;; version if necessary, but only signal
               ;; RECORD-UNEXPECTED-OUTCOME *after* writing the event
               ;; out.
               (multiple-value-bind (gone-logging version exit outcome)
                   (nlx-outcome-and-maybe->logging
                    version record-streamlet condition condition-key)
                 (let ((event (make-out-event* (peek-replay-event)
                                               name version exit outcome)))
                   (handle-out event)
                   (when gone-logging
                     (signal 'record-unexpected-outcome :new-event event))))))
           (handle-out (event)
             (handle-out-event record-streamlet log-record event
                               in-event-strategy insertable
                               replay-streamlet replay-eoj-error-p)))
        ;; We make no attempt to handle errors encountered during the
        ;; processing of either of the cleanup functions. If that happens
        ;; in HANDLE-NLX that could replace one error with another.
        (nlx-protect (:on-return #'handle-return :on-nlx #'handle-nlx)
          (funcall function))))))

(defun maybe-downgrade-version (version name record-streamlet)
  (cond ((and record-streamlet
              (or (eq *record-journal-state* :mismatched)
                  (eq *record-journal-state* :logging)))
         (when (or (eq version :infinity)
                   (and version (invoked-function name)))
           (check-within-with-journaling-failure-on-nlx)
           (error 'data-event-lossage))
         nil)
        (t
         version)))

;;; By NLX, here, we mean the lisp @NON-LOCAL-EXITs. That is,
;;; EVENT-EXIT :CONDITION, :ERROR and :NLX.
(defun nlx-outcome-and-maybe->logging
    (version record-streamlet condition condition-key)
  ;; See RECORD-UNEXPECTED-OUTCOME. Here we downgrade versions of
  ;; out-events whose in-event was not recording in :LOGGING state.
  (let ((gone-logging nil))
    (multiple-value-bind (exit outcome)
        (if condition
            (let ((c (and condition-key (funcall condition-key condition))))
              (if c
                  (values :condition c)
                  (values :error
                          (with-standard-io-syntax
                            (cleanup
                             (list
                              (princ-to-string (type-of condition))
                              (princ-to-string condition)))))))
            (values :nlx nil))
      (when (and version record-streamlet)
        (let ((state *record-journal-state*))
          (cond ((and (or (eq exit :error) (eq exit :nlx))
                      (eq state :recording))
                 (set-journal-state record-streamlet :logging)
                 (setq gone-logging t)
                 (setq version nil))
                ((or (eq state :mismatched)
                     (eq state :logging))
                 (setq version nil)))))
      (values gone-logging version exit outcome))))

(defun cleanup (obj)
  ;; On SBCL, BASE-STRINGs are particularly ugly when printed:
  ;;
  ;; (with-standard-io-syntax
  ;;   (prin1 (princ-to-string 1)))
  ;; .. #A((1) BASE-CHAR . "1")
  #+sbcl
  (if (typep obj 'base-string)
      (make-array (length obj) :element-type 'character :initial-contents obj)
      obj)
  #-sbcl
  obj)

(defun handle-out-event (record-streamlet log-record out-event
                         in-event-strategy
                         insertable replay-streamlet replay-eoj-error-p)
  (let ((strategy (if (eq in-event-strategy :insert)
                      ;; REPLAY-STRATEGY would look at the next replay
                      ;; event, but the decision to insert has been
                      ;; made already based on previous replay event
                      ;; (more precisely, on the in-event of the
                      ;; nested frame modulo skips).
                      :insert
                      (replay-strategy out-event insertable
                                       record-streamlet replay-streamlet
                                       replay-eoj-error-p))))
    (ecase strategy
      ((:insert)
       (route-event out-event record-streamlet log-record))
      ((:upgrade)
       (when record-streamlet
         (write-event out-event record-streamlet))
       (read-event replay-streamlet)
       (skip-events-and-maybe->recording record-streamlet replay-streamlet))
      ((:match)
       (let ((replay-event (peek-event replay-streamlet)))
         (check-outcome out-event replay-event)
         (when record-streamlet
           (write-event out-event record-streamlet))
         (read-event replay-streamlet)
         (skip-events-and-maybe->recording record-streamlet replay-streamlet))))
    (maybe-sync-after-out-event out-event record-streamlet)))

(defun check-outcome (out-event replayed-out-event)
  (assert (expected-outcome-p replayed-out-event))
  (cond ((unexpected-outcome-p out-event)
         (replay-failure 'replay-unexpected-outcome :new-event out-event
                         :replay-event replayed-out-event))
        (t
         ;; From this point on, we know that we are dealing with two
         ;; normal outcomes, but they can be :VALUES or :CONDITION.
         (unless (and (eq (event-exit out-event)
                          (event-exit replayed-out-event))
                      (equal (event-outcome out-event)
                             (event-outcome replayed-out-event)))
           (replay-failure 'replay-outcome-mismatch
                           :new-event out-event
                           :replay-event replayed-out-event
                           :upgrade-restart t)))))


(defsection @replay-failures (:title "Replay failures")
  (replay-failure condition)
  (replay-failure-new-event (reader replay-failure))
  (replay-failure-replay-event (reader replay-failure))
  (replay-failure-replay-journal (reader replay-failure))
  (replay-name-mismatch condition)
  (replay-version-downgrade condition)
  (replay-args-mismatch condition)
  (replay-outcome-mismatch condition)
  (replay-unexpected-outcome condition)
  (replay-incomplete condition)
  (replay-force-insert restart)
  (replay-force-upgrade restart))

(define-condition replay-failure (serious-condition)
  ((new-event
    :initarg :new-event :reader replay-failure-new-event
    :documentation "The event being generated.")
   (replay-event
    :initarg :replay-event :reader replay-failure-replay-event
    :documentation "The event read from the replay journal.")
   (record-journal
    :initform (record-journal) :reader replay-failure-record-journal
    :documentation "The RECORD-JOURNAL in effect at the time of
    signalling the error.")
   (replay-journal
    :initform (replay-journal) :reader replay-failure-replay-journal
    :documentation "The REPLAY-JOURNAL in effect at the time of
    signalling the error.")
   (replay-position
    :initform (read-position *replay-streamlet*)
    :reader replay-failure-replay-position))
  (:documentation "A common superclass (never signalled itself) for
  all kinds of mismatches between the events produced and the replay
  journal. Signalled only in JOURNAL-STATE :REPLAYING and only once
  per WITH-JOURNALING. If a REPLAY-FAILURE is signalled for an EVENT,
  then the event will be recorded, but RECORD-JOURNAL will transition
  to JOURNAL-STATE :MISMATCHED. Like JOURNALING-FAILURE, this is a
  serious condition because it is to be handled outside the enclosing
  WITH-JOURNALING. If a REPLAY-FAILURE were to be handled inside the
  WITH-JOURNALING, keep in mind that in :MISMATCHED, replay always
  uses the _insert_ replay strategy (see @THE-REPLAY-STRATEGY)."))

(defun replay-failure (condition-type &key new-event replay-event
                       insert-restart upgrade-restart continue-restart)
  (assert *replay-streamlet*)
  ;; Don't signal more than once. Relies on switching to :INSERT on
  ;; :MISMATCHED.
  (assert (null *replay-failure*))
  (let ((condition (make-condition condition-type :new-event new-event
                                   :replay-event replay-event)))
    (assert (typep condition 'replay-failure))
    (flet ((on-nlx (c)
             (assert (eq c condition))
             (cond ((typep condition 'replay-incomplete)
                    ;; REPLAY-INCOMPLETE does not have a new event. Just
                    ;; check that the state is already set.
                    (when *record-streamlet*
                      (assert (eq (journal-state (record-journal)) :failed))))
                   (*record-streamlet*
                    (assert (eq (journal-state (record-journal)) :replaying))
                    (set-journal-state *record-streamlet* :mismatched)
                    ;; Make sure that the event that caused the failure is
                    ;; recorded for debugging and to make different failures
                    ;; not be IDENTICAL-JOURNALS-P and avoid reaping.
                    (write-event (replay-failure-new-event condition)
                                 *record-streamlet*)))
             ;; This is for WITH-JOURNALING to be able to tell without a
             ;; RECORD-JOURNAL whether to signal REPLAY-INCOMPLETE.
             (setq *replay-failure* condition)))
      (nlx-protect (:on-nlx #'on-nlx)
        (labels
            ((maybe-with-insert-restart ()
               (if insert-restart
                   (restart-case
                       (maybe-with-upgrade-restart)
                     (replay-force-insert ()
                       :report "Force :INSERT as JRN:@THE-REPLAY-STRATEGY."
                       (values :insert)))
                   (maybe-with-upgrade-restart)))
             (maybe-with-upgrade-restart ()
               (if upgrade-restart
                   (restart-case
                       (maybe-with-continue-restart)
                     (replay-force-upgrade ()
                       :report "Force :UPGRADE as JRN:@THE-REPLAY-STRATEGY."
                       (values :upgrade)))
                   (maybe-with-continue-restart)))
             (maybe-with-continue-restart ()
               (if continue-restart
                   (cerror "Ignore this condition." condition)
                   (error condition))))
          (declare (inline maybe-with-insert-restart
                           maybe-with-upgrade-restart
                           maybe-with-continue-restart))
          (maybe-with-insert-restart))))))

(define-condition replay-name-mismatch (replay-failure)
  ()
  (:documentation "Signaled when the new event's and REPLAY-EVENT's
  EVENT-NAME are not EQUAL. The REPLAY-FORCE-INSERT,
  REPLAY-FORCE-UPGRADE restarts are provided.")
  (:report (lambda (condition stream)
             (format stream "~@<The names of the new event ~S and ~
                            the REPLAY-EVENT ~S (at READ-POSITION ~A) ~
                            are not EQUAL.~:@>"
                     (replay-failure-new-event condition)
                     (replay-failure-replay-event condition)
                     (replay-failure-replay-position condition)))))

(define-condition replay-version-downgrade (replay-failure)
  ()
  (:documentation "Signaled when the new event and the REPLAY-EVENT
  have the same EVENT-NAME, but the new event has a lower version. The
  REPLAY-FORCE-UPGRADE restart is provided.")
  (:report (lambda (condition stream)
             (format stream "~@<The new event ~S has a lower :VERSION than ~
                            the REPLAY-EVENT ~S (at READ-POSITION ~A).~:@>"
                     (replay-failure-new-event condition)
                     (replay-failure-replay-event condition)
                     (replay-failure-replay-position condition)))))

(define-condition replay-args-mismatch (replay-failure)
  ()
  (:documentation "Signaled when the new event's and REPLAY-EVENT's
  EVENT-ARGS are not EQUAL. The REPLAY-FORCE-UPGRADE restart is
  provided.")
  (:report (lambda (condition stream)
             (format stream "~@<The :ARGS of the new event ~S ~
                            and the REPLAY-EVENT ~S (at position ~A) ~
                            are not EQUAL.~:@>"
                     (replay-failure-new-event condition)
                     (replay-failure-replay-event condition)
                     (replay-failure-replay-position condition)))))

(define-condition replay-outcome-mismatch (replay-failure)
  ()
  (:documentation "Signaled when the new event's and REPLAY-EVENT's
  EVENT-EXIT and/or EVENT-OUTCOME are not EQUAL. The
  REPLAY-FORCE-UPGRADE restart is provided.")
  (:report (lambda (condition stream)
             (format stream "~@<The EXITs and OUTCOMEs of the new event ~S ~
                            and the REPLAY-EVENT ~S (at position ~A) ~
                            are not equal.~:@>"
                     (replay-failure-new-event condition)
                     (replay-failure-replay-event condition)
                     (replay-failure-replay-position condition)))))

(define-condition replay-unexpected-outcome (replay-failure)
  ()
  (:documentation "Signaled when the new event has an
  UNEXPECTED-OUTCOME. Note that the REPLAY-EVENT always has an
  EXPECTED-OUTCOME due to the logic of RECORD-UNEXPECTED-OUTCOME. No
  restarts are provided.")
  (:report (lambda (condition stream)
             (format stream
                     "~@<The new event ~S has an unexpected outcome while the ~
                     REPLAY-EVENT ~S (at position ~A) has not.~:@>"
                     (replay-failure-new-event condition)
                     (replay-failure-replay-event condition)
                     (replay-failure-replay-position condition)))))

(define-condition replay-incomplete (replay-failure)
  ()
  (:documentation "Signaled if there are unprocessed non-log events in
  REPLAY-JOURNAL when WITH-JOURNALING finishes and the body of
  WITH-JOURNALING returned normally, which is to prevent this
  condition to cancel an ongoing unwinding. No restarts are
  provided.")
  (:report (lambda (condition stream)
             (format stream "~@<Replay incomplete: there are unprocessed ~
                            events left in ~S. The next one is ~S ~
                            (at position ~A).~:@>"
                     (replay-failure-replay-journal condition)
                     (replay-failure-replay-event condition)
                     (replay-failure-replay-position condition)))))

(define-restart replay-force-insert ()
  "This restart forces @THE-REPLAY-STRATEGY to be :INSERT, overriding
  REPLAY-NAME-MISMATCH. This is intended for upgrades, and extreme
  care must be taken not to lose data.")

(define-restart replay-force-upgrade ()
  "This restart forces @THE-REPLAY-STRATEGY to be :UPGRADE, overriding
  REPLAY-NAME-MISMATCH, REPLAY-VERSION-DOWNGRADE,
  REPLAY-ARGS-MISMATCH, REPLAY-OUTCOME-MISMATCH. This is intended for
  upgrades, and extreme care must be taken not to lose data.")


(defsection @upgrades-and-replay (:title "Upgrades and replay")
  "The replay mechanism is built on the assumption that the tree of
  @FRAMEs is the same when the code is replayed as it was when the
  replay journal was originally recorded. Thus, non-deterministic
  control flow poses a challenge, but non-determinism can be isolated
  with EXTERNAL-EVENTs. However, when the code changes, we might find
  the structure of frames in previous recordings hard to accommodate.
  In this case, we might decide to alter the structure, giving up some
  of the safety provided by the replay mechanism. There are various
  tools at our disposal to control this tradeoff between safety and
  flexibility:

  - We can insert individual frames with JOURNALED's INSERTABLE,
    upgrade frames by bumping JOURNALED's VERSION, and filter frames
    with WITH-REPLAY-FILTER. This option allows for the most
    consistency checks.

  - The REPLAY-FORCE-UPGRADE and REPLAY-FORCE-INSERT restarts allow
    overriding @THE-REPLAY-STRATEGY, but their use requires great care
    to be taken.

  - Or we may decide to keep the bare minimum of the replay journal
    around, and discard everything except for EXTERNAL-EVENTs. This
    option is equivalent to

          (let ((*force-insertable* t))
            (with-replay-filter (:patterns '((:name nil)))
              42))

  - Rerecording the journal without replay might be another option if
    there are no EXTERNAL-EVENTs to worry about.

  - Finally, we can rewrite the replay journal using the low-level
    interface (see @STREAMLETS-REFERENCE). In this case, extreme care
    must be taken not to corrupt the journal (and lose data) as there
    are no consistency checks to save us.

  With that, let's see how WITH-REPLAY-FILTER works."
  (with-replay-streamlet macro)
  (peek-replay-event function)
  (with-replay-filter macro))

(defmacro with-replay-streamlet ((var) &body body)
  "Open REPLAY-JOURNAL for reading with WITH-OPEN-JOURNAL set the
  READ-POSITION on it to the event next read by the @REPLAY
  mechanism (which is never a LOG-EVENT). The low-level
  @READING-FROM-STREAMLETS api is then available to inspect the
  contents of the replay. It is an error if REPLAY-JOURNAL is NIL."
  `(with-open-journal (,var (replay-journal) :direction :input)
     (setf (read-position ,var) (read-position *replay-streamlet*))
     ,@body))

(defun peek-replay-event ()
  "Return the REPLAY-EVENT to be read from REPLAY-JOURNAL. This is
  roughly equivalent to

  ```
  (when (replay-journal)
    (with-replay-streamlet (streamlet)
      (peek-event streamlet))
  ```

  Imagine a business process for paying an invoice. In the first
  version of this process, we just pay the invoice:

  ```
  (replayed (pay))
  ```

  We have left the implementation of PAY blank. In the second version,
  we need to get an approval first:

  ```
  (when (replayed (get-approval)
          (= (random 2) 0))
    (replayed (pay)))
  ```

  Replaying a journal produced by the first version of the code with
  the second version would run into difficulties because inserting
  EXTERNAL-EVENTs is tricky.

  We have to first decide how to handle the lack of approval in the
  first version. Here, we just assume the processes started by the
  first version get approval automatically. The implementation is
  based on a dummy `PROCESS` block whose version is bumped when the
  payment process changes and is inspected at the start of journaling.

  When v1 is replayed with v2, we introduce an INSERTABLE, versioned
  `GET-APPROVAL` block that just returns T. When replaying the code
  again, still with v2, the `GET-APPROVAL` block will be upgraded to
  :INFINITY.

  ```
  (let ((bundle (make-in-memory-bundle)))
    ;; First version of the payment process. Just pay.
    (with-bundle (bundle)
      (checked (process :version 1))
      (replayed (pay)))
    ;; Second version of the payment process. Only pay if approved.
    (loop repeat 2 do
      (with-bundle (bundle)
        (let ((replay-process-event (peek-replay-event)))
          (checked (process :version 2))
          (when (if (and replay-process-event
                         (< (event-version replay-process-event) 2))
                    ;; This will be upgraded to :INFINITY the second
                    ;; time around the LOOP.
                    (checked (get-approval :insertable t)
                      t)
                    (replayed (get-approval)
                      (= (random 2) 0)))
            (replayed (pay)))))))
  ```"
  (when *replay-streamlet*
    (peek-event *replay-streamlet*)))

(defmacro with-replay-filter ((&key patterns) &body body)
  "If there is a REPLAY-JOURNAL, then, in addition to filtering out
  LOG-EVENTs (which happens all time during replay), filter out all
  events that belong to descendant frames that match any of PATTERNS.
  Filtered out events are never seen by JOURNALED as it replays
  events. All patterns are of the format `(&KEY NAME VERSION<)`, where
  VERSION< is a valid [EVENT-VERSION][type], and NAME may be NIL,
  which acts as a wildcard.

  WITH-REPLAY-FILTER affects only the immediately enclosing
  WITH-JOURNALING. A WITH-REPLAY-FILTER nested within another in the
  same WITH-JOURNALING inherits PATTERNS of its parent, to which it
  add its own PATTERNS.

  Examples:

  ```
  ;; Match events with name FOO and version 1, 2, 3 or 4
  (:name foo :version< 5)
  ;; Match events with name BAR and any version
  (:name bar :version< :infinity)
  ;; Same as the previous
  (:name bar)
  ;; Match all names
  (:name nil)
  ;; Same as the previous
  ()
  ```

  Filtering can be thought of as removing nodes of the tree of frames,
  connecting its children to its parent. The following example removes
  frames `J1` and `J2` from around `J3`, the `J1` frame from within
  `J3`, and the third `J1` frame.

  ```
  (let ((journal (make-in-memory-journal)))
    ;; Record trees J1 -> J2 -> J3 -> J1, and J1.
    (with-journaling (:record journal)
      (checked (j1)
        (checked (j2)
          (checked (j3)
            (checked (j1)
              42))))
      (checked (j1)
        7))
    ;; Filter out all occurrences of VERSIONED-EVENTs named J1 and
    ;; J2 from the replay, leaving only J3 to match.
    (with-journaling (:replay journal :record t :replay-eoj-error-p t)
      (with-replay-filter (:patterns '((:name j1) (:name j2)))
        (checked (j3)
          42))))
  ```

  WITH-REPLAY-FILTER is intended to assist in upgrades where some
  JOURNALED @BLOCKs are removed from the code, which would render
  replaying previously recorded journals impossible. Note that, for
  reasons of safety, it is not possible to filter EXTERNAL-EVENTs.

  For how to add new blocks in a code upgrade, see JOURNALED's
  :INSERTABLE argument."
  (alexandria:with-gensyms (with-filtering-body eat pred depth)
    `(flet ((,with-filtering-body () ,@body))
       (declare (dynamic-extent #',with-filtering-body))
       (if *replay-streamlet*
           (let* ((*patterns* (append *patterns* ,patterns))
                  (,pred (patterns-to-disjunction *patterns*))
                  (,depth (%depth *replay-streamlet*)))
             (flet ((,eat ()
                      (eat-events ,pred *replay-streamlet* ,depth)))
               (unwind-protect
                    (let ((*skip-events* #',eat))
                      (with-journaling-failure-on-nlx
                        (skip-events-and-maybe->recording
                         *record-streamlet* *replay-streamlet*))
                      (,with-filtering-body))
                 ;; Being in a cleanup form, be conservative about
                 ;; potentially piling more errors on top of
                 ;; unrecoverable ones.
                 (unless *journaling-failure*
                   (with-journaling-failure-on-nlx
                     ;; When the whole frame is filterable, sometimes
                     ;; it is ambiguous whether the replay events
                     ;; would be within the dynamic extent of
                     ;; WITH-REPLAY-FILTER because there is no
                     ;; unfiltered event to follow them. In this case,
                     ;; we choose to greedily filter the events.
                     (eat-full-frames-of-events ,pred *replay-streamlet* ,depth)
                     ;; If there is an enclosing WITH-REPLAY-FILTER
                     ;; without an intervening JOURNALED, then its
                     ;; *SKIP-EVENTS* filter must be run. This also
                     ;; performs the :REPLAYING -> :RECORDING
                     ;; transition.
                     (skip-events-and-maybe->recording
                      *record-streamlet* *replay-streamlet*))))))
           (,with-filtering-body)))))

(defvar *patterns* ())

(defun patterns-to-disjunction (patterns)
  (let ((preds (mapcar #'pattern-to-pred patterns)))
    (lambda (event)
      (loop for pred in preds
              thereis (funcall pred event)))))

(defun pattern-to-pred (pattern)
  (destructuring-bind (&key name version<) pattern
    (check-type name (not null))
    (check-type version< event-version)
    (lambda (event)
      (and (or (null name)
               (equal (event-name event) name))
           (version< (event-version event)
                     (or version< :infinity))))))

;;; Consume all consecutive log-events and those that match PRED.
(defun eat-events (pred streamlet base-depth)
  (loop
    (let ((depth (%depth streamlet))
          (event (peek-event streamlet)))
      (cond ((and event
                  ;; Filter LOG-EVENTs and those matching PRED unless
                  ;; DEPTH would go below BASE-DEPTH.
                  (or (log-event-p event)
                      (funcall pred event))
                  (or (< base-depth depth)
                      (not (out-event-p event))))
             (read-event streamlet)
             (when (= base-depth (%depth streamlet))
               (return)))
            (t
             (return))))))

(defun eat-full-frames-of-events (pred streamlet base-depth)
  (if (= (%depth streamlet) base-depth)
      (loop for read-position = (every-event-in-frame pred streamlet)
            while read-position
            do (setf (read-position streamlet) read-position))
      (assert (eq (journal-state (record-journal)) :mismatched))))

;;; Return true if PRED is true for every event in the non-empty frame
;;; next read from STREAMLET.
;;;
;;; FIXME: PRED vs LOG-EVENTs?
(defun every-event-in-frame (pred streamlet)
  (save-excursion (streamlet)
    (let ((end-of-frame-position (end-of-frame-position streamlet :depth 0)))
      (when end-of-frame-position
        (loop for event = (read-event streamlet)
              do (unless (funcall pred event)
                   (return-from every-event-in-frame nil))
              while (<= (read-position streamlet) end-of-frame-position))
        (return-from every-event-in-frame (read-position streamlet))))))


(defsection @testing (:title "Testing")
  """Having discussed the @REPLAY mechanism, next are @TESTING and
  @PERSISTENCE, which rely heavily on replay. Suppose we want to unit
  test user registration. Unfortunately, the code communicates with a
  database service and also takes input from the user. A natural
  solution is to create [mocks][mock-object] for these external
  systems to unshackle the test from the cumbersome database
  dependency and to allow it to run without user interaction.

  We do this below by wrapping external interaction in JOURNALED with
  :VERSION :INFINITY (see @REPLAYING-THE-OUTCOME).

  ```
  (defparameter *db* (make-hash-table))

  (defun set-key (key value)
    (replayed ("set-key" :args `(,key ,value))
      (format t "Updating db~%")
      (setf (gethash key *db*) value)
      nil))

  (defun get-key (key)
    (replayed ("get-key" :args `(,key))
      (format t "Query db~%")
      (gethash key *db*)))

  (defun ask-username ()
    (replayed ("ask-username")
      (format t "Please type your username: ")
      (read-line)))

  (defun maybe-win-the-grand-prize ()
    (checked ("maybe-win-the-grand-prize")
      (when (= 1000000 (hash-table-count *db*))
        (format t "You are the lucky one!"))))

  (defun register-user (username)
    (unless (get-key username)
      (set-key username `(:user-object :username ,username))
      (maybe-win-the-grand-prize)))
  ```

  Now we write a test that records these interactions in a file when
  it's run for the first time.

  ```
  (define-file-bundle-test (test-user-registration
                            :directory (asdf:system-relative-pathname
                                        :journal "test/registration/"))
    (let ((username (ask-username)))
      (register-user username)
      (assert (get-key username))
      (register-user username)
      (assert (get-key username))))

  ;; Original recording: everything is executed
  JRN> (test-user-registration)
  Please type your username: joe
  Query db
  Updating db
  Query db
  Query db
  Query db
  => NIL
  ```

  On reruns, none of the external stuff is executed. The return values
  of the external JOURNALED blocks are replayed from the journal:

  ```
  ;; Replay: all external interactions are mocked.
  JRN> (test-user-registration)
  => NIL
  ```

  Should the code change, we might want to upgrade carefully (see
  @UPGRADES-AND-REPLAY) or just rerecord from scratch:

  ```
  JRN> (test-user-registration :rerecord t)
  Please type your username: joe
  Query db
  Updating db
  Query db
  Query db
  Query db
  => NIL
  ```

  Thus satisfied that our test runs, we can commit the journal file in
  the bundle into version control. Its contents are:

  ```

  (:IN "ask-username" :VERSION :INFINITY)
  (:OUT "ask-username" :VERSION :INFINITY :VALUES ("joe" NIL))
  (:IN "get-key" :VERSION :INFINITY :ARGS ("joe"))
  (:OUT "get-key" :VERSION :INFINITY :VALUES (NIL NIL))
  (:IN "set-key" :VERSION :INFINITY :ARGS ("joe" (:USER-OBJECT :USERNAME "joe")))
  (:OUT "set-key" :VERSION :INFINITY :VALUES (NIL))
  (:IN "maybe-win-the-grand-prize" :VERSION 1)
  (:OUT "maybe-win-the-grand-prize" :VERSION 1 :VALUES (NIL))
  (:IN "get-key" :VERSION :INFINITY :ARGS ("joe"))
  (:OUT "get-key" :VERSION :INFINITY :VALUES ((:USER-OBJECT :USERNAME "joe") T))
  (:IN "get-key" :VERSION :INFINITY :ARGS ("joe"))
  (:OUT "get-key" :VERSION :INFINITY :VALUES ((:USER-OBJECT :USERNAME "joe") T))
  (:IN "get-key" :VERSION :INFINITY :ARGS ("joe"))
  (:OUT "get-key" :VERSION :INFINITY :VALUES ((:USER-OBJECT :USERNAME "joe") T))
  ```

  Note that when this journal is replayed, new VERSIONED-EVENTs are
  required to match the replay. So after the original recording, we
  can check by eyeballing that the record represents a correct
  execution. Then on subsequent replays, even though
  `MAYBE-WIN-THE-GRAND-PRIZE` sits behind `REGISTER-USER` and is hard
  to test with ASSERTs, the replay mechanism verifies that it is
  called only for new users.

  This record-and-replay style of testing is not the only possibility:
  direct inspection of a journal with the low-level events api (see
  @EVENTS-REFERENCE) can facilitate checking non-local invariants.
  """
  (define-file-bundle-test macro))

(defmacro define-file-bundle-test ((name &key directory (equivalentp t))
                                   &body body)
  "Define a function with NAME for record-and-replay testing. The
  function's BODY is executed in a WITH-BUNDLE to guarantee
  replayability. The bundle in question is a FILE-BUNDLE created in
  DIRECTORY. The function has a single keyword argument, RERECORD. If
  RERECORD is true, the bundle is deleted with DELETE-FILE-BUNDLE to
  start afresh.

  Furthermore, if BODY returns normally, and it is a replay of a
  previous run, and EQUIVALENTP, then it is ASSERTed that the record
  and replay journals are EQUIVALENT-REPLAY-JOURNALS-P. If this check
  fails, RECORD-JOURNAL is discarded when the function returns. In
  addition to the replay consistency, this checks that no inserts or
  upgrades were performed (see @THE-REPLAY-STRATEGY)."
  (alexandria:with-gensyms (dir)
    `(defun ,name (&key rerecord)
       (let ((,dir ,directory))
         (when rerecord
           (delete-file-bundle ,dir))
         (with-bundle ((make-file-bundle ,dir :max-n-failed 0))
           (multiple-value-prog1
               (progn ,@body)
             (when (and (replay-journal) ,equivalentp)
               (unless (equivalent-replay-journals-p (record-journal)
                                                     (replay-journal))
                 ;; KLUDGE: This is not a normal transition and it is
                 ;; not reflected in the file. However this is enough
                 ;; to make WITH-BUNDLE reap the journal due to the
                 ;; :MAX-N-FAILED 0 above.
                 (setf (%state (record-journal)) :failed)
                 (slot-makunbound *record-streamlet*
                                  'completed-on-abort-deferred-p)
                 (assert nil ()
                         "~@<The record ~S and replay ~S journals are not ~
                         equivalent.~:@>"
                         (pathname-of (record-journal))
                         (pathname-of (replay-journal)))))))))))


(defsection @persistence (:title "Persistence")
  (@persistence-tutorial section)
  (@synchronization section))

(defsection @persistence-tutorial (:title "Persistence tutorial")
  """Let's write a simple game.

  ```
  (defun play-guess-my-number ()
    (let ((my-number (replayed (think-of-a-number)
                       (random 10))))
      (format t "~%I thought of a number.~%")
      (loop for i upfrom 0 do
        (write-line "Guess my number:")
        (let ((guess (replayed (read-guess)
                       (values (parse-integer (read-line))))))
          (format t "You guessed ~D.~%" guess)
          (when (= guess my-number)
            (checked (game-won :args `(,(1+ i))))
            (format t "You guessed it in ~D tries!" (1+ i))
            (return))))))

  (defparameter *the-evergreen-game* (make-in-memory-bundle))
  ```

  ##### Original recording

  Unfortunately, the implementation is lacking in the input validation
  department. In the transcript below, PARSE-INTEGER fails with `junk
  in string` when the user enters `not a number`:

  ```
  CL-USER> (handler-case
               (with-bundle (*the-evergreen-game*)
                 (play-guess-my-number))
             (error (e)
               (format t "Oops. ~A~%" e)))
  I thought of a number.
  Guess my number:
  7 ; real user input
  You guessed 7.
  Guess my number:
  not a number ; real user input
  Oops. junk in string "not a number"
  ```

  ##### Replay and extension

  Instead of fixing this bug, we just restart the game from the
  beginning, @REPLAYING-THE-OUTCOME of external interactions marked
  with REPLAYED:

  ```
  CL-USER> (with-bundle (*the-evergreen-game*)
             (play-guess-my-number))
  I thought of a number.
  Guess my number:
  You guessed 7.
  Guess my number: ; New recording starts here
  5 ; real user input
  You guessed 5.
  Guess my number:
  4 ; real user input
  You guessed 4.
  Guess my number:
  2 ; real user input
  You guessed 2.
  You guessed it in 4 tries!
  ```

  ##### It's evergreen

  We can now replay this game many times without any user interaction:

  ```
  CL-USER> (with-bundle (*the-evergreen-game*)
             (play-guess-my-number))
  I thought of a number.
  Guess my number:
  You guessed 7.
  Guess my number:
  You guessed 5.
  Guess my number:
  You guessed 4.
  Guess my number:
  You guessed 2.
  You guessed it in 4 tries!
  ```

  ##### The generated events

  This simple mechanism allows us to isolate external interactions and
  write tests in record-and-replay style based on the events produced:

  ```
  CL-USER> (list-events *the-evergreen-game*)
  ((:IN THINK-OF-A-NUMBER :VERSION :INFINITY)
   (:OUT THINK-OF-A-NUMBER :VERSION :INFINITY :VALUES (2))
   (:IN READ-GUESS :VERSION :INFINITY)
   (:OUT READ-GUESS :VERSION :INFINITY :VALUES (7))
   (:IN READ-GUESS :VERSION :INFINITY :ARGS NIL)
   (:OUT READ-GUESS :VERSION :INFINITY :VALUES (5))
   (:IN READ-GUESS :VERSION :INFINITY :ARGS NIL)
   (:OUT READ-GUESS :VERSION :INFINITY :VALUES (4))
   (:IN READ-GUESS :VERSION :INFINITY :ARGS NIL)
   (:OUT READ-GUESS :VERSION :INFINITY :VALUES (2))
   (:IN GAME-WON :VERSION 1 :ARGS (4))
   (:OUT GAME-WON :VERSION 1 :VALUES (NIL)))
  ```

  In fact, being able to replay this game at all already checks it
  through the `GAME-WON` event that the number of tries calculation is
  correct.

  In addition, thus being able to reconstruct the internal state of
  the program gives us persistence by replay. If instead of a
  IN-MEMORY-BUNDLE, we used a FILE-BUNDLE, the game would have been
  saved on disk without having to write any code for saving and
  loading the game state.

  ##### Discussion

  Persistence by replay, also known as [Event
  Sourcing][event-sourcing], is appropriate when the external
  interactions are well-defined and stable. Storing events shines in
  comparison to persisting state when the control flow is too
  complicated to be interrupted and resumed easily. Resuming execution
  in deeply nested function calls is fraught with such peril that it
  is often easier to flatten the program into a state machine, which
  is as pleasant as manually managing [continuations][continuation].

  [continuation]: https://en.wikipedia.org/wiki/Continuation

  In contrast, the Journal library does not favour certain styles of
  control flow, and only requires that non-determinism is packaged up
  in REPLAYED, which allows it to reconstruct the state of the program
  from the recorded events at any point during its execution and
  resume from there.
  """)

(defsection @synchronization (:title "Synchronization to storage")
  "In the following, we explore how journals can serve as a
  persistence mechanism and the guarantees they offer. The high-level
  summary is that journals with SYNC can serve as a durable and
  consistent storage medium. The other two
  [ACID](https://en.wikipedia.org/wiki/ACID) properties, atomicity and
  isolation, do not apply because Journal is single-client and does
  not need transactions."
  (aborted-execution glossary-term)
  (data-event glossary-term)
  (@synchronization-strategies section)
  (@synchronization-with-in-memory-journals section)
  (@synchronization-with-file-journals section))

(define-glossary-term aborted-execution (:title "aborted execution")
  "Aborted execution is when the operating system or the application
  crashes, calls `abort()`, is killed by a `SIGKILL` signal or there
  is a power outage. Synchronization guarantees are defined in face of
  aborted execution and do not apply to hardware errors, Lisp or OS
  bugs.")

(define-glossary-term data-event (:title "data event")
  "Data events are the only events that may be non-deterministic. They
  record information which could change if the same code were run
  multiple times. Data events typically correspond to interactions
  with the user, servers or even the random number generator. Due to
  their non-determinism, they are the only parts of the journal not
  reproducible by rerunning the code. In this sense, only data events
  are not redundant with the code and whether other events are
  persisted does not affect durability. There are two kinds of data
  events:

  - An EXTERNAL-EVENT that is also an OUT-EVENT.

  - The IN-EVENT of an INVOKED function, which lies outside the
    normal, deterministic control flow.")

;;; These functions test for :RECORDING, thus must be called after
;;; reading the replay event and calling SKIP-EVENTS.
(defun maybe-sync-after-in-event (in-event record-streamlet)
  (when (and record-streamlet
             (slot-value record-streamlet 'sync)
             (eq *record-journal-state* :recording)
             (versioned-event-p in-event)
             (invoked-function (event-name in-event)))
    (sync-streamlet record-streamlet)))

(defun maybe-sync-after-out-event (out-event record-streamlet)
  (when (and record-streamlet
             (slot-value record-streamlet 'sync)
             (external-event-p out-event)
             (eq *record-journal-state* :recording))
    (sync-streamlet record-streamlet)))

(defsection @synchronization-strategies (:title "Synchronization strategies")
  "When a journal or bundle is created (see MAKE-IN-MEMORY-JOURNAL,
  MAKE-FILE-JOURNAL, MAKE-IN-MEMORY-BUNDLE, MAKE-FILE-BUNDLE), the
  SYNC option determines when - as a RECORD-JOURNAL - the recorded
  events and JOURNAL-STATE changes are persisted durably. For
  FILE-JOURNALs, persisting means calling something like `fsync()`,
  while for IN-MEMORY-JOURNALs, a user defined function is called to
  persist the data.

  - NIL: Never synchronize. A FILE-JOURNAL's file may be corrupted on
    ABORTED-EXECUTION. In IN-MEMORY-JOURNALs, SYNC-FN is never called.

  - T: This is the _no data loss_ setting with minimal
    synchronization. It guarantees _consistency_ (i.e. no corruption)
    and _durability_ up to the most recent DATA-EVENT written in
    JOURNAL-STATE :RECORDING or for the entire record journal in
    states :FAILED and :COMPLETED. :FAILED or :COMPLETED is guaranteed
    when leaving WITH-JOURNALING at the latest.

  - Values other than NIL and T are reserved for future extensions.
    Using them triggers a JOURNAL-ERROR.")

(defun check-sync-value (sync)
  (check-type sync (member nil t)))

(defun sync-journal (&optional (journal (record-journal)))
  "Durably persist all preceding writes made to JOURNAL during an
  enclosing WITH-JOURNALING or via LOG-RECORD from any thread. Writes
  made in a WITH-JOURNALING in another thread are not persisted. The
  changes in question are WRITE-EVENT calls and state changes. This is
  a noop JOURNAL-SYNC is NIL. This function is safe to call from any
  thread."
  (check-type journal journal)
  (when (slot-value journal 'sync)
    (if (eq journal (record-journal))
        ;; Thread-safe because we are in WITH-JOURNALING, and it is
        ;; the only writer.
        (with-journaling-failure-on-nlx
          (sync-streamlet *record-streamlet*))
        ;; The only other possible writer is LOG-RECORD via
        ;; JOURNAL-OUTPUT-STREAMLET and that acquires the journal lock
        ;; for writes.
        (with-journal-locked (journal)
          (when (%output-streamlet-of journal)
            ;; Since we are holding the journal lock, we can be sure
            ;; that no WRITE-EVENTs can occur through LOG-RECORD and
            ;; state changes never happen through that mechanism.
            ;; There are no more kinds of concurrent writes to worry
            ;; about except direct STREAMLET access, which we ignore.
            (sync-streamlet (%output-streamlet-of journal)))))))

(defsection @synchronization-with-in-memory-journals
    (:title "Synchronization with in-memory journals")
  """Unlike FILE-JOURNALs, IN-MEMORY-JOURNALs do not have any built-in
  persistent storage backing them, but with SYNC-FN, persistence can
  be tacked on. If non-NIL, SYNC-FN must be a function of a single
  argument, an IN-MEMORY-JOURNAL. SYNC-FN is called according to
  @SYNCHRONIZATION-STRATEGIES, and upon normal return the journal must
  be stored durably.

  The following example saves the entire journal history when a new
  DATA-EVENT is recorded. Note how `SYNC-TO-DB` is careful to
  overwrite `*DB*` only if it is called with a journal that has not
  failed the replay (as in @REPLAY-FAILUREs) and is sufficiently
  different from the replay journal as determined by
  JOURNAL-DIVERGENT-P.

  ```
  (defparameter *db* ())

  (defun sync-to-db (journal)
    (when (and (member (journal-state journal)
                       '(:recording :logging :completed))
               (journal-divergent-p journal))
      (setq *db* (journal-events journal))
      (format t "Saved ~S~%New events from position ~S~%" *db*
              (journal-previous-sync-position journal))))

  (defun make-db-backed-record-journal ()
    (make-in-memory-journal :sync-fn 'sync-to-db))

  (defun make-db-backed-replay-journal ()
    (make-in-memory-journal :events *db*))

  (with-journaling (:record (make-db-backed-record-journal)
                    :replay (make-db-backed-replay-journal))
    (replayed (a)
      2)
    (ignore-errors
      (replayed (b)
        (error "Whoops"))))
  .. Saved #((:IN A :VERSION :INFINITY)
  ..         (:OUT A :VERSION :INFINITY :VALUES (2)))
  .. New events from position 0
  .. Saved #((:IN A :VERSION :INFINITY)
  ..         (:OUT A :VERSION :INFINITY :VALUES (2))
  ..         (:IN B :VERSION :INFINITY)
  ..         (:OUT B :ERROR ("SIMPLE-ERROR" "Whoops")))
  .. New events from position 2
  ..
  ```

  In a real application, external events often involve unreliable or
  high-latency communication. In the above example, block `B` signals
  an error, say, to simulate some kind of network condition. Now a new
  journal _for replay_ is created and initialized with the saved
  events and the whole process is restarted.

  ```
  (defun run-with-db ()
    (with-journaling (:record (make-db-backed-record-journal)
                      :replay (make-db-backed-replay-journal))
      (replayed (a)
        (format t "A~%")
        2)
      (replayed (b)
        (format t "B~%")
        3)))

  (run-with-db)
  .. B
  .. Saved #((:IN A :VERSION :INFINITY)
  ..         (:OUT A :VERSION :INFINITY :VALUES (2))
  ..         (:IN B :VERSION :INFINITY)
  ..         (:OUT B :VERSION :INFINITY :VALUES (3)))
  .. New events from position 0
  ..
  => 3
  ```

  Note that on the rerun, block `A` is not executed because external
  events are replayed simply by reproducing their outcome, in this
  case returning 2. See @REPLAYING-THE-OUTCOME. Block `B`, on the
  other hand, was rerun because it had an UNEXPECTED-OUTCOME the first
  time around. This time it ran without error, a DATA-EVENT was
  triggered and SYNC-FN invoked.

  If we were to invoke the now completed `RUN-WITH-DB` again, it would
  simply return 3 without ever invoking SYNC-FN:

  ```
  (run-with-db)
  => 3
  ```

  With JOURNAL-REPLAY-MISMATCH, SYNC-FN can be optimized to to reuse
  the sequence of events in the replay journal up until the point of
  divergence.
  """)


(defsection @in-memory-journals (:title "In-memory journals")
  (in-memory-journal class)
  (make-in-memory-journal function)
  (journal-events (reader in-memory-journal))
  (journal-previous-sync-position (reader in-memory-journal)))

(defclass in-memory-journal (journal)
  ((events
    :initarg :events :reader journal-events :reader events
    :documentation "A sequence of events in the journal. Not to be
    mutated by client code.")
   (sync-fn :initform nil :initarg :sync-fn)
   (previous-sync-position
    :initform 0 :reader journal-previous-sync-position
    :documentation "The length of JOURNAL-EVENTS at the time of the
    most recent invocation of SYNC-FN."))
  (:documentation "IN-MEMORY-JOURNALs are backed by a non-persistent,
  Lisp array of events. Much quicker than FILE-JOURNALs, they are
  ideal for smallish journals persisted manually (see
  @SYNCHRONIZATION-WITH-IN-MEMORY-JOURNALS for an example).

  They are also useful for writing tests based on what events were
  generated. They differ from FILE-JOURNALs in that events written to
  IN-MEMORY-JOURNALs are not serialized (and deserialized on replay)
  with the following consequences for the objects recorded by
  JOURNALED (i.e. its NAME, ARGS arguments, but also the return VALUES
  of the block, or the value returned by CONDITION):

  - These objects need not be @READABLE.

  - Their identity (`EQ`ness) is not lost.

  - They must __must not be mutated__ in any way."))

(defmethod print-object ((journal in-memory-journal) stream)
  (print-unreadable-object (journal stream :type t)
    (format stream "~S ~S ~S" (ignore-errors (journal-state journal))
            :n-events (ignore-errors (length (events journal))))))

(defmethod identical-journals-p ((journal-1 in-memory-journal)
                                 (journal-2 in-memory-journal))
  (and (= (length (events journal-1)) (length (events journal-2)))
       (every #'equal (events journal-1) (events journal-2))))

(defclass in-memory-streamlet (streamlet)
  ((%read-position :initform 0 :reader read-position)
   (peeked))
  (:documentation "A streamlet for performing IO on IN-MEMORY-JOURNALs."))

(declaim (inline %set-in-memory-streamlet-read-position))
(defun %set-in-memory-streamlet-read-position (streamlet read-position)
  (setf (slot-value streamlet '%read-position) read-position)
  (setf (slot-value streamlet 'peeked)
        (let ((events (events (journal streamlet))))
          (if (< read-position (length events))
              (aref events read-position)
              nil))))

(defmethod set-read-position ((streamlet in-memory-streamlet) read-position)
  (%set-in-memory-streamlet-read-position streamlet read-position))

(defun make-in-memory-journal (&key (events () eventsp) state
                               (sync nil syncp) sync-fn)
  "By default, creates an empty IN-MEMORY-JOURNAL in JOURNAL-STATE
  :NEW, which is suitable for recording. To make a replay journal, use
  :STATE :COMPLETED with some sequence of EVENTS:

  ```
  (make-in-memory-journal :events '((:in foo :version 1)) :state :completed)
  ```

  If the EVENTS argument is provided, then STATE defaults to :NEW,
  else to :COMPLETED.

  SYNC determines when SYNC-FN will be invoked on the RECORD-JOURNAL.
  SYNC defaults to T if SYNC-FN, else to NIL. For a description of
  possible values, see @SYNCHRONIZATION-STRATEGIES. For more
  discussion, see @SYNCHRONIZATION-WITH-IN-MEMORY-JOURNALS."
  (check-sync-value sync)
  (make-instance 'in-memory-journal
                 :state (or state (if eventsp :completed :new))
                 :events (if eventsp
                             (make-array (length events)
                                         :adjustable t :fill-pointer t
                                         :initial-contents events)
                             (make-array 14 :adjustable t :fill-pointer 0))
                 :sync (if syncp
                           sync
                           (if sync-fn t nil))
                 :sync-fn sync-fn))

(defmethod open-streamlet ((journal in-memory-journal) &key direction)
  (let ((streamlet (make-instance 'in-memory-streamlet :journal journal
                                  :direction direction)))
    (when (input-direction-p direction)
      ;; This sets up PEEKED.
      (set-read-position streamlet 0))
    streamlet))

(defmethod close-streamlet ((streamlet in-memory-streamlet)))

(defmethod make-streamlet-finalizer ((streamlet in-memory-streamlet))
  nil)

(defmethod peek-event ((streamlet in-memory-streamlet))
  (slot-value streamlet 'peeked))

(defmethod read-event ((streamlet in-memory-streamlet) &optional eoj-error-p)
  (let ((old-peeked (slot-value streamlet 'peeked)))
    (%set-in-memory-streamlet-read-position
     streamlet (1+ (slot-value streamlet '%read-position)))
    (or old-peeked
        (if eoj-error-p
            (error 'end-of-journal :journal (journal streamlet))
            nil))))

(defmethod write-event (event (streamlet in-memory-streamlet))
  (vector-push-extend event (events (journal streamlet))))

(defmethod write-position ((streamlet in-memory-streamlet))
  (length (events (journal streamlet))))

(defmethod request-completed-on-abort ((streamlet in-memory-streamlet))
  t)

(defmethod sync-streamlet ((streamlet in-memory-streamlet))
  (let* ((journal (journal streamlet))
         (sync-fn (slot-value journal 'sync-fn)))
    (when sync-fn
      (let ((prev-position (slot-value journal 'previous-sync-position))
            (events (events journal)))
        (unless (= prev-position (length events))
          (funcall sync-fn journal)
          (setf (slot-value journal 'previous-sync-position)
                (length events)))))))


(defsection @file-journals (:title "File journals")
  (file-journal class)
  (make-file-journal function)
  (pathname-of (reader file-journal)))

(defclass file-journal (journal)
  ((pathname
    :initarg :pathname :reader pathname-of
    :documentation "The pathname of the file backing the journal."))
  (:documentation """A FILE-JOURNAL is a journal whose contents and
  JOURNAL-STATE are persisted in a file. This is the [JOURNAL][class]
  subclass with out-of-the-box persistence, but see @FILE-BUNDLES for
  a more full-featured solution for repeated @REPLAYs.

  Since serialization in FILE-JOURNALs is built on top of Lisp READ
  and WRITE, everything that JOURNALED records in events (i.e. its
  NAME, ARGS arguments, but also the return VALUES of the block, or
  the value returned by CONDITION) must be @READABLE.

  File journals are human-readable, and editable by hand with some
  care. When editing, the following needs to be remembered:

  - The first character of the file represents its JOURNAL-STATE. It
    is a `#\Space` (for state :NEW, :REPLAYING, :MISMATCHED and
    :FAILED), or a `#\Newline` (for state :RECORDING, :LOGGING and
    :COMPLETED).

  - If the journal has SYNC (see @SYNCHRONIZATION-STRATEGIES), then in
    between events, there may be `#\Del` or `#\Ack`
    characters (CHAR-CODE 127 and 6). `#\Del` marks the end of the
    journal contents which may be read back: it's kind of an
    uncommitted-transaction marker for the events that follow it.
    `#\Ack` characters, of which there may be many in the file, mark
    the sequence of events until the next marker of either kind as
    valid (or committed). `#\Ack` characters are ignored when reading
    the journal.

  Thus, when editing a file, don't change the first character and
  leave the `#\Del` character, if any, where it is. Also see
  @SYNCHRONIZATION-WITH-FILE-JOURNALS."""))

(defmethod print-object ((journal file-journal) stream)
  (print-unreadable-object (journal stream :type t)
    (format stream "~S ~S" (ignore-errors (pathname-of journal))
            (ignore-errors (journal-state journal)))))

(defclass file-streamlet (streamlet)
  ((stream :initarg :stream :type (or stream null) :accessor %stream)
   ;; To support separate read and write positions, we need keep track
   ;; of whether the last operation was a read or a write, and if it
   ;; was a write where the read file position is.
   (wrote-last-p :initform nil :accessor wrote-last-p)
   (read-file-position :initform nil :accessor read-file-position)
   (peeked :initform nil)
   ;; For indenting the events in the file.
   (depth :initform 0 :accessor %depth)
   (txn-start-file-position :initform nil)
   (set-complete-on-abort-on-commit-p :initform nil)))

(defvar *truename-to-file-journal*
  (trivial-garbage:make-weak-hash-table :weakness :value :test #'equal))

(defvar *file-journal-lock* (bt:make-lock "file-journal-lock"))

(defun make-file-journal (pathname &key sync)
  "Return a FILE-JOURNAL backed by the file with PATHNAME. The file is
  created when the journal is opened for writing. For a description of
  SYNC, see @SYNCHRONIZATION-STRATEGIES.

  If there is already an existing FILE-JOURNAL backed by the same
  file, then that object is returned. If the existing object has
  different options (e.g. it has SYNC T while the SYNC argument is NIL
  here), then a JOURNAL-ERROR is signalled.

  If there is already an existing FILE-JOURNAL backed by the same
  file, the JOURNAL-STATE is not :NEW, but the file doesn't exist,
  then the existing object is __invalidated__: attempts to write will
  fail with JOURNAL-ERROR. If the existing journal object is being
  written, then invalidation fails with a JOURNAL-ERROR. After
  invalidation, a new FILE-JOURNAL object is created."
  (check-sync-value sync)
  (let ((truename (%truename pathname)))
    (bt:with-lock-held (*file-journal-lock*)
      ;; For example, if a BUNDLE's directory is blown away we'd have
      ;; its journals lingering here (with the wrong JOURNAL-STATE).
      ;; Make sure they are superseeded safely.
      (let ((existing (gethash truename *truename-to-file-journal*)))
        (when (and existing
                   (or (and (not (probe-file truename))
                            (not (eq (journal-state existing) :new)))
                       ;; Notice manual edits to journal state, too.
                       (and (probe-file truename)
                            (not (eq (journal-state existing)
                                     (read-file-journal-state truename))))))
          (invalidate-journal existing)
          (remhash truename *truename-to-file-journal*)))
      (or (check-file-journal-options
           (gethash truename *truename-to-file-journal*) sync)
          (setf (gethash truename *truename-to-file-journal*)
                (let ((state (if (probe-file truename)
                                 (read-file-journal-state truename)
                                 :new)))
                  (make-instance 'file-journal :state state
                                 :pathname pathname :sync sync)))))))

(defun check-file-journal-options (journal sync)
  (when journal
    (unless (eq (slot-value journal 'sync) sync)
      (error 'journal-error :journal journal
             :format-control "~@<Incompatible options given for ~S: ~
                             ~S ~S.~:@>"
             :format-args (list journal :sync sync)))
    journal))

(defun delete-file-journal (journal)
  (invalidate-journal journal)
  (bt:with-lock-held (*file-journal-lock*)
    (remhash (pathname-of journal) *truename-to-file-journal*)
    (delete-file (pathname-of journal))))

(defun %truename (pathname)
  (ensure-directories-exist pathname)
  (let ((dir-truename (truename (directory-namestring pathname))))
    (and dir-truename (merge-pathnames (file-namestring pathname)
                                       dir-truename))))

(defmethod open-streamlet ((journal file-journal) &key direction)
  (let* ((stream (open (pathname-of journal)
                       :direction direction
                       ;; Only applies to :OUTPUT or :IO.
                       :if-exists :error
                       :if-does-not-exist (if (input-direction-p direction)
                                              :error
                                              :create)))
         (streamlet (make-instance 'file-streamlet
                                   :journal journal
                                   :direction direction
                                   :stream stream
                                   :sync (slot-value journal 'sync))))
    (setf (wrote-last-p streamlet) nil)
    (setf (read-file-position streamlet) 0)
    (when (output-direction-p direction)
      (write-file-journal-state stream :new))
    streamlet))

(defmethod close-streamlet ((streamlet file-streamlet))
  (close (%stream streamlet))
  (setf (%stream streamlet) nil))

(defmethod make-streamlet-finalizer ((streamlet file-streamlet))
  (let ((outputp (output-streamlet-p streamlet))
        (stream (%stream streamlet)))
    (lambda ()
      (when outputp
        (finish-output stream))
      (close stream))))

(defun position-to-write (streamlet)
  ;; If the last operation was a read, we need to the file positition.
  (unless (wrote-last-p streamlet)
    ;; Save the read position. If we never switch between reading
    ;; and writing, there is no overhead.
    (let ((stream (%stream streamlet)))
      (setf (read-file-position streamlet) (file-position stream)
            (wrote-last-p streamlet) t)
      (file-position stream (file-length stream)))))

(defun position-to-read (streamlet)
  ;; If the last operation was a write, we need to the file positition.
  (when (wrote-last-p streamlet)
    (let ((stream (%stream streamlet)))
      (setf (wrote-last-p streamlet) nil)
      (file-position stream (read-file-position streamlet)))))

(defmethod read-position ((streamlet file-streamlet))
  (if (wrote-last-p streamlet)
      (read-file-position streamlet)
      (file-position (%stream streamlet))))

(defmethod set-read-position ((streamlet file-streamlet) read-position)
  (setf (slot-value streamlet 'peeked) nil)
  (if (wrote-last-p streamlet)
      (setf (read-file-position streamlet) read-position)
      (file-position (%stream streamlet) read-position)))

(defmethod peek-event ((streamlet file-streamlet))
  (with-slots (peeked) streamlet
    (if peeked
        (car peeked)
        (let ((entry (save-excursion (streamlet)
                       (cons (%read-event-from-streamlet streamlet nil)
                             (file-position (%stream streamlet))))))
          (setf peeked entry)
          (car entry)))))

(defmethod read-event ((streamlet file-streamlet) &optional eoj-error-p)
  (let ((peeked (slot-value streamlet 'peeked)))
    (cond (peeked
           (set-read-position streamlet (cdr peeked))
           (car peeked))
          (t
           (%read-event-from-streamlet streamlet eoj-error-p)))))

(defun %read-event-from-streamlet (streamlet eoj-error-p)
  (position-to-read streamlet)
  (let* ((eof (gensym))
         (event (%read-event-from-stream (%stream streamlet) eof)))
    (if (eq event eof)
        (if eoj-error-p
            (error 'end-of-journal :journal (journal streamlet))
            nil)
        event)))

(defmethod write-event (event (streamlet file-streamlet))
  (position-to-write streamlet)
  (let ((stream (%stream streamlet)))
    (when (out-event-p event)
      (decf (%depth streamlet)))
    (ensure-txn streamlet stream)
    (with-standard-io-syntax
      (loop repeat (* 2 (%depth streamlet))
            do (write-char #\Space stream))
      (prin1 event stream)
      (terpri stream))
    ;; Flush stream buffers. This is not that bad for performance, and
    ;; makes it less likely to lose events if running without SYNC.
    ;; Also, if running with SYNC, it's necessary to do before calling
    ;; fsync() in SYNC-STREAMLET.
    (finish-output stream)
    (when (in-event-p event)
      (incf (%depth streamlet)))))

(defmethod write-position ((streamlet file-streamlet))
  (let ((stream (%stream streamlet)))
    (finish-output stream)
    (file-length stream)))


(defsection @synchronization-with-file-journals
    (:title "Synchronization with file journals")
  "For FILE-JOURNALs, SYNC determines when the events written to the
  RECORD-JOURNAL and its JOURNAL-STATE will be persisted durably in
  the file. Syncing to the file involves two calls to `fsync()`, and
  is not cheap.

  Syncing events to files is implemented as follows.

  - When the journal file is created, its parent directory is
    immediately fsynced to make sure that the file will not be lost on
    ABORTED-EXECUTION.

  - When an event is about to be written the first time after file
    creation or after a sync, a transaction start marker is written to
    the file.

  - Any number of events may be subsequently written until syncing is
    deemed necessary (see @SYNCHRONIZATION-STRATEGIES).

  - At this point, `fsync()` is called to flush all event data and
    state changes to the file, and the transaction start marker is
    _overwritten_ with a transaction completed marker and another
    `fsync()` is performed.

  - When reading back this file (e.g. for replay), an open transaction
    marker is treated as the end of file.

  Note that this implementation assumes that after writing the start
  transaction marker a crash cannot leave any kind of garbage bytes
  around: it must leave zeros. This is not true for all filesytems.
  For example, ext3/ext4 with `data=writeback` [can leave garbage
  around][ext4-writeback].

  [ext4-writeback]: https://ext4.wiki.kernel.org/index.php/Ext3_Data=Ordered_vs_Data=Writeback_mode")

(defun read-file-journal-state (pathname)
  (with-open-file (stream pathname)
    (if (eql (read-char stream nil nil) #\Newline)
        :completed
        :failed)))

(defun write-file-journal-state (stream state)
  (assert (eq state :new))
  (write-char #\Space stream)
  (finish-output stream))

(defun %read-event-from-stream (stream eof)
  (let ((char (peek-char t stream nil eof)))
    (cond ((eq char eof) eof)
          ;; An unfinished transaction
          ((char= char #\Del)
           eof)
          ;; A finished transaction
          ((char= char #\Ack)
           (read-char stream)
           (with-standard-io-syntax (read stream nil eof)))
          (t
           (with-standard-io-syntax (read stream nil eof))))))

(defun ensure-txn (streamlet stream)
  (when (and (slot-value streamlet 'sync)
             (not (slot-value streamlet 'txn-start-file-position)))
    (setf (slot-value streamlet 'txn-start-file-position)
          (file-position stream))
    (write-char #\Del stream)))

(defmethod request-completed-on-abort ((streamlet file-streamlet))
  ;; We defer this until the end of the current transaction, if any.
  (cond ((slot-value streamlet 'txn-start-file-position)
         (setf (slot-value streamlet 'set-complete-on-abort-on-commit-p) t)
         nil)
        (t
         (set-file-journal-completed-on-abort streamlet)
         t)))

(defun set-file-journal-completed-on-abort (streamlet)
  (let* ((stream (%stream streamlet))
         (saved-position (file-position stream)))
    (%set-file-journal-completed-on-abort streamlet)
    (file-position stream saved-position)))

;;; Changes FILE-POSITION.
(defun %set-file-journal-completed-on-abort (streamlet)
  (let* ((journal (journal streamlet))
         (sync (slot-value journal 'sync))
         (stream (%stream streamlet)))
    (when sync
      (fsync-directory (directory-namestring (pathname-of journal))))
    (file-position stream 0)
    (write-char #\Newline stream)
    (finish-output stream)
    (when sync
      (fsync (stream-fd stream)))))

;;; Currently, we do at call fsync() at least 4 times during
;;; recording: to flush the events, to write txn committed marker, to
;;; flush the completed state, and to fsync the directory. The first
;;; two is actually per-transaction, so that 2+2*N, where N is max(1,
;;; n-new-data-events).
(defmethod sync-streamlet ((streamlet file-streamlet))
  (let* ((stream (%stream streamlet))
         (saved-position (file-position stream)))
    (%commit-file-journal-events streamlet)
    (when (slot-value streamlet 'set-complete-on-abort-on-commit-p)
      (%set-file-journal-completed-on-abort streamlet)
      (setf (slot-value streamlet 'set-complete-on-abort-on-commit-p) nil))
    (file-position stream saved-position)))

;;; Changes FILE-POSITION.
(defun %commit-file-journal-events (streamlet)
  (let ((txn-start (slot-value streamlet 'txn-start-file-position))
        (stream (%stream streamlet)))
    (when txn-start
      ;; Persist the events.
      (finish-output stream)
      (fsync (stream-fd stream))
      ;; Mark the transaction as successful.
      (file-position stream txn-start)
      (write-char #\Ack stream)
      (finish-output stream)
      (fsync (stream-fd stream))
      (setf (slot-value streamlet 'txn-start-file-position) nil)
      t)))

(defun fsync (fd)
  #-darwin
  (osicat-posix:fsync fd)
  #+darwin
  (let ((f-fullsync 51))
    (osicat-posix:fcntl fd f-fullsync)))

(defun stream-fd (stream)
  #+allegro
  (excl.osi::stream-to-fd stream)
  #+ccl
  (cll::stream-device stream :output)
  #+clisp
  (nth-value 1 (ext:stream-handles stream))
  #+cmucl
  (sys:fd-stream-fd stream)
  #+ecl
  (ext:file-stream-fd stream)
  #+sbcl
  (sb-sys:fd-stream-fd stream)
  #-(or allegro ccl clisp cmucl ecl sbcl)
  (error "Don't know how to get the unix fd from a STREAM."))

(defun fsync-directory (pathname)
  (with-open-file (s pathname)
    (fsync (stream-fd s))))


(defsection @safety (:title "Safety")
  "##### Thread safety

  Changes to journals come in two varieties: adding an event and
  changing the JOURNAL-STATE. Both are performed by JOURNALED only
  unless the low-level streamlet interface is used (see
  @STREAMLETS-REFERENCE). Using JOURNALED wrapped in a
  WITH-JOURNALING, WITH-BUNDLE, or @LOG-RECORD without WITH-JOURNALING
  is thread-safe.

  - Every journal is guaranteed to have at most a single writer active
    at any time. Writers are WITH-JOURNALING and WITH-BUNDLE, but also
    any journals used as LOG-RECORD (unless that journal is the
    RECORD-JOURNAL) have a log writer stored in the journal object.

  - WITH-JOURNALING and WITH-BUNDLE have dynamic extent as writers,
    but log writers of journals have indefinite extent: once a journal
    is used as a LOG-RECORD there remains a writer.

  - Attempting to create a second writer triggers a JOURNAL-ERROR.

  - Writing to the same journal via LOG-RECORD from multiple threads
    concurrently is possible since this doesn't create multiple
    writers. It is ensured with locking that events are written
    atomically. Frames can be interleaved, but these are LOG-EVENTs,
    so this does not affect replay.

  - The juggling of replay and record journals performed by
    WITH-BUNDLE is also thread-safe.

  - It is ensured that there is at most one FILE-JOURNAL object in the
    same Lisp image is backed by the same file.

  - Similarly, there is at most FILE-BUNDLE object for a directory.

  ##### Process safety

  Currently, there is no protection against multiple OS processes
  writing the same FILE-JOURNAL or FILE-BUNDLE.

  ##### Signal safety

  Journal is _designed_ to be @ASYNC-UNWIND safe, but _not reentrant_.
  Interrupts are disabled only for the most critical cleanup forms. If
  a thread is killed without unwinding, that constitutes
  ABORTED-EXECUTION, so guarantees about @SYNCHRONIZATION apply, but
  JOURNAL objects written by the thread are not safe to access, and
  the Lisp should probably be restarted.")


(defsection @bundles-reference (:title "Bundles reference")
  """In @BUNDLES, we covered the repeated replay problem that
  WITH-BUNDLE automates. Here we provide a reference for the bundle
  classes."""
  (bundle class)
  (max-n-failed (accessor bundle))
  (max-n-completed (accessor bundle))
  (@in-memory-bundles section)
  (@file-bundles section))

(defclass bundle ()
  ((journals
    :initform () :initarg :journals :reader journals :accessor %journals)
   (max-n-failed
    :initform 1 :initarg :max-n-failed :accessor max-n-failed
    :documentation "If MAX-N-FAILED is non-NIL, and the number of
    journals of [JOURNAL-STATE][type] :FAILED in the bundle exceeds
    its value, then some journals (starting with the oldest) are
    deleted.")
   (max-n-completed
    :initform 1 :initarg :max-n-completed :accessor max-n-completed
    :documentation "If MAX-N-COMPLETED is non-NIL, and the number of
    journals of [JOURNAL-STATE][type] :COMPLETED in the bundle exceeds
    its value, then some journals (starting with the oldest) are
    deleted.")
   (lock :initform (bt:make-lock))
   (n-writers :initform 0))
  (:documentation "This is an abstract base class. Direct subclasses
  are IN-MEMORY-BUNDLE and FILE-BUNDLE.

  A BUNDLE consists of a sequence of journals which are all reruns of
  the same code, hopefully making more and more progress towards
  completion. These journals are @REPLAYs of the previous successful
  one, extending it with new events. Upon replay (see WITH-BUNDLE),
  the latest journal in the bundle in JOURNAL-STATE :COMPLETED plays
  the role of the replay journal, and a new journal is added to the
  bundle for recording. If the replay succeeds, this new journal
  eventually becomes :COMPLETED and takes over the role of the replay
  journal for future replays until another replay succeeds. When the
  bundle is created and it has no journals yet, the replay journal is
  an empty, completed one."))

(defmethod print-object ((bundle bundle) stream)
  (print-unreadable-object (bundle stream :type t)
    (format stream "~S" (ignore-errors (length (journals bundle))))))

(defmacro with-bundle-locked ((bundle) &body body)
  `(bt:with-recursive-lock-held ((slot-value ,bundle 'lock))
     ,@body))

(defmethod to-journal ((bundle bundle))
  (bundle-replay-journal bundle))

(defmethod open-streamlet ((bundle bundle) &key direction)
  (if (eq direction :input)
      (open-streamlet (bundle-replay-journal bundle) :direction :input)
      (error 'journal-error :journal nil
             :format-control "~@<Cannot open bundle for direction ~S.~:@>"
             :format-args (list direction))))

(defun bundle-replay-journal (bundle)
  (find :completed (journals bundle) :key #'journal-state))

(defun bundle-record-journal (bundle)
  ;; Always called holding BUNDLE's LOCK.
  (let ((journal (make-record-journal-in-bundle bundle)))
    (push journal (%journals bundle))
    (reap-journals-in-bundle bundle)
    journal))

;;; This would belong to the @JOURNAL-BUNDLE-EXTENSION section if
;;; there was one.
(defgeneric make-record-journal-in-bundle (bundle))

(defun reap-journals-in-bundle (bundle)
  (let ((max-n-failed (max-n-failed bundle))
        (max-n-completed (max-n-completed bundle))
        (n-failed 0)
        (n-completed 0))
    (assert (or (null max-n-completed) (<= 1 max-n-completed)))
    ;; Iteration starts with the most recent.
    (dolist (journal (journals bundle))
      (let ((failedp (eq (journal-state journal) :failed))
            (completedp (eq (journal-state journal) :completed)))
        (when failedp
          (incf n-failed))
        (when completedp
          (incf n-completed))
        (when (or (and failedp max-n-failed
                       (< max-n-failed n-failed))
                  (and completedp max-n-completed
                       (< max-n-completed n-completed)))
          (delete-journal-from-bundle bundle journal))))))

(defun reap-identical-or-non-divergent-journals (bundle)
  (let* ((latest (first (%journals bundle)))
         (state (journal-state latest)))
    (ecase state
      ((:completed)
       (when (not (journal-divergent-p latest))
         (delete-journal-from-bundle bundle latest)))
      ((:failed)
       ;; Currently, we compare the most recent failed journal in
       ;; BUNDLE to the previous failed journal.
       (let ((previous (find state (rest (%journals bundle))
                             :key #'journal-state)))
         (when (and previous (identical-journals-p latest previous))
           (delete-journal-from-bundle bundle latest)))))))

;;; This would belong to the @JOURNAL-BUNDLE-EXTENSION section if
;;; there was one.
(defgeneric delete-journal-from-bundle (bundle journal)
  (:method :around (bundle journal)
    (call-next-method)
    (setf (%journals bundle) (remove journal (%journals bundle)))
    (values)))


(defsection @in-memory-bundles (:title "In-memory bundles")
  (in-memory-bundle class)
  (make-in-memory-bundle function))

(defclass in-memory-bundle (bundle)
  ((sync :initarg :sync)
   (sync-fn :initarg :sync-fn))
  (:documentation "An IN-MEMORY-BUNDLE is a BUNDLE that is built on
  IN-MEMORY-JOURNALs. IN-MEMORY-BUNDLEs have limited utility as a
  persistence mechanism and are provided mainly for reasons of
  symmetry and for testing. See
  @SYNCHRONIZATION-WITH-IN-MEMORY-JOURNALS for an example of how to
  achieve persistence without bundles."))

(defun make-in-memory-bundle (&key (max-n-failed 1) (max-n-completed 1)
                              sync sync-fn)
  "Create a new IN-MEMORY-BUNDLE with [MAX-N-FAILED][(accessor
  bundle)] and [MAX-N-COMPLETED][(accessor bundle)]. SYNC and SYNC-FN
  are passed on to MAKE-IN-MEMORY-JOURNAL."
  (check-sync-value sync)
  (make-instance 'in-memory-bundle
                 :max-n-failed max-n-failed
                 :max-n-completed max-n-completed
                 :sync sync
                 :sync-fn sync-fn))

(defmethod make-record-journal-in-bundle ((bundle in-memory-bundle))
  (with-slots (sync sync-fn) bundle
    (make-in-memory-journal :sync sync :sync-fn sync-fn)))

(defmethod delete-journal-from-bundle ((bundle in-memory-bundle) journal))


(defsection @file-bundles (:title "File bundles")
  (file-bundle class)
  (directory-of (reader file-bundle))
  (make-file-bundle function)
  (delete-file-bundle function))

(defclass file-bundle (bundle)
  ((directory
    :initarg :directory :reader directory-of
    :documentation "The directory where the files backing the
    FILE-JOURNALs in the FILE-BUNDLE are kept.")
   (sync :initarg :sync))
  (:documentation "A FILE-BUNDLE is a BUNDLE that is built on
  FILE-JOURNALs. It provides easy replay-based persistence."))

(defmethod print-object ((bundle file-bundle) stream)
  (print-unreadable-object (bundle stream :type t)
    (format stream "~S ~S" (ignore-errors (namestring (directory-of bundle)))
            (ignore-errors (length (journals bundle))))))

(defvar *truename-to-file-bundle*
  (trivial-garbage:make-weak-hash-table :weakness :value :test #'equal))

(defvar *file-bundle-lock* (bt:make-lock "file-bundle-lock"))

(defun make-file-bundle (directory &key (max-n-failed 1) (max-n-completed 1)
                         sync)
  "Return a FILE-BUNDLE object backed by FILE-JOURNALs in DIRECTORY.
  See [MAX-N-FAILED][(accessor file-bundle)] and
  [MAX-N-COMPLETED][(accessor file-bundle)]. For a description of
  SYNC, see @SYNCHRONIZATION-STRATEGIES.

  If there is already a FILE-BUNDLE with the same directory (according
  to TRUENAME), return that object is returned if it has the same
  MAX-N-FAILED, MAX-N-COMPLETED and SYNC options, else JOURNAL-ERROR
  is signalled."
  (check-sync-value sync)
  (let ((directory (cl-fad:pathname-as-directory directory)))
    ;; Ensure it exists first so that TRUENAME doesn't fail.
    (ensure-directories-exist directory)
    (bt:with-lock-held (*file-bundle-lock*)
      ;; Make DIRECTORY independent of *DEFAULT-PATHNAME-DEFAULTS* and such.
      (let* ((directory (truename directory))
             (bundle (or (check-file-bundle-options
                          (gethash directory *truename-to-file-bundle*)
                          max-n-failed max-n-completed sync)
                         (setf (gethash directory *truename-to-file-bundle*)
                               (make-instance
                                'file-bundle
                                :directory directory
                                :max-n-failed max-n-failed
                                :max-n-completed max-n-completed
                                :sync sync)))))
        (setf (slot-value bundle 'journals)
              (make-file-journals-for-bundle directory sync))
        bundle))))

(defun check-file-bundle-options (bundle max-n-failed max-n-completed sync)
  (when bundle
    (let ((incompatibilities ()))
      (unless (eql (slot-value bundle 'max-n-failed) max-n-failed)
        (setq incompatibilities (append incompatibilities
                                        `(:max-n-failed ,max-n-failed))))
      (unless (eql (slot-value bundle 'max-n-completed) max-n-completed)
        (setq incompatibilities (append incompatibilities
                                        `(:max-n-completed ,max-n-completed))))
      (unless (eq (slot-value bundle 'sync) sync)
        (setq incompatibilities (append incompatibilities `(:sync ,sync))))
      (when incompatibilities
        (error 'journal-error :journal bundle
               :format-control "~@<Incompatible options given for ~S: ~
                               ~S.~:@>"
               :format-args (list bundle incompatibilities))))
    bundle))

(defun make-file-journals-for-bundle (directory sync)
  (mapcar (lambda (pathname)
            (make-file-journal pathname :sync sync))
          (list-journal-files-in-directory directory)))

(defun list-journal-files-in-directory (directory)
  (let* ((pattern (make-pathname :name :wild :type "jrn" :defaults directory))
         (pathname-and-version-list
           (loop for pathname in (directory pattern)
                 for version = (bundle-file-version pathname)
                 when version
                   collect (cons pathname version))))
    (mapcar #'car (sort pathname-and-version-list #'> :key #'cdr))))

(defun delete-file-bundle (directory)
  "Delete all journal files (`*.jrn`) from DIRECTORY. Delete the
  directory if empty after the journal files were deleted, else signal
  an error. Existing FILE-BUNDLE objects are not updated, so
  MAKE-FILE-JOURNAL with FORCE-RELOAD may be required."
  (let ((directory (cl-fad:pathname-as-directory directory))
        (pattern (make-pathname :name :wild :type "jrn" :defaults directory)))
    (dolist (pathname (directory pattern))
      (delete-file pathname))
    (uiop:delete-empty-directory directory)))

(defun bundle-file-version (pathname)
  (ignore-errors (parse-integer (pathname-name pathname))))

(defmethod make-record-journal-in-bundle ((bundle file-bundle))
  (let* ((next-id (if (endp (journals bundle))
                      0
                      (1+ (bundle-file-version
                           (pathname-of (first (journals bundle)))))))
         ;; We always output at least 8 digits, padding with leading
         ;; zeros if necessary, but we don't rely on them when
         ;; parsing.
         (pathname (make-pathname :name (format nil "~8,'0D" next-id)
                                  :type "jrn" :defaults (directory-of bundle))))
    (make-file-journal pathname :sync (slot-value bundle 'sync))))

(defmethod delete-journal-from-bundle ((bundle file-bundle) journal)
  (delete-file-journal journal))


(defsection @glossary (:title "Glossary" :export nil)
  (@async-unwind glossary-term)
  (@boolean-valued-symbol glossary-term)
  (@non-local-exit glossary-term)
  (@readable glossary-term))

(define-glossary-term @async-unwind (:title "async-unwind")
  "If an asynchronous event, say a `SIGINT` triggered by `C-c`, is
  delivered to a thread running Lisp or foreign code called from Lisp,
  a Lisp condition is typically signalled. If the handler for this
  condition unwinds the stack, then we have an asynchronous unwind.
  Another example is BT:INTERRUPT-THREAD which, as it can execute
  arbitrary code, may unwind the stack in the target thread.")

(define-glossary-term @boolean-valued-symbol (:title "boolean-valued symbol")
  "Imagine writing two STREAMs with a spaghetti of functions and
  wanting to have pretty-printed output on one of them. Unfortunately,
  binding *PRINT-PRETTY* to T will affect writes to both streams.

  A solution is to have streams look up their own print-pretty flag
  with `(SYMBOL-VALUE (STREAM-PRETTY-PRINT STREAM))` and have the
  caller specify the dynamic variable they want:

  ```
  (defvar *print-pretty-1* nil)
  (setf (stream-print-pretty stream-1) '*print-pretty-1*)
  (let ((*print-pretty-1* t))
    (spaghetti stream-1 stream-2))
  ```

  Note that if the default `STREAM-PRINT-PRETTY` is `'*PRINT-PRETTY*`,
  then we have the normal Common Lisp behaviour. Setting
  `STREAM-PRINT-PRETTY` to NIL or T also works, because they are
  self-evaluating.

  If not by CL:STREAMs, boolean-valued symbols are used by
  MAKE-LOG-DECORATOR and PPRINT-JOURNALs.")

(define-glossary-term @non-local-exit (:title "non-local exit")
  "This is a term from the Common Lisp ANSI standard. If a form does
  not return normally, but control is transferred via `GO`, RETURN,
  RETURN-FROM or THROW, then it is said to have performed a non-local
  exit. This definition of a non-local exit includes EVENT-EXIT
  :CONDITION, :ERROR and :NLX.")

(define-glossary-term @readable (:title "readable")
  "In Common Lisp, readable objects are those that can be printed readably.
  Anything written to stream-based journals needs to be readable.")
