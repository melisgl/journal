<a id='x-28JOURNAL-3A-40JOURNAL-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# Journal manual

## Table of Contents

- [1 JOURNAL ASDF System Details][9c42]
- [2 Links][1726]
- [3 Portability][2ee0]
- [4 Background][31b6]
- [5 Distinguishing features][4182]
- [6 Basics][7c19]
    - [6.1 In-events][1d28]
    - [6.2 Out-events][5721]
    - [6.3 Working with unreadable values][51dd]
    - [6.4 Utilities][0919]
    - [6.5 Pretty-printing][9607]
    - [6.6 Error handling][fb13]
- [7 Logging][77df]
    - [7.1 Customizing logs][bb0f]
    - [7.2 Log record][3380]
    - [7.3 Logging with leaf-events][6be9]
- [8 Tracing][7849]
    - [8.1 Slime integration][d6c0]
- [9 Replay][0dc7]
    - [9.1 Journaled for replay][0bc8]
    - [9.2 Bundles][5b0f]
    - [9.3 The replay strategy][3c00]
    - [9.4 Matching in-events][e04a]
        - [9.4.1 Replaying the outcome][b6d1]
    - [9.5 Matching out-events][21d1]
    - [9.6 Replay failures][588a]
    - [9.7 Upgrades and replay][1acb]
- [10 Testing][c28b]
    - [10.1 Testing on multiple levels][d95f]
- [11 Persistence][98d3]
    - [11.1 Persistence tutorial][f6f7]
    - [11.2 Synchronization to storage][a074]
        - [11.2.1 Synchronization strategies][355b]
        - [11.2.2 Synchronization with in-memory journals][86a2]
        - [11.2.3 Synchronization with file journals][eb29]
- [12 Safety][7224]
- [13 Events reference][d9ae]
    - [13.1 Event versions][9382]
    - [13.2 In-events][040a]
    - [13.3 Out-events][9dd2]
    - [13.4 Leaf-events][ae7d]
- [14 Journals reference][3991]
    - [14.1 Comparing journals][75ff]
    - [14.2 In-memory journals][68a8]
    - [14.3 File journals][7bd1]
    - [14.4 Pretty-printing journals][eeda]
- [15 Bundles reference][a1fe]
    - [15.1 In-memory bundles][afda]
    - [15.2 File bundles][4e3d]
- [16 Streamlets reference][2453]
    - [16.1 Opening and closing][ade5]
    - [16.2 Reading from streamlets][e099]
    - [16.3 Writing to streamlets][afa1]
- [17 Glossary][1311]

###### \[in package JOURNAL with nicknames JRN\]
<a id='x-28-23A-28-287-29-20BASE-CHAR-20-2E-20-22journal-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

## 1 JOURNAL ASDF System Details

- Version: 0.1.0
- Description: A library for logging, tracing, testing and persistence.
- Licence: MIT, see COPYING.
- Author: Gábor Melis <mega@retes.hu>
- Homepage: [http://github.com/melisgl/journal](http://github.com/melisgl/journal)
- Bug tracker: [http://github.com/melisgl/journal/issues](http://github.com/melisgl/journal/issues)
- Source control: [GIT](https://github.com/melisgl/journal.git)

<a id='x-28JOURNAL-3A-40JOURNAL-LINKS-20MGL-PAX-3ASECTION-29'></a>

## 2 Links

Here is the [official repository](https://github.com/melisgl/journal)
and the [HTML
documentation](http://melisgl.github.io/mgl-pax-world/journal-manual.html)
for the latest version.

<a id='x-28JOURNAL-3A-40JOURNAL-PORTABILITY-20MGL-PAX-3ASECTION-29'></a>

## 3 Portability

Tested on CCL, CLISP, CMUCL, ECL, and SBCL. AllegroCL Express
edition runs out of heap while running the tests. Lispworks is not
tested. On Lisps that seem to lack support for disabling and
enabling of interrupts, such as ABCL and CLISP, durability is
compromised, and any attempt to [`SYNC-JOURNAL`][6bc6] (see
[Synchronization strategies][355b] and [Safety][7224]) will be a runtime error.

<a id='x-28JOURNAL-3A-40JOURNAL-BACKGROUND-20MGL-PAX-3ASECTION-29'></a>

## 4 Background

Logging, tracing, testing, and persistence are about what happened
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
single macro at its heart: [`JOURNALED`][4f52], which does pretty much what
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

The Journal library is this idea taken to its logical conclusion.

<a id='x-28JOURNAL-3A-40JOURNAL-FEATURES-20MGL-PAX-3ASECTION-29'></a>

## 5 Distinguishing features

##### As a logging facility

- Nested contexts and single messages

- Customizable content and format

- Human- or machine-readable output

```
#68200.234: ("some-context")
#68200.234:   Informative log message
#68200.250: => NIL
```

See [Logging][77df] for a complete example.

##### Compared to `CL:TRACE`

- Ability to handle [non-local exit][e17e]s

- Customizable content and format

- Optional timestamps, internal real- and run-time

```
(FOO 2.1)
  (1+ 2.1)
  => 3.1
=E "SIMPLE-ERROR" "The assertion (INTEGERP 3.1) failed."
```

See [Tracing][7849] for a complete example.

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

See [Testing][c28b] for a complete example.

##### As a solution for persistence

- Event Sourcing: replay interactions with the external world

- Unchanged control flow

- Easy to implement history, undo

```
(defun my-resumable-autosaving-game-with-history ()
  (with-bundle (bundle)
    (play-guess-my-number)))
```

See [Persistence][98d3] for a complete example.

<a id='x-28JOURNAL-3A-40JOURNAL-BASICS-20MGL-PAX-3ASECTION-29'></a>

## 6 Basics

The [`JOURNALED`][4f52] macro does both recording and replaying of events,
possibly at the same time. Recording is easy: events generated by
`JOURNALED` are simply written to a journal, which is a sequence of
events much like a file. What events are generated is described in
`JOURNALED`. [Replay][0dc7] is much more involved, thus it gets its own
section. The journals used for recording and replaying are specified
by [`WITH-JOURNALING`][234e] or by [`WITH-BUNDLE`][0ddc].

The [Journals reference][3991] is presented later, but for most purposes
creating them (e.g. with [`MAKE-IN-MEMORY-JOURNAL`][0605], [`MAKE-FILE-JOURNAL`][182e])
and maybe querying their contents with [`LIST-EVENTS`][3c76] will suffice.
Some common cases of journal creation are handled by the convenience
function [`TO-JOURNAL`][ce19].

Built on top of journals, [Bundles][5b0f] juggle repeated replay-and-record
cycles focussing on persistence.

<a id='x-28JOURNAL-3ATO-JOURNAL-20GENERIC-FUNCTION-29'></a>

- [generic-function] **TO-JOURNAL** *DESIGNATOR*

    Return the journal designated by `DESIGNATOR` or
    signal an error. The default implementation:
    
    - returns `DESIGNATOR` itself if it is of type [`JOURNAL`][86bc],
    
    - returns a new [`IN-MEMORY-JOURNAL`][17a8] if `DESIGNATOR` is `T`,
    
    - returns a new [`FILE-JOURNAL`][f6b2] if `DESIGNATOR` is a `PATHNAME`.


<a id='x-28JOURNAL-3AWITH-JOURNALING-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-JOURNALING** *(&KEY RECORD REPLAY REPLAY-EOJ-ERROR-P) &BODY BODY*

    Turn recording and/or replaying of events on or off for the
    duration of `BODY`. Both `RECORD` and `REPLAY` should be a [`JOURNAL`][86bc]
    designator (in the sense of [`TO-JOURNAL`][ce19]) or `NIL`.
    
    If `RECORD` designates a `JOURNAL`, then events generated by enclosed
    [`JOURNALED`][4f52] [block][6572]s are written to that journal (with exceptions, see
    the `LOG-RECORD` argument of `JOURNALED`). If `REPLAY` designates a
    `JOURNAL`, then the generated events are matched against events from
    that journal according to the rules of [Replay][0dc7].
    
    A [`JOURNAL-ERROR`][571f] is signalled, if `RECORD` is a `JOURNAL` that has been
    previously recorded to by another `WITH-JOURNALING` (that is, if its
    [`JOURNAL-STATE`][3631] is not `:NEW`), or if `REPLAY` is a `JOURNAL` that is not a
    complete recording of successful replay (i.e. its `JOURNAL-STATE` is
    not `:COMPLETED`). These checks are intended to catch mistakes that
    would render the new or existing records unusable for replay. When
    `WITH-JOURNALING` finishes, the `RECORD` journal is marked `:COMPLETED` or
    `:FAILED` in its `JOURNAL-STATE`.
    
    `REPLAY-EOJ-ERROR-P` controls whether an [`END-OF-JOURNAL`][9c7e] is signalled
    when a new event is being matched to the replay journal from which
    there are no more events to read. If there was a [`JOURNALING-FAILURE`][6db0]
    or a [`REPLAY-FAILURE`][955f] during execution, then `END-OF-JOURNAL` is not
    signalled.
    
    If `BODY` completes successfully, but `REPLAY` has unprocessed events,
    then signal [`REPLAY-INCOMPLETE`][32a1].
    
    `WITH-JOURNALING` for different `RECORD` journals can be nested and run
    independently.

<a id='x-28JOURNAL-3A-40BLOCK-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **block**

    A journaled block, or simply block, is a number of forms wrapped in
    [`JOURNALED`][4f52]. When a block is executed, a [frame][1452] is created.

<a id='x-28JOURNAL-3A-40FRAME-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **frame**

    A frame is an [`IN-EVENT`][e10e], [`OUT-EVENT`][cba8] pair, which are created when a
    [block][6572] is entered and left, respectively.

<a id='x-28JOURNAL-3ARECORD-JOURNAL-20FUNCTION-29'></a>

- [function] **RECORD-JOURNAL** 

    Return the journal in which events are currently being
    recorded (see [`WITH-JOURNALING`][234e] and [`WITH-BUNDLE`][0ddc]) or `NIL`.

<a id='x-28JOURNAL-3AREPLAY-JOURNAL-20FUNCTION-29'></a>

- [function] **REPLAY-JOURNAL** 

    Return the journal from which events are currently being
    replayed (see [`WITH-JOURNALING`][234e] and [`WITH-BUNDLE`][0ddc]) or `NIL`.

<a id='x-28JOURNAL-3AJOURNALED-20MGL-PAX-3AMACRO-29'></a>

- [macro] **JOURNALED** *(NAME &KEY (LOG-RECORD :RECORD) VERSION ARGS VALUES CONDITION INSERTABLE REPLAY-VALUES REPLAY-CONDITION) &BODY BODY*

    `JOURNALED` generates events upon entering and leaving the dynamic
    extent of `BODY` (also known as the journaled [block][6572]), which we call
    the [In-events][1d28] and [Out-events][5721]. Between generating the two events,
    `BODY` is typically executed normally (except for
    [Replaying the outcome][b6d1]).
    
    Where the generated events are written is determined by the `:RECORD`
    argument of the enclosing [`WITH-JOURNALING`][234e]. If there is no enclosing
    `WITH-JOURNALING` and `LOG-RECORD` is `NIL`, then event recording is
    turned off and `JOURNALED` imposes minimal overhead.
    
    - `NAME` can be of any type except `NULL`, not evaluated. For names, and
      for anything that gets written to a journal, a non-keyword symbol
      is a reasonable choice as it can be easily made unique. However,
      it also exposes the package structure, which might make reading
      stuff back more difficult. Keywords and strings do not have this
      problem.
    
    - `ARGS` can be of any type, but is typically a list.
    
    Also see in [Log record][3380] in the [Logging][77df] section. For a description
    of `VERSION`, `INSERTABLE`, `REPLAY-VALUES` and `REPLAY-CONDITION`, see
    [Journaled for replay][0bc8].

<a id='x-28JOURNAL-3A-40IN-EVENTS-20MGL-PAX-3ASECTION-29'></a>

### 6.1 In-events

Upon entering a [block][6572], [`JOURNALED`][4f52] generates an [`IN-EVENT`][e10e],
which conceptually opens a new [frame][1452]. These in-events are created
from the `NAME`, `VERSION` and `ARGS` arguments of `JOURNALED`. For example,

```
(journaled (name :version version :args args) ...)
```

creates an event like this:

```
`(:in ,name :version ,version :args ,args)
```

where `:VERSION` and `:ARGS` may be omitted if they are `NIL`. Versions
are used for [Replay][0dc7].

<a id='x-28JOURNAL-3A-40OUT-EVENTS-20MGL-PAX-3ASECTION-29'></a>

### 6.2 Out-events

Upon leaving a [block][6572], [`JOURNALED`][4f52] generates and [`OUT-EVENT`][cba8], closing
the [frame][1452] opened by the corresponding in-event. These out-events
are property lists like this:

```
(:out foo :version 1 :values (42))
```

Their `NAME` and `VERSION` (`FOO` and `1` in the example) are the same
as in the in-event: they are the corresponding arguments of
`JOURNALED`. `EXIT` and `OUTCOME` are filled in differently depending on
how the block finished its execution.

<a id='x-28JOURNAL-3AEVENT-EXIT-20TYPE-29'></a>

- [type] **EVENT-EXIT**

    One of `:VALUES`, `:CONDITION`, `:ERROR` and `:NLX`. Indicates whether a
    journaled [block][6572]:
    
    - returned normally (`:VALUES`, see [values outcome][39f4]),
    
    - unwound on an expected condition (`:CONDITION`, see [condition outcome][f4eb]),
    
    - unwound on an unexpected condition (`:ERROR`, see [error outcome][248f]),
    
    - unwound by performing a [non-local exit][e17e] of some other kind such as
      a throw (`:NLX`, see [nlx outcome][61f5]).
    
    The first two are [expected outcome][32e0]s, while the latter two are
    [unexpected outcome][f57e]s.

<a id='x-28JOURNAL-3AVALUES-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **values outcome**

    If the [`JOURNALED`][4f52] [block][6572] returns normally, [`EVENT-EXIT`][306a] is
    `:VALUES` and the outcome is the list of values returned:
    
    ```
    (journaled (foo) (values 7 t))
    ;; generates the out-event
    (:out foo :values (7 t))
    ```
    
    The list of return values of the block is transformed by the `VALUES`
    argument of `JOURNALED`, whose default is `#'IDENTITY`. Also see
    [Working with unreadable values][51dd]).

<a id='x-28JOURNAL-3ACONDITION-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **condition outcome**

    If the [block][6572] unwound due to a condition, and [`JOURNALED`][4f52]'s
    `CONDITION` argument (a function whose default is `(CONSTANTLY NIL)`)
    returns non-NIL when invoked on it, then [`EVENT-EXIT`][306a] is
    `:CONDITION` and the outcome is this return value:
    
    ```
    (journaled (foo :condition (lambda (c) (prin1-to-string c)))
      (error "xxx"))
    ;; generates the out-event
    (:out foo :condition "xxx")
    ```
    
    Conditions thus recognized are those that can be considered to be
    part of normal execution. Just like return values, these expected
    conditions may be required to match what's in the replay journal.
    Furthermore, given a suitable `REPLAY-CONDITION` in `JOURNALED`, they
    may be replayed without running the `@BLOCK`.

<a id='x-28JOURNAL-3AERROR-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **error outcome**

    If the [`JOURNALED`][4f52] [block][6572] unwound due to a condition, but
    `JOURNALED`'s `CONDITION` argument returns `NIL` when invoked on it, then
    [`EVENT-EXIT`][306a] is `:ERROR` and the outcome the string
    representations of the type of the condition and the condition
    itself.
    
    ```
    (journaled (foo)
      (error "xxx"))
    ;; generates the out-event
    (:out foo :error ("simple-error" "xxx"))
    ```
    
    The conversion to string is performed with `PRINC` in
    `WITH-STANDARD-IO-SYNTAX`. This scheme is intended to avoid leaking
    random implementation details into the journal, which would make
    `READ`ing it back difficult.
    
    In contrast with [condition outcome][f4eb]s, error outcomes are what the
    code is not prepared to handle or replay in a meaningful way.

<a id='x-28JOURNAL-3ANLX-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **nlx outcome**

    If the [`JOURNALED`][4f52] [block][6572] performed a [non-local exit][e17e] that was not
    due to a condition, then [`EVENT-EXIT`][306a] is `:NLX` and the outcome
    is `NIL`.
    
    ```
    (catch 'xxx
      (journaled (foo)
        (throw 'xxx nil)))
    ;; generates the out-event
    (:out foo :nlx nil)
    ```
    
    Note that [condition outcome][f4eb]s and [error outcome][248f]s are also due to
    `@NON-LOCAL-EXIT`s, but are distinct from nlx outcomes.
    
    Currently, nlx outcomes are detected rather heuristically as there
    is no portable way to detect what really caused the unwinding of the
    stack.

There is a further grouping of outcomes into expected and unexpected.

<a id='x-28JOURNAL-3AEXPECTED-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **expected outcome**

    An [`OUT-EVENT`][cba8] is said to have an expected outcome if it had a
    [values outcome][39f4] or a [condition outcome][f4eb], or equivalently, when its
    [`EVENT-EXIT`][306a] is `:VALUES` or `:CONDITION`.

<a id='x-28JOURNAL-3AUNEXPECTED-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **unexpected outcome**

    An [`OUT-EVENT`][cba8] is said to have an unexpected outcome if it had an
    [error outcome][248f] or an [nlx outcome][61f5], or equivalently, when its
    [`EVENT-EXIT`][306a] is `:ERROR` or `:NLX`.

<a id='x-28JOURNAL-3A-40WORKING-WITH-UNREADABLE-VALUES-20MGL-PAX-3ASECTION-29'></a>

### 6.3 Working with unreadable values

The events recorded often need to be [readable][531d]. This is always
required with [`FILE-JOURNAL`][f6b2]s, often with [`IN-MEMORY-JOURNAL`][17a8]s, but
never with [`PPRINT-JOURNAL`][2123]s. By choosing an appropriate identifier or
string representation of the unreadable object to journal, this is
not a problem in practice. [`JOURNALED`][4f52] provides the `VALUES` hook for
this purpose.

With [`EXTERNAL-EVENT`][eed7]s, whose outcome is replayed (see
[Replaying the outcome][b6d1]), we also need to be able to reverse the
transformation of `VALUES`, and this is what the `REPLAY-VALUES`
argument of `JOURNALED` is for.

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
something `@READABLE`. On the `Recording` run, `(VALUES-> #'USER-ID)`
replaces the user object with its id in the [`EVENT-OUTCOME`][95b8] recorded,
but the original user object is returned.

When `Replaying`, the journaled [`OUT-EVENT`][cba8] is replayed (see
[Replaying the outcome][b6d1]):

```
(:OUT GET-MESSAGE :VERSION :INFINITY :VALUES (7 "hello"))
```

The user object is looked up according to `:REPLAY-VALUES` and is
returned along with `"hello"`.

<a id='x-28JOURNAL-3AVALUES--3E-20FUNCTION-29'></a>

- [function] **VALUES-\>** *&REST FNS*

    A utility to create a function suitable as the `VALUES` argument of
    [`JOURNALED`][4f52]. The `VALUES` function is called with the list of values
    returned by the [block][6572] and returns a transformed set of values that
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
    untransformed by specifying #'`IDENTITY` or `NIL` as the function:
    
    ```
    (funcall (values-> #'1+ nil #'symbol-name)
             '(7 :something :another))
    => (8 :SOMETHING "ANOTHER")
    ```


<a id='x-28JOURNAL-3AVALUES-3C--20FUNCTION-29'></a>

- [function] **VALUES\<-** *&REST FNS*

    The inverse of [`VALUES->`][a799], this returns a function suitable as
    the `REPLAY-VALUES` argument of [`JOURNALED`][4f52]. It does pretty much what
    `VALUES->` does, but the function returned returns the transformed
    list as multiple values instead of as a list.
    
    ```
    (funcall (values<- #'1-) '(8 :something))
    => 7
    => :SOMETHING
    ```


<a id='x-28JOURNAL-3A-40JOURNAL-UTILITIES-20MGL-PAX-3ASECTION-29'></a>

### 6.4 Utilities

<a id='x-28JOURNAL-3ALIST-EVENTS-20FUNCTION-29'></a>

- [function] **LIST-EVENTS** *&OPTIONAL (JOURNAL (RECORD-JOURNAL))*

    Return a list of all the events in the journal designated by
    `JOURNAL`. Calls [`SYNC-JOURNAL`][6bc6] first, to make sure that all writes are
    taken into account.

<a id='x-28JOURNAL-3AEVENTS-TO-FRAMES-20FUNCTION-29'></a>

- [function] **EVENTS-TO-FRAMES** *EVENTS*

    Convert a flat list of events, such as those returned by [`LIST-EVENTS`][3c76],
    to a nested list representing the [frame][1452]s. Each frame is a list of
    the form `(<in-event> <nested-frames>* <out-event>?)`. Like in
    [`PRINT-EVENTS`][3bd9], `EVENTS` may be a [`JOURNAL`][86bc].
    
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
    an [`OUT-EVENT`][cba8]) are included in the output.

<a id='x-28JOURNAL-3AEXPECTED-TYPE-20FUNCTION-29'></a>

- [function] **EXPECTED-TYPE** *TYPE*

    Return a function suitable as the `CONDITION` argument of [`JOURNALED`][4f52],
    which returns the type of its single argument as a string if it is
    of `TYPE`, else `NIL`.

<a id='x-28JOURNAL-3A-40PRETTY-PRINTING-20MGL-PAX-3ASECTION-29'></a>

### 6.5 Pretty-printing

<a id='x-28JOURNAL-3APRINT-EVENTS-20FUNCTION-29'></a>

- [function] **PRINT-EVENTS** *EVENTS &KEY STREAM*

    Print `EVENTS` to `STREAM` as lists, starting a new line for each
    event and indenting them according to their nesting structure.
    `EVENTS` may be a [`JOURNAL`][86bc], in which case [`LIST-EVENTS`][3c76] is called on it
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


<a id='x-28JOURNAL-3APPRINT-EVENTS-20FUNCTION-29'></a>

- [function] **PPRINT-EVENTS** *EVENTS &KEY STREAM (PRETTIFIER 'PRETTIFY-EVENT)*

    Like [`PRINT-EVENTS`][3bd9], but produces terser, more human readable
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
    
    The function given as the `PRETTIFIER` argument formats individual
    events. The above output was produced with [`PRETTIFY-EVENT`][29db]. For a
    description of `PRETTIFIER`'s arguments see `PRETTIFY-EVENT`.

<a id='x-28JOURNAL-3APRETTIFY-EVENT-20FUNCTION-29'></a>

- [function] **PRETTIFY-EVENT** *EVENT DEPTH STREAM*

    Write `EVENT` to `STREAM` in a somewhat human-friendly format. This
    is the function [`PPRINT-JOURNAL`][2123], [`PPRINT-EVENTS`][74d2], and [Tracing][7849] use by
    default. In addition to the basic example in `PPRINT-EVENTS`,
    [decoration][ca1a] on events is printed before normal, indented output like
    this:
    
    ```
    (pprint-events '((:leaf "About to sleep" :time "19:57:00" :function "FOO")))
    ..
    .. 19:57:00 FOO: About to sleep
    ```
    
    `DEPTH` is the nesting level of the `EVENT`. Top-level events have depth
    0. `PRETTIFY-EVENT` prints indents the output, after printing the
    decorations, by 2 spaces per depth.

Instead of collecting events and then printing them, events can
be pretty-printed to a stream as they generated. This is
accomplished with [Pretty-printing journals][eeda], discussed in detail later, in
the following way:

```
(let ((journal (make-pprint-journal)))
  (with-journaling (:record journal)
    (journaled (foo) "Hello")))
..
.. (FOO)
.. => "Hello"
```

Note that [Pretty-printing journals][eeda] are not tied to [`WITH-JOURNALING`][234e] and are
most often used for [Logging][77df] and [Tracing][7849].

<a id='x-28JOURNAL-3A-40JOURNAL-ERROR-HANDLING-20MGL-PAX-3ASECTION-29'></a>

### 6.6 Error handling

<a id='x-28JOURNAL-3AJOURNALING-FAILURE-20CONDITION-29'></a>

- [condition] **JOURNALING-FAILURE** *SERIOUS-CONDITION*

    Signalled during the dynamic extent of
    [`WITH-JOURNALING`][234e] when an error threatens to leave the journaling
    mechanism in an inconsistent state. These include I/O errors
    encountered reading or writing journals by `WITH-JOURNALING`,
    [`JOURNALED`][4f52], [`LOGGED`][0765], [`WITH-REPLAY-FILTER`][fa00], [`SYNC-JOURNAL`][6bc6], but also
    `STORAGE-CONDITION`s, assertion failures, and errors calling
    `JOURNALED`'s `VALUES` and `CONDITION` function arguments. Crucially, this
    does not apply to non-local exits from other code, such as `JOURNALED`
    [block][6572]s, whose error handling is largely unaltered (see [Out-events][5721]
    and [Replay failures][588a]).
    
    In general, any [non-local exit][e17e] from critical parts of the code is
    turned into a `JOURNALING-FAILURE` to protect the integrity of the
    [`RECORD-JOURNAL`][35c4]. The condition that caused the unwinding is in
    [`JOURNALING-FAILURE-EMBEDDED-CONDITION`][b9bc], or `NIL` if it was a pure
    `@NON-LOCAL-EXIT` like `THROW`. This is a `SERIOUS-CONDITION`, not to be
    handled within `WITH-JOURNALING`.
    
    After a `JOURNALING-FAILURE`, the journaling mechanism cannot be
    trusted anymore. The [`REPLAY-JOURNAL`][103b] might have failed a read and be
    out-of-sync. The `RECORD-JOURNAL` may have missing events (or even
    half-written events with [`FILE-JOURNAL`][f6b2]s without `SYNC`, see
    [Synchronization strategies][355b]), and further writes to it would risk
    replayability, which is equivalent to database corruption. Thus,
    upon signalling `JOURNALING-FAILURE`, [`JOURNAL-STATE`][3631] is set to
    
    - `:COMPLETED` if the journal is in state `:RECORDING` or `:LOGGING` and
      the transition to `:RECORDING` was reflected in storage,
    
    - else it is set to `:FAILED`.
    
    After a `JOURNALING-FAILURE`, any further attempt within the affected
    `WITH-JOURNALING` to use the critical machinery mentioned
    above (`JOURNALED`, `LOGGED`, etc) resignals the same journal failure
    condition. As a consequence, the record journal cannot be changed
    and the only way to recover is to leave `WITH-JOURNALING`. This does
    not affect processing in other threads, which by design cannot write
    to the record journal.
    
    Note that in contrast with `JOURNALING-FAILURE` and [`REPLAY-FAILURE`][955f],
    which necessitate leaving `WITH-JOURNALING` to recover from, the other
    conditions - [`JOURNAL-ERROR`][571f], and [`STREAMLET-ERROR`][4dc9] - are subclasses of
    `ERROR` as the their handling need not be so heavy-handed.

<a id='x-28JOURNAL-3AJOURNALING-FAILURE-EMBEDDED-CONDITION-20-28MGL-PAX-3AREADER-20JOURNAL-3AJOURNALING-FAILURE-29-29'></a>

- [reader] **JOURNALING-FAILURE-EMBEDDED-CONDITION** *JOURNALING-FAILURE* *(:EMBEDDED-CONDITION)*

<a id='x-28JOURNAL-3ARECORD-UNEXPECTED-OUTCOME-20CONDITION-29'></a>

- [condition] **RECORD-UNEXPECTED-OUTCOME**

    Signalled (with `SIGNAL`: this is not an `ERROR`) by
    [`JOURNALED`][4f52] when a [`VERSIONED-EVENT`][ffc4] or an [`EXTERNAL-EVENT`][eed7] had an
    [unexpected outcome][f57e] while in [`JOURNAL-STATE`][3631] `:RECORDING`. Upon
    signalling this condition, `JOURNAL-STATE` is set to `:LOGGING`, thus no
    more events can be recorded that will affect replay of the journal
    being recorded. The event that triggered this condition is recorded
    in state `:LOGGING`, with its version downgraded. Since
    [Replay][0dc7] (except [Invoked][4492]) is built on the assumption that control
    flow is deterministic, an unexpected outcome is significant because
    it makes this assumption to hold unlikely.
    
    Also see [`REPLAY-UNEXPECTED-OUTCOME`][8d93].

<a id='x-28JOURNAL-3ADATA-EVENT-LOSSAGE-20CONDITION-29'></a>

- [condition] **DATA-EVENT-LOSSAGE** *JOURNALING-FAILURE*

    Signalled when a [data event][ecce] is about to be recorded
    in [`JOURNAL-STATE`][3631] `:MISMATCHED` or `:LOGGING`. Since the data event will
    not be replayed that constitutes data loss.

<a id='x-28JOURNAL-3AJOURNAL-ERROR-20CONDITION-29'></a>

- [condition] **JOURNAL-ERROR** *ERROR*

    Signalled by [`WITH-JOURNALING`][234e], [`WITH-BUNDLE`][0ddc] and by
    [Log record][3380]. It is also signalled by the low-level streamlet
    interface (see [Streamlets reference][2453]).

<a id='x-28JOURNAL-3AEND-OF-JOURNAL-20CONDITION-29'></a>

- [condition] **END-OF-JOURNAL** *JOURNAL-ERROR*

    This might be signalled by the replay mechanism if
    [`WITH-JOURNALING`][234e]'s `REPLAY-EOJ-ERROR-P` is true. Unlike
    [`REPLAY-FAILURE`][955f]s, this does not affect [`JOURNAL-STATE`][3631] of
    [`RECORD-JOURNAL`][35c4]. At a lower level, it is signalled by [`READ-EVENT`][6ed4] upon
    reading past the end of the [`JOURNAL`][86bc] if `EOJ-ERROR-P`.

<a id='x-28JOURNAL-3A-40LOGGING-20MGL-PAX-3ASECTION-29'></a>

## 7 Logging

Imagine a utility library called glib.

##### Default to muffling

```
(defvar *glib-log* nil)
(defvar *patience* 1)

(defun sl33p (seconds)
  (logged (*glib-log*) "Sleeping for ~As." seconds)
  (sleep (* *patience* seconds)))
```

Glib follows the recommendation to have a special variable globally
bound to `NIL` by default. The value of `*GLIB-LOG*` is the journal to
which glib log messages will be routed. Since it's `NIL`, the log
messages are muffled, and to record any log message, we need to
change its value.

##### Routing logs to a journal

Let's send the logs to a [`PPRINT-JOURNAL`][2123]:

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
messages are included by default in the [`RECORD-JOURNAL`][35c4]. In this
example, the special `*GLIB-LOG*` acts like a log category for all
the log messages of the glib library (currently one).

##### Rerouting a category

Next, we route `*GLIB-LOG*` to wherever `*APP-LOG*` is pointing by
binding `*GLIB-LOG*` *to the symbol* `*APP-LOG*`.

```
(defvar *app-log* nil)

(let ((*glib-log* '*app-log*)
      (*app-log* (make-pprint-journal :pretty nil)))
  (sl33p 0.01))
..
.. (:LEAF "Sleeping for 0.01s.")
```

Note how pretty-printing was turned off and we see the [`LEAF-EVENT`][fef8]
generated by [`LOGGED`][0765] in its raw plist form.

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

`LOGGED` is for single messages. [`JOURNALED`][4f52] can provide nested context:

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


<a id='x-28JOURNAL-3A-40CUSTOMIZING-LOGS-20MGL-PAX-3ASECTION-29'></a>

### 7.1 Customizing logs

Customizing the output format is possible if we don't necessarily
expect to be able to read the logs back programmatically. There is
an example in [Tracing][7849], which is built on [Pretty-printing journals][eeda].

Here, we discuss how to make logs more informative.

<a id='x-28JOURNAL-3A-40DECORATION-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **decoration**

    [`JOURNAL-LOG-DECORATOR`][d3f7] adds additional data to [`LOG-EVENT`][3a51]s as they
    are written to the journal. This data is called decoration and it is
    to capture the context in which the event was triggered. See
    [`MAKE-LOG-DECORATOR`][e04b] for a typical example. Decorations, since they
    can be on `LOG-EVENT`s only, do not affect [Replay][0dc7]. Decorations are
    most often used with [Pretty-printing][9607].

<a id='x-28JOURNAL-3AJOURNAL-LOG-DECORATOR-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3AJOURNAL-29-29'></a>

- [accessor] **JOURNAL-LOG-DECORATOR** *JOURNAL* *(:LOG-DECORATOR = NIL)*

    If non-NIL, a function to add [decoration][ca1a] to
    [`LOG-EVENT`][3a51]s before they are written to a journal. The only allowed
    transformation is to *append* a plist to the event, which is a
    plist itself. The keys can be anything.

<a id='x-28JOURNAL-3AMAKE-LOG-DECORATOR-20FUNCTION-29'></a>

- [function] **MAKE-LOG-DECORATOR** *&KEY THREAD TIME REAL-TIME RUN-TIME*

    Return a function suitable as [`JOURNAL-LOG-DECORATOR`][d3f7] that may add
    the name of the thread, a timestamp, the internal real-time or
    run-time (both in seconds) to events. `THREAD`, `TIME`, `REAL-TIME` and
    `RUN-TIME` are [boolean-valued symbol][dcba]s.
    
    ```
    (funcall (make-log-decorator :thread t :time t :real-time t :run-time t)
             (make-leaf-event :foo))
    => (:LEAF :FOO :TIME "2020-08-31T13:38:58.129178+02:00"
        :REAL-TIME 66328.82 :RUN-TIME 98.663 :THREAD "worker")
    ```


<a id='x-28JOURNAL-3A-40LOG-RECORD-20MGL-PAX-3ASECTION-29'></a>

### 7.2 Log record

[`WITH-JOURNALING`][234e] and [`WITH-BUNDLE`][0ddc] control replaying and recording
within their dynamic extent, which is rather a necessity because
[Replay][0dc7] needs to read the events in the same order as the [`JOURNALED`][4f52]
[block][6572]s are being executed. However, [`LOG-EVENT`][3a51]s do not affect replay
so we can allow more flexibility in routing them.

The `LOG-RECORD` argument of `JOURNALED` and [`LOGGED`][0765] controls where
`LOG-EVENT`s are written both within `WITH-JOURNALING` and without. The
algorithm to determine the target journal is this:

1. If `LOG-RECORD` is `:RECORD`, then `(RECORD-JOURNAL)` is returned.

2. If `LOG-RECORD` is `NIL`, then it is returned.

3. If `LOG-RECORD` is a [`JOURNAL`][86bc], then it is returned.

4. If `LOG-RECORD` is symbol (other than `NIL`), then the `SYMBOL-VALUE`
   of that symbol is assigned to `LOG-RECORD` and we go to step 1.

If the return value is `NIL`, then the event will not be written
anywhere, else it is written to the journal returned.

This is reminiscent of `SYNONYM-STREAM`s, also in that it is possible
end up in cycles in the resolution. For this reason, the algorithm
stop with a [`JOURNAL-ERROR`][571f] after 100 iterations.

##### Interactions

Events may be written to `LOG-RECORD` even without an enclosing
`WITH-JOURNALING`, and it does not affect the [`JOURNAL-STATE`][3631]. However,
it is a `JOURNAL-ERROR` to write to a `:COMPLETED` journal (see
`JOURNAL-STATE`).

When multiple threads log to the same journal it is guaranteed that
individual events are written atomically, but frames from different
threads do not necessarily nest. To keep the log informative, the
name of thread may be added to the events as [decoration][ca1a].

Also see notes on thread [Safety][7224].

<a id='x-28JOURNAL-3A-40LOGGING-WITH-LEAVES-20MGL-PAX-3ASECTION-29'></a>

### 7.3 Logging with leaf-events

<a id='x-28JOURNAL-3ALOGGED-20MGL-PAX-3AMACRO-29'></a>

- [macro] **LOGGED** *(&OPTIONAL (LOG-RECORD :RECORD)) FORMAT-CONTROL &REST FORMAT-ARGS*

    `LOGGED` creates a single [`LEAF-EVENT`][fef8] whose name is the string
    constructed by `FORMAT`. For example:
    
    ```
    (with-journaling (:record t)
      (logged () "Hello, ~A." "world")
      (list-events))
    => ((:LEAF "Hello, world."))
    ```
    
    `LEAF-EVENT`s are [`LOG-EVENT`][3a51]s with no separate in- and out-events. They
    have an [`EVENT-NAME`][2b45] and no other properties. Use `LOGGED` for
    point-in-time textual log messages, and [`JOURNALED`][4f52] with `VERSION`
    `NIL` (i.e. [`FRAMED`][806e]) to provide context.
    
    Also see [Log record][3380].

<a id='x-28JOURNAL-3A-40TRACING-20MGL-PAX-3ASECTION-29'></a>

## 8 Tracing

[`JTRACE`][254d] behaves similarly to `CL:TRACE`, but deals [non-local exit][e17e]s
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

In the above, [`*TRACE-JOURNAL*`][5428] was bound locally to keep the example
from wrecking the global default, but the same effect could be
achieved by `SETF`ing [`PPRINT-JOURNAL-PRETTIFIER`][a4ee],
[`PPRINT-JOURNAL-STREAM`][bce9] and [`JOURNAL-LOG-DECORATOR`][d3f7].

<a id='x-28JOURNAL-3AJTRACE-20MGL-PAX-3AMACRO-29'></a>

- [macro] **JTRACE** *&REST NAMES*

    Like `CL:TRACE`, `JTRACE` takes a list of symbols. When functions
    denoted by those `NAMES` are invoked, their names, arguments and
    outcomes are printed in human readable form to `*TRACE-OUTPUT*`. These
    values may not be [readable][531d], `JTRACE` does not care.
    
    The format of the output is the same as that of [`PPRINT-EVENTS`][74d2].
    Behind the scenes, `JTRACE` encapsulates the global functions with
    `NAMES` in wrapper that behaves as if `FOO` in the example above was
    defined like this:
    
    ```
    (defun foo (x)
      (framed (foo :args `(,x) :log-record *trace-journal*)
        (1+ x)))
    ```
    
    If `JTRACE` is invoked with no arguments, it returns the list of
    symbols currently traced.
    
    On Lisps other than SBCL, where a function encapsulation facility is
    not available or it is not used by Journal, `JTRACE` simply sets
    `SYMBOL-FUNCTION`. This solution loses the tracing encapsulation when
    the function is recompiled. On these platforms, `(JTRACE)` also
    retraces all functions that should be traced, but aren't.
    
    The main advantage of `JTRACE` over `CL:TRACE` is the ability to trace
    errors, not just normal return values. As it is built on [`JOURNALED`][4f52],
    it can also detect - somewhat heuristically - `THROW`s and similar.

<a id='x-28JOURNAL-3AJUNTRACE-20MGL-PAX-3AMACRO-29'></a>

- [macro] **JUNTRACE** *&REST NAMES*

    Like `CL:UNTRACE`, `JUNTRACE` makes it so that the global functions
    denoted by the symbols `NAMES` are no longer traced by [`JTRACE`][254d]. When
    invoked with no arguments, it untraces all traced functions.

<a id='x-28JOURNAL-3A-2ATRACE-PRETTY-2A-20VARIABLE-29'></a>

- [variable] **\*TRACE-PRETTY\*** *T*

    If `*TRACE-PRETTY*` is true, then [`JTRACE`][254d] produces output like
    [`PPRINT-EVENTS`][74d2], else it's like [`PRINT-EVENTS`][3bd9].

<a id='x-28JOURNAL-3A-2ATRACE-THREAD-2A-20VARIABLE-29'></a>

- [variable] **\*TRACE-THREAD\*** *NIL*

    Controls whether to decorate the trace with the name of the
    originating thread. See [`MAKE-LOG-DECORATOR`][e04b].

<a id='x-28JOURNAL-3A-2ATRACE-TIME-2A-20VARIABLE-29'></a>

- [variable] **\*TRACE-TIME\*** *NIL*

    Controls whether to decorate the trace with a timestamp. See
    [`MAKE-LOG-DECORATOR`][e04b].

<a id='x-28JOURNAL-3A-2ATRACE-REAL-TIME-2A-20VARIABLE-29'></a>

- [variable] **\*TRACE-REAL-TIME\*** *NIL*

    Controls whether to decorate the trace with the internal real-time.
    See [`MAKE-LOG-DECORATOR`][e04b].

<a id='x-28JOURNAL-3A-2ATRACE-RUN-TIME-2A-20VARIABLE-29'></a>

- [variable] **\*TRACE-RUN-TIME\*** *NIL*

    Controls whether to decorate the trace with the internal run-time.
    See [`MAKE-LOG-DECORATOR`][e04b].

<a id='x-28JOURNAL-3A-2ATRACE-JOURNAL-2A-20VARIABLE-29'></a>

- [variable] **\*TRACE-JOURNAL\*** *#\<PPRINT-JOURNAL :NEW\>*

    The [`JOURNAL`][86bc] where [`JTRACE`][254d] writes [`LOG-EVENT`][3a51]s. By default it is a
    [`PPRINT-JOURNAL`][2123] that sets up a `SYNONYM-STREAM` to `*TRACE-OUTPUT*` and
    sends its output there. It pays attention to [`*TRACE-PRETTY*`][c7bd], and its
    log decorator is affected by [`*TRACE-TIME*`][958b] and [`*TRACE-THREAD*`][fc8e].
    However, by changing [`JOURNAL-LOG-DECORATOR`][d3f7] and
    [`PPRINT-JOURNAL-PRETTIFIER`][a4ee] content and output can be customized.

<a id='x-28JOURNAL-3A-40JOURNAL-SLIME-INTEGRATION-20MGL-PAX-3ASECTION-29'></a>

### 8.1 Slime integration

[Slime](https://common-lisp.net/project/slime/) by default binds
`C-c C-t` to toggling `CL:TRACE`. To integrate [`JTRACE`][254d] into Slime, add
the following ELisp snippet to your Emacs initialization file or
load `src/journal.el`:

<a id='x-28JOURNAL-3A-3AJOURNAL-2EEL-20-28MGL-PAX-3AINCLUDE-20-23P-22-2Fhome-2Fmelisgl-2Fown-2Fjournal-2Fsrc-2Fjournal-2Eel-22-20-3AHEADER-NL-20-22-60-60-60elisp-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29'></a>

```elisp
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
```

Since [`JTRACE`][254d] lacks some features of `CL:TRACE`, most notably that of
tracing non-global functions, it is assigned a separate binding,
`C-c C-j`.

<a id='x-28JOURNAL-3A-40REPLAY-20MGL-PAX-3ASECTION-29'></a>

## 9 Replay

During replay, code is executed normally with special rules for
[block][6572]s. There are two modes for dealing with blocks: replaying the
code or replaying the outcome. When code is replayed, upon entering
and leaving a block, the events generated are matched to events read
from the journal being replayed. If the events don't match, a
[`REPLAY-FAILURE`][955f] is signaled which marks the record journal as having
failed the replay. This is intended to make sure that the state of
the program during the replay matches the state at the time of
recording. In the other mode, when the outcome is replayed, a block
may not be executed at all, but its recorded outcome is
reproduced (e.g. the recorded return values are returned).

Replay can be only be initiated with [`WITH-JOURNALING`][234e] (or its close
kin [`WITH-BUNDLE`][0ddc]). After the per-event processing described below,
when `WITH-JOURNALING` finishes, it might signal [`REPLAY-INCOMPLETE`][32a1] if
there are unprocessed non-log events left in the replay journal.

Replay is deemed successful or failed depending on whether all
events are replayed from the replay journal without a
`REPLAY-FAILURE`. A journal that records events from a successful
replay can be used in place of the journal that was replayed, and so
on. The logic of replacing journals with their successful replays is
automated by [Bundles][5b0f]. `WITH-JOURNALING` does not allow replay from
journals that were failed replays themselves. The mechanism, in
terms of which tracking success and failure of replays is
implemented, revolves around [`JOURNAL-STATE`][3631] and
[`EVENT-VERSION`][917c]s, which we discuss next.

<a id='x-28JOURNAL-3AJOURNAL-STATE-20TYPE-29'></a>

- [type] **JOURNAL-STATE**

    [`JOURNAL`][86bc]'s state, with respect to replay, is updated during
    [`WITH-JOURNALING`][234e]. This possible states are:
    
    - **`:NEW`**: This journal was just created, but never recorded to.
    
    - **`:REPLAYING`**: Replaying events has started, some events may have
      been replayed successfuly, but there are more, non-log events to
      replay.
    
    - **`:MISMATCHED`**: There was a [`REPLAY-FAILURE`][955f]. In this state,
      [`VERSIONED-EVENT`][ffc4]s generated are downgraded to [`LOG-EVENT`][3a51]s,
      [`EXTERNAL-EVENT`][eed7]s and [Invoked][4492] trigger a [`DATA-EVENT-LOSSAGE`][1668].
    
    - **`:RECORDING`**: All events from the replay journal were
      successfully replayed and now new events are being recorded
      without being matched to the replay journal.
    
    - **`:LOGGING`**: There was a [`RECORD-UNEXPECTED-OUTCOME`][cf72]. In this
      state, `VERSIONED-EVENT`s generated are downgraded to `LOG-EVENT`s,
      `EXTERNAL-EVENT`s and `INVOKED` trigger a `DATA-EVENT-LOSSAGE`.
    
    - **`:FAILED`**: The journal is to be discarded. It encountered a
      [`JOURNALING-FAILURE`][6db0] or a `REPLAY-FAILURE` without completing the
      replay and reaching `:RECORDING`.
    
    - **`:COMPLETED`**: All events were successfully replayed and
      `WITH-JOURNALING` finished or a `JOURNALING-FAILURE` occurred while
      `:RECORDING` or `:LOGGING`.
    
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
    
    `:NEW` is the starting state. It is a [`JOURNAL-ERROR`][571f] to attempt to
    write to journals in `:COMPLETED`. Note that once in `:RECORDING`, the
    only possible terminal state is `:COMPLETED`.

<a id='x-28JOURNAL-3A-40JOURNALED-FOR-REPLAY-20MGL-PAX-3ASECTION-29'></a>

### 9.1 Journaled for replay

The following arguments of [`JOURNALED`][4f52] control behaviour under replay.

- `VERSION`: see [`EVENT-VERSION`][917c] below.

- `INSERTABLE` controls whether [`VERSIONED-EVENT`][ffc4]s and [`EXTERNAL-EVENT`][eed7]s
  may be replayed with the *insert* replay strategy (see
  [The replay strategy][3c00]). Does not affect [`LOG-EVENT`][3a51]s, that are always
  \_insert\_ed. Note that inserting `EXTERNAL-EVENT`s while `:REPLAYING`
  is often not meaningful (e.g. asking the user for input may lead
  to a [`REPLAY-FAILURE`][955f]). See [`PEEK-REPLAY-EVENT`][63a5] for an example on how
  to properly insert these kinds of `EXTERNAL-EVENT`s.

- `REPLAY-VALUES`, a function or `NIL`, may be called with [`EVENT-OUTCOME`][95b8]
  when replaying and `:VERSION` `:INFINITY`. `NIL` is equivalent to
  `VALUES-LIST`. See [`VALUES<-`][e29b] for an example.

- `REPLAY-CONDITION`, a function or `NIL`, may be called with
  `EVENT-OUTCOME` (the return value of the function provided as
  `CONDITION`) when replaying and `:VERSION` is `:INFINITY`. `NIL` is
  equivalent to `ERROR`. Replaying conditions is cumbersome and best
  avoided.


<a id='x-28JOURNAL-3A-2AFORCE-INSERTABLE-2A-20VARIABLE-29'></a>

- [variable] **\*FORCE-INSERTABLE\*** *NIL*

    The default value of the `INSERTABLE` argument of [`JOURNALED`][4f52] for
    [`VERSIONED-EVENT`][ffc4]s. Binding this to `T` allows en-masse structural
    upgrades in combination with [`WITH-REPLAY-FILTER`][fa00]. Does not affect
    [`EXTERNAL-EVENT`][eed7]s. See [Upgrades and replay][1acb].

<a id='x-28JOURNAL-3AEVENT-VERSION-20TYPE-29'></a>

- [type] **EVENT-VERSION**

    An event's version is either `NIL`, a positive fixnum, or `:INFINITY`,
    which correspond to [`LOG-EVENT`][3a51]s, [`VERSIONED-EVENT`][ffc4]s, and
    [`EXTERNAL-EVENT`][eed7]s, respectively, and have an increasingly strict
    behaviour with regards to [Replay][0dc7]. All `EVENTs` have versions. The
    versions of the in- and out-events belonging to the same [frame][1452] are
    the same.

<a id='x-28JOURNAL-3ALOG-EVENT-20TYPE-29'></a>

- [type] **LOG-EVENT**

    Events with [`EVENT-VERSION`][917c] `NIL` called log events. During [Replay][0dc7],
    they are never matched to events from the replay journal, and log
    events in the replay do not affect events being recorded either.
    These properties allow log events to be recorded in arbitrary
    journals with [`JOURNALED`][4f52]'s `LOG-RECORD` argument. The convenience macro
    [`FRAMED`][806e] is creating frames of log-events, while the [`LOGGED`][0765] generates
    a log-event that's a [`LEAF-EVENT`][fef8].

<a id='x-28JOURNAL-3AVERSIONED-EVENT-20TYPE-29'></a>

- [type] **VERSIONED-EVENT**

    Events with a positive integer [`EVENT-VERSION`][917c] are called
    versioned events. In [Replay][0dc7], they undergo consistency checks unlike
    [`LOG-EVENT`][3a51]s, but the rules for them are less strict than for
    [`EXTERNAL-EVENT`][eed7]s. In particular, higher versions are always
    considered compatible with lower versions, they become an *upgrade*
    in terms of the [The replay strategy][3c00], and versioned events can be
    inserted into the record without a corresponding [replay event][ca07] with
    [`JOURNALED`][4f52]'s `INSERTABLE`.
    
    If a `VERSIONED-EVENT` has an [unexpected outcome][f57e],
    [`RECORD-UNEXPECTED-OUTCOME`][cf72] is signalled.

<a id='x-28JOURNAL-3AEXTERNAL-EVENT-20TYPE-29'></a>

- [type] **EXTERNAL-EVENT**

    Events with [`EVENT-VERSION`][917c] `:INFINITY` are called external events.
    They are like [`VERSIONED-EVENT`][ffc4]s whose version was bumped all the way
    to infinity, which rules out easy, non-matching upgrades. Also, they
    are never inserted to the record without a matching replay
    event (see [The replay strategy][3c00]).
    
    In return for these restrictions, external events can be replayed
    without running the corresponding [block][6572] (see
    [Replaying the outcome][b6d1]). This allows their out-event variety, called
    [data event][ecce]s, to be non-deterministic. Data events play a crucial
    role in [Persistence][98d3].
    
    If an `EXTERNAL-EVENT` has an [unexpected outcome][f57e],
    [`RECORD-UNEXPECTED-OUTCOME`][cf72] is signalled.

Built on top of [`JOURNALED`][4f52], the macros below record a pair of
[In-events][1d28] and [Out-events][5721], but differ in how they are replayed and
the requirements on their [block][6572]s. The following table names the
type of [`EVENT`][0f76] produced ([`Event`][0f76]), how [In-events][1d28] are
replayed (`In-e.`), whether the block is always run (`Run`), how
[Out-events][5721] are replayed (`Out-e.`), whether the block must be
deterministic (`Det`) or side-effect free (`SEF`).

    |          | Event     | In-e.  | Run | Out-e. | Det | SEF |
    |----------+-----------+--------+-----+--------+-----+-----|
    | FRAMED   | log       | skip   | y   | skip   | n   | n   |
    | CHECKED  | versioned | match  | y   | match  | y   | n   |
    | REPLAYED | external  | match  | n   | replay | n   | y   |
    | INVOKED  | versioned | replay | y   | match  | y   | n   |

Note that the replay-replay combination is not implemented because
there is nowhere to return values from replay-triggered functions.

<a id='x-28JOURNAL-3AFRAMED-20MGL-PAX-3AMACRO-29'></a>

- [macro] **FRAMED** *(NAME &KEY LOG-RECORD ARGS VALUES CONDITION) &BODY BODY*

    A wrapper around [`JOURNALED`][4f52] to produce [frame][1452]s of [`LOG-EVENT`][3a51]s. That
    is, `VERSION` is always `NIL`, and some irrelevant arguments are
    omitted. The related [`LOGGED`][0765] creates a single [`LEAF-EVENT`][fef8].
    
    With `FRAMED`, `BODY` is always run and no [`REPLAY-FAILURE`][955f]s are
    triggered. `BODY` is not required to be deterministic and it may have
    side-effects.

<a id='x-28JOURNAL-3ACHECKED-20MGL-PAX-3AMACRO-29'></a>

- [macro] **CHECKED** *(NAME &KEY (VERSION 1) ARGS VALUES CONDITION INSERTABLE) &BODY BODY*

    A wrapper around [`JOURNALED`][4f52] to produce [frame][1452]s of [`VERSIONED-EVENT`][ffc4]s.
    `VERSION` defaults to 1. `CHECKED` is for ensuring that supposedly
    deterministic processing does not veer off the replay.
    
    With `CHECKED`, `BODY` - which must be deterministic - is always run and
    [`REPLAY-FAILURE`][955f]s are triggered when the events generated do not match
    the events in the replay journal. `BODY` may have side-effects.
    
    For further discussion of determinism, see [`REPLAYED`][c2fb].

<a id='x-28JOURNAL-3AREPLAYED-20MGL-PAX-3AMACRO-29'></a>

- [macro] **REPLAYED** *(NAME &KEY ARGS VALUES CONDITION INSERTABLE REPLAY-VALUES REPLAY-CONDITION) &BODY BODY*

    A wrapper around [`JOURNALED`][4f52] to produce [frame][1452]s of [`EXTERNAL-EVENT`][eed7]s.
    `VERSION` is `:INFINITY`. `REPLAYED` is for primarily for marking and
    isolating non-deterministic processing.
    
    With `REPLAYED`, the [`IN-EVENT`][e10e] is checked for consistency with the
    replay (as with [`CHECKED`][cc3d]), but `BODY` is not run (assuming it has a
    recorded [expected outcome][32e0]) and the outcome in the [`OUT-EVENT`][cba8] is
    reproduced (see [Replaying the outcome][b6d1]). For this scheme to work,
    `REPLAYED` requires its `BODY` to be side-effect free, but it may be
    non-deterministic.

<a id='x-28JOURNAL-3AINVOKED-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **Invoked**

    Invoked refers to functions and blocks defined by [`DEFINE-INVOKED`][635d] or
    [`FLET-INVOKED`][48bb]. Invoked frames may be recorded in response to
    asynchronous events, and at replay the presence of its in-event
    triggers the execution of the function associated with the name of
    the event.
    
    On one hand, [`FRAMED`][806e], [`CHECKED`][cc3d], [`REPLAYED`][c2fb] or plain [`JOURNALED`][4f52] have
    [In-events][1d28] that are always predictable from the code and the
    preceding events. The control flow - on the level of recorded frames
    - is deterministic in this sense. On the other hand, Invoked encodes
    in its [`IN-EVENT`][e10e] what function to call next, introducing
    non-deterministic control flow.
    
    By letting events choose the code to run, Invoked resembles typical
    [Event Sourcing][event-sourcing] frameworks. When Invoked is used
    exclusively, the journal becomes a sequence of events. In contrast,
    `JOURNALED` and its wrappers put code first, and the journal will be a
    projection of the call tree.

<a id='x-28JOURNAL-3ADEFINE-INVOKED-20MGL-PAX-3AMACRO-29'></a>

- [macro] **DEFINE-INVOKED** *FUNCTION-NAME ARGS (NAME &KEY (VERSION 1) INSERTABLE) &BODY BODY*

    `DEFINE-INVOKED` is intended for recording asynchronous function
    invocations like event or signal handlers. It defines a function
    that records [`VERSIONED-EVENT`][ffc4]s with `ARGS` set to the actual arguments.
    At replay, it is invoked whenever the recorded [`IN-EVENT`][e10e] becomes the
    [replay event][ca07].
    
    `DEFUN` and [`CHECKED`][cc3d] rolled into one, `DEFINE-INVOKED` defines a
    top-level function with `FUNCTION-NAME` and `ARGS` (only simple
    positional arguments are allowed) and wraps `CHECKED` with `NAME`, the
    same `ARGS` and `INSERTABLE` around `BODY`. Whenever an `IN-EVENT` becomes
    the `REPLAY-EVENT` and it has a `DEFINE-INVOKED` defined with the name
    of the event, then `FUNCTION-NAME` is invoked with [`EVENT-ARGS`][5da8].
    
    While `BODY`'s return values are recorded as usual, the defined
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
    
    The above can be alternatively implemented with [`REPLAYED`][c2fb] explicitly
    encapsulating the non-determinism:
    
    ```
    (let ((x (replayed (choose) (random 2))))
      (if (zerop x)
          (checked (foo :args `(,x))
            (setq *state* (1+ x)))
          (checked (bar :args `(,x))
            (setq *state* (+ 2 x)))))
    ```


<a id='x-28JOURNAL-3AFLET-INVOKED-20MGL-PAX-3AMACRO-29'></a>

- [macro] **FLET-INVOKED** *DEFINITIONS &BODY BODY*

    Like [`DEFINE-INVOKED`][635d], but with `FLET` instead of `DEFUN`. The event
    name and the function are associated in the dynamic extent of `BODY`.
    [`WITH-JOURNALING`][234e] does not change the bindings. The example in
    `DEFINE-INVOKED` can be rewritten as:
    
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


<a id='x-28JOURNAL-3A-40BUNDLES-20MGL-PAX-3ASECTION-29'></a>

### 9.2 Bundles

Consider replaying the same code repeatedly, hoping to make
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

With [`FILE-JOURNAL`][f6b2]s, the motivating example above would be even more
complicated, but [`FILE-BUNDLE`][eb5d]s work the same way as
[`IN-MEMORY-BUNDLE`][2314]s.

<a id='x-28JOURNAL-3AWITH-BUNDLE-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-BUNDLE** *(BUNDLE) &BODY BODY*

    This is like [`WITH-JOURNALING`][234e] where the [`REPLAY-JOURNAL`][103b] is the last
    successfully completed one in `BUNDLE`, and the [`RECORD-JOURNAL`][35c4] is a
    new one created in `BUNDLE`. When `WITH-BUNDLE` finishes, the record
    journal is in [`JOURNAL-STATE`][3631] `:FAILED` or `:COMPLETED`.
    
    To avoid accumulating useless data, the new record is immediately
    deleted when `WITH-BUNDLE` finishes if it has not diverged from the
    replay journal (see [`JOURNAL-DIVERGENT-P`][f748]). Because `:FAILED` journals
    are always divergent in this sense, they are deleted instead based
    on whether there is already a previous failed journal in the bundle
    and the new record is identical to that journal (see
    [`IDENTICAL-JOURNALS-P`][05ca]).
    
    It is a [`JOURNAL-ERROR`][571f] to have concurrent or nested `WITH-BUNDLE`s on
    the same bundle.

<a id='x-28JOURNAL-3A-40THE-REPLAY-STRATEGY-20MGL-PAX-3ASECTION-29'></a>

### 9.3 The replay strategy

The replay process for both [In-events][1d28] and [Out-events][5721] starts by
determining how the generated event (the *new* event from now on)
shall be replayed. Roughly, the decision is based on the `NAME` and
`VERSION` of the new event and the [replay event][ca07] (the next event to be
read from the replay). There are four possible strategies:

- **match**: A new in-event must match the replay event in its `ARGS`.
  See [Matching in-events][e04a] for details. A new out-event must match
  the replay event's `EXIT` and `OUTCOME`, see [Matching out-events][21d1].

- **upgrade**: The new event is not matched to any replay event, but
  an event is consumed from the replay journal. This happens if next
  new event has the same name as the replay event, but its version
  is higher.

- **insert**: The new event is not matched to any replay event, and
  no events are consumed from the replay journal, which may be
  empty. This is always the case for new [`LOG-EVENT`][3a51]s and when there
  are no more events to read from the replay journal (unless
  `REPLAY-EOJ-ERROR-P`). For [`VERSIONED-EVENT`][ffc4]s, it is affected by
  setting [`JOURNALED`][4f52]'s `INSERTABLE` to true (see
  [Journaled for replay][0bc8]).

    The out-event's strategy is always *insert* if the strategy for
    the corresponding in-event was *insert*.

- Also, [`END-OF-JOURNAL`][9c7e], [`REPLAY-NAME-MISMATCH`][d031] and
  [`REPLAY-VERSION-DOWNGRADE`][2efb] may be signalled. See the algorithm below
  details.

The strategy is determined by the following algorithm, invoked
whenever an event is generated by a journaled [block][6572]:

1. Log events are not matched to the replay. If the new event is a
   log event or a [`REPLAY-FAILURE`][955f] has been signalled before (i.e. the
   record journal's [`JOURNAL-STATE`][3631] is `:MISMATCHED`), then **insert**
   is returned.

2. Else, log events to be read in the replay journal are skipped,
   and the next unread, non-log event is peeked at (without
   advancing the replay journal).

    - **end of replay**: If there are no replay events left, then:

        - If `REPLAY-EOJ-ERROR-P` is `NIL` in [`WITH-JOURNALING`][234e] (the
          default), **insert** is returned.

        - If `REPLAY-EOJ-ERROR-P` is true, then **`END-OF-JOURNAL`**
          is signalled.

    - **mismatched name**: Else, if the next unread replay event's
      name is not `EQUAL` to the name of the new event, then:

        - For `VERSIONED-EVENT`s, **`REPLAY-NAME-MISMATCH`** is
          signalled if `INSERTABLE` is `NIL`, else **insert** is
          returned.

        - For [`EXTERNAL-EVENT`][eed7]s, **`REPLAY-NAME-MISMATCH`** is
          signalled.

    - **matching name**: Else, if the name of the next unread event
      in the replay journal is `EQUAL` to the name of new event, then
      it is chosen as the *replay* event.

        - If the replay event's version is higher than the new
          event's version, then **`REPLAY-VERSION-DOWNGRADE`** is
          signalled.

        - If the two versions are equal, then **match** is returned.

        - If the new event's version is higher, then **upgrade** is
          returned.

        Where `:INFINITY` is considered higher than any integer and
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
     | downgrade-error | match | upgrade   |


<a id='x-28JOURNAL-3AREPLAY-EVENT-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **replay event**

    The replay event is the next event to be read from [`REPLAY-JOURNAL`][103b]
    which is not to be skipped. There may be no replay event if there
    are no more unread events in the replay journal.
    
    An event in the replay journal is skipped if it is a [`LOG-EVENT`][3a51] or
    there is a [`WITH-REPLAY-FILTER`][fa00] with a matching `:SKIP`. If `:SKIP` is in
    effect, the replay event may be indeterminate.
    
    Events from the replay journal are read when they are `:MATCH`ed or
    `:UPGRADE`d (see [The replay strategy][3c00]), when nested events are
    echoed while [Replaying the outcome][b6d1], or when there is an [Invoked][4492]
    defined with the same name as the replay event.
    
    The replay event is available via [`PEEK-REPLAY-EVENT`][63a5].

<a id='x-28JOURNAL-3A-40MATCHING-IN-EVENTS-20MGL-PAX-3ASECTION-29'></a>

### 9.4 Matching in-events

If the replay strategy is *match*, then, for in-events, the
matching process continues like this:

- If the [`EVENT-ARGS`][5da8] are not `EQUAL`, then **[`REPLAY-ARGS-MISMATCH`][28a4]**
  signalled.

- At this point, two things might happen:

    - For [`VERSIONED-EVENT`][ffc4]s, the [block][6572] will be executed as normal
      and its outcome will be matched to the [replay event][ca07] (see
      [Matching out-events][21d1]).

    - For [`EXTERNAL-EVENT`][eed7]s, the corresponding replay [`OUT-EVENT`][cba8] is
      looked at. If there is one, meaning that the frame finished
      with an [expected outcome][32e0], then its outcome will be
      replayed (see [Replaying the outcome][b6d1]). If the `OUT-EVENT` is
      missing, then `EXTERNAL-EVENT`s behave like `VERSIONED-EVENT`s,
      and the `@BLOCK` is executed.


<a id='x-28JOURNAL-3A-40REPLAYING-THE-OUTCOME-20MGL-PAX-3ASECTION-29'></a>

#### 9.4.1 Replaying the outcome

So, if an in-event is triggered that matches the replay,
`EVENT-VERSION`([`0`][4f5f] [`1`][917c]) is `:INFINITY`, then normal execution is altered in the
following manner:

- The journaled [block][6572] is not executed.

- To keep execution and the replay journal in sync, events of frames
  nested in the current one are skipped over in the replay journal.

- All events (including [`LOG-EVENT`][3a51]s) skipped over are echoed to the
  record journal. This serves to keep a trail of what happened
  during the original recording. Note that functions corresponding
  to [Invoked][4492] frames are called when their [`IN-EVENT`][e10e] is skipped over.

- The out-event corresponding to the in-event being processed is
  then read from the replay journal and is recorded again (to allow
  recording to function properly).

To be able to reproduce the outcome in the replay journal, some
assistance may be required from `REPLAY-VALUES` and `REPLAY-CONDITION`:

- If the [replay event][ca07] has normal return (i.e. `EVENT-EXIT`([`0`][a6d1] [`1`][306a]) `:VALUES`),
  then the recorded return values (in [`EVENT-OUTCOME`][95b8]) are returned
  immediately as in `(VALUES-LIST (EVENT-OUTCOME REPLAY-EVENT))`. If
  `REPLAY-VALUES` is specified, it is called instead of `VALUES-LIST`.
  See [Working with unreadable values][51dd] for an example.

- Similarly, if the replay event has unwound with an expected
  condition (has `EVENT-EXIT` `:CONDITION`), then the recorded
  condition (in `EVENT-OUTCOME`) is signalled as
  IN `(ERROR (EVENT-OUTCOME REPLAY-EVENT))`. If `REPLAY-CONDITION` is
  specified, it is called instead of `ERROR`. `REPLAY-CONDITION` must
  not return normally.

[`WITH-REPLAY-FILTER`][fa00]'s `NO-REPLAY-OUTCOME` can selectively turn off
replaying the outcome. See [Testing on multiple levels][d95f], for an
example.

<a id='x-28JOURNAL-3A-40MATCHING-OUT-EVENTS-20MGL-PAX-3ASECTION-29'></a>

### 9.5 Matching out-events

If there were no [Replay failures][588a] during the matching of the
[`IN-EVENT`][e10e], and the conditions for [Replaying the outcome][b6d1] were not
met, then the [block][6572] is executed. When the outcome of the block is
determined, an [`OUT-EVENT`][cba8] is triggered and is matched to the replay
journal. The matching of out-events starts out as in
[The replay strategy][3c00] with checks for [`EVENT-NAME`][2b45] and
[`EVENT-VERSION`][4f5f].

If the replay strategy is *insert* or *upgrade*, then the out-event
is written to [`RECORD-JOURNAL`][35c4], consuming an event with a matching
name from the [`REPLAY-JOURNAL`][103b] in the latter case. If the strategy is
*match*, then:

- If the new event has an [unexpected outcome][f57e], then
  **[`REPLAY-UNEXPECTED-OUTCOME`][8d93]** is signalled. Note that the replay
  event always has an [expected outcome][32e0] due to the handling of
  [`RECORD-UNEXPECTED-OUTCOME`][cf72].

- If the new event has an `EXPECTED-OUTCOME`, then unless the new and
  [replay event][ca07]'s `EVENT-EXIT`([`0`][a6d1] [`1`][306a])s are `EQ` and their [`EVENT-OUTCOME`][95b8]s are
  `EQUAL`, **[`REPLAY-OUTCOME-MISMATCH`][55b7]** is signalled.

- Else, the replay event is consumed and the new event is written
  the `RECORD-JOURNAL`.

Note that [The replay strategy][3c00] for the in-event and the out-event of
the same [frame][1452] may differ if the corresponding out-event is not
present in `REPLAY-JOURNAL`, which may be the case when the recording
process failed hard without unwinding properly, or when an
`UNEXPECTED-OUTCOME` triggered the transition to [`JOURNAL-STATE`][3631]
`:LOGGING`.

<a id='x-28JOURNAL-3A-40REPLAY-FAILURES-20MGL-PAX-3ASECTION-29'></a>

### 9.6 Replay failures

<a id='x-28JOURNAL-3AREPLAY-FAILURE-20CONDITION-29'></a>

- [condition] **REPLAY-FAILURE** *SERIOUS-CONDITION*

    A common superclass (never signalled itself) for
    all kinds of mismatches between the events produced and the replay
    journal. Signalled only in [`JOURNAL-STATE`][3631] `:REPLAYING` and only once
    per [`WITH-JOURNALING`][234e]. If a `REPLAY-FAILURE` is signalled for an [`EVENT`][0f76],
    then the event will be recorded, but [`RECORD-JOURNAL`][35c4] will transition
    to `JOURNAL-STATE` `:MISMATCHED`. Like [`JOURNALING-FAILURE`][6db0], this is a
    serious condition because it is to be handled outside the enclosing
    `WITH-JOURNALING`. If a `REPLAY-FAILURE` were to be handled inside the
    `WITH-JOURNALING`, keep in mind that in `:MISMATCHED`, replay always
    uses the *insert* replay strategy (see [The replay strategy][3c00]).

<a id='x-28JOURNAL-3AREPLAY-FAILURE-NEW-EVENT-20-28MGL-PAX-3AREADER-20JOURNAL-3AREPLAY-FAILURE-29-29'></a>

- [reader] **REPLAY-FAILURE-NEW-EVENT** *REPLAY-FAILURE* *(:NEW-EVENT)*

<a id='x-28JOURNAL-3AREPLAY-FAILURE-REPLAY-EVENT-20-28MGL-PAX-3AREADER-20JOURNAL-3AREPLAY-FAILURE-29-29'></a>

- [reader] **REPLAY-FAILURE-REPLAY-EVENT** *REPLAY-FAILURE* *(:REPLAY-EVENT)*

<a id='x-28JOURNAL-3AREPLAY-FAILURE-REPLAY-JOURNAL-20-28MGL-PAX-3AREADER-20JOURNAL-3AREPLAY-FAILURE-29-29'></a>

- [reader] **REPLAY-FAILURE-REPLAY-JOURNAL** *REPLAY-FAILURE* *(= '(REPLAY-JOURNAL))*

<a id='x-28JOURNAL-3AREPLAY-NAME-MISMATCH-20CONDITION-29'></a>

- [condition] **REPLAY-NAME-MISMATCH** *REPLAY-FAILURE*

    Signaled when the new event's and [replay event][ca07]'s
    [`EVENT-NAME`][2b45] are not `EQUAL`. The [`REPLAY-FORCE-INSERT`][524c],
    [`REPLAY-FORCE-UPGRADE`][5ef1] restarts are provided.

<a id='x-28JOURNAL-3AREPLAY-VERSION-DOWNGRADE-20CONDITION-29'></a>

- [condition] **REPLAY-VERSION-DOWNGRADE** *REPLAY-FAILURE*

    Signaled when the new event and the [replay event][ca07]
    have the same [`EVENT-NAME`][2b45], but the new event has a lower version. The
    [`REPLAY-FORCE-UPGRADE`][5ef1] restart is provided.

<a id='x-28JOURNAL-3AREPLAY-ARGS-MISMATCH-20CONDITION-29'></a>

- [condition] **REPLAY-ARGS-MISMATCH** *REPLAY-FAILURE*

    Signaled when the new event's and [replay event][ca07]'s
    [`EVENT-ARGS`][5da8] are not `EQUAL`. The [`REPLAY-FORCE-UPGRADE`][5ef1] restart is
    provided.

<a id='x-28JOURNAL-3AREPLAY-OUTCOME-MISMATCH-20CONDITION-29'></a>

- [condition] **REPLAY-OUTCOME-MISMATCH** *REPLAY-FAILURE*

    Signaled when the new event's and [replay event][ca07]'s
    `EVENT-EXIT`([`0`][a6d1] [`1`][306a]) and/or [`EVENT-OUTCOME`][95b8] are not `EQUAL`. The
    [`REPLAY-FORCE-UPGRADE`][5ef1] restart is provided.

<a id='x-28JOURNAL-3AREPLAY-UNEXPECTED-OUTCOME-20CONDITION-29'></a>

- [condition] **REPLAY-UNEXPECTED-OUTCOME** *REPLAY-FAILURE*

    Signaled when the new event has an
    [unexpected outcome][f57e]. Note that the [replay event][ca07] always has an
    [expected outcome][32e0] due to the logic of [`RECORD-UNEXPECTED-OUTCOME`][cf72]. No
    restarts are provided.

<a id='x-28JOURNAL-3AREPLAY-INCOMPLETE-20CONDITION-29'></a>

- [condition] **REPLAY-INCOMPLETE** *REPLAY-FAILURE*

    Signaled if there are unprocessed non-log events in
    [`REPLAY-JOURNAL`][103b] when [`WITH-JOURNALING`][234e] finishes and the body of
    `WITH-JOURNALING` returned normally, which is to prevent this
    condition to cancel an ongoing unwinding. No restarts are
    provided.

<a id='x-28JOURNAL-3AREPLAY-FORCE-INSERT-20RESTART-29'></a>

- [restart] **REPLAY-FORCE-INSERT**

    This restart forces [The replay strategy][3c00] to be `:INSERT`, overriding
    [`REPLAY-NAME-MISMATCH`][d031]. This is intended for upgrades, and extreme
    care must be taken not to lose data.

<a id='x-28JOURNAL-3AREPLAY-FORCE-UPGRADE-20RESTART-29'></a>

- [restart] **REPLAY-FORCE-UPGRADE**

    This restart forces [The replay strategy][3c00] to be `:UPGRADE`, overriding
    [`REPLAY-NAME-MISMATCH`][d031], [`REPLAY-VERSION-DOWNGRADE`][2efb],
    [`REPLAY-ARGS-MISMATCH`][28a4], [`REPLAY-OUTCOME-MISMATCH`][55b7]. This is intended for
    upgrades, and extreme care must be taken not to lose data.

<a id='x-28JOURNAL-3A-40UPGRADES-AND-REPLAY-20MGL-PAX-3ASECTION-29'></a>

### 9.7 Upgrades and replay

The replay mechanism is built on the assumption that the tree of
[frame][1452]s is the same when the code is replayed as it was when the
replay journal was originally recorded. Thus, non-deterministic
control flow poses a challenge, but non-determinism can be isolated
with [`EXTERNAL-EVENT`][eed7]s. However, when the code changes, we might find
the structure of frames in previous recordings hard to accommodate.
In this case, we might decide to alter the structure, giving up some
of the safety provided by the replay mechanism. There are various
tools at our disposal to control this tradeoff between safety and
flexibility:

- We can insert individual frames with [`JOURNALED`][4f52]'s `INSERTABLE`,
  upgrade frames by bumping `JOURNALED`'s `VERSION`, and filter frames
  with [`WITH-REPLAY-FILTER`][fa00]. This option allows for the most
  consistency checks.

- The [`REPLAY-FORCE-UPGRADE`][5ef1] and [`REPLAY-FORCE-INSERT`][524c] restarts allow
  overriding [The replay strategy][3c00], but their use requires great care
  to be taken.

- Or we may decide to keep the bare minimum of the replay journal
  around, and discard everything except for `EXTERNAL-EVENT`s. This
  option is equivalent to

        (let ((*force-insertable* t))
          (with-replay-filter (:skip '((:name nil)))
            42))

- Rerecording the journal without replay might be another option if
  there are no `EXTERNAL-EVENT`s to worry about.

- Finally, we can rewrite the replay journal using the low-level
  interface (see [Streamlets reference][2453]). In this case, extreme care
  must be taken not to corrupt the journal (and lose data) as there
  are no consistency checks to save us.

With that, let's see how `WITH-REPLAY-FILTER` works.

<a id='x-28JOURNAL-3AWITH-REPLAY-STREAMLET-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-REPLAY-STREAMLET** *(VAR) &BODY BODY*

    Open [`REPLAY-JOURNAL`][103b] for reading with [`WITH-OPEN-JOURNAL`][8383] set the
    [`READ-POSITION`][1548] on it to the event next read by the [Replay][0dc7]
    mechanism (which is never a [`LOG-EVENT`][3a51]). The low-level
    [Reading from streamlets][e099] api is then available to inspect the
    contents of the replay. It is an error if `REPLAY-JOURNAL` is `NIL`.

<a id='x-28JOURNAL-3APEEK-REPLAY-EVENT-20FUNCTION-29'></a>

- [function] **PEEK-REPLAY-EVENT** 

    Return the [replay event][ca07] to be read from [`REPLAY-JOURNAL`][103b]. This is
    roughly equivalent to
    
    ```
    (when (replay-journal)
      (with-replay-streamlet (streamlet)
        (peek-event streamlet))
    ```
    
    except `PEEK-REPLAY-EVENT` takes into account [`WITH-REPLAY-FILTER`][fa00]
    `:MAP`, and it may return `(:INDETERMINATE)` if `WITH-REPLAY-FILTER`
    `:SKIP` is in effect and what events are to be skipped cannot be
    decided until the next in-event generated by the code.
    
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
    [`EXTERNAL-EVENT`][eed7]s is tricky.
    
    We have to first decide how to handle the lack of approval in the
    first version. Here, we just assume the processes started by the
    first version get approval automatically. The implementation is
    based on a dummy `PROCESS` block whose version is bumped when the
    payment process changes and is inspected at the start of journaling.
    
    When v1 is replayed with v2, we introduce an `INSERTABLE`, versioned
    `GET-APPROVAL` block that just returns `T`. When replaying the code
    again, still with v2, the `GET-APPROVAL` block will be upgraded to
    `:INFINITY`.
    
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
    ```


<a id='x-28JOURNAL-3AWITH-REPLAY-FILTER-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-REPLAY-FILTER** *(&KEY MAP SKIP NO-REPLAY-OUTCOME) &BODY BODY*

    `WITH-REPLAY-FILTER` performs journal upgrade during replay by
    allowing events to be transformed as they are read from the replay
    journal or skipped if they match some patterns. For how to add new
    blocks in a code upgrade, see [`JOURNALED`][4f52]'s `:INSERTABLE` argument. In
    addition, it also allows some control over [Replaying the outcome][b6d1].
    
    - `MAP`: A function called with an event read from the replay journal
      which returns a transformed event. See [Events reference][d9ae]. `MAP`
      takes effect before before `SKIP`.
    
    - `SKIP`: In addition to filtering out [`LOG-EVENT`][3a51]s (which always
      happens during replay), filter out all events that belong to
      descendant frames that match any of its `SKIP` patterns. Filtered
      out events are never seen by `JOURNALED` as it replays events. `SKIP`
      patterns are of the format `(&KEY NAME VERSION<)`, where `VERSION`<
      is a valid [`EVENT-VERSION`][917c], and `NAME` may be `NIL`, which acts
      as a wildcard.
    
        `SKIP` is for when `JOURNALED` [block][6572]s are removed from the code,
        which would render replaying previously recorded journals
        impossible. Note that, for reasons of safety, it is not possible
        to filter [`EXTERNAL-EVENT`][eed7]s.
    
    - `NO-REPLAY-OUTCOME` is a list of [`EVENT-NAME`][2b45]s. [Replaying the outcome][b6d1]
      is prevented for frames with `EQUAL` names. See
      [Testing on multiple levels][d95f] for an example.
    
    `WITH-REPLAY-FILTER` affects only the immediately enclosing
    [`WITH-JOURNALING`][234e]. A `WITH-REPLAY-FILTER` nested within another in the
    same `WITH-JOURNALING` inherits the `SKIP` patterns of its parent, to
    which it adds its own. The `MAP` function is applied to before the
    parent's `MAP`.
    
    Examples of `SKIP` patterns:
    
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
    
    Skipping can be thought of as removing nodes of the tree of frames,
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
        (with-replay-filter (:skip '((:name j1) (:name j2)))
          (checked (j3)
            42))))
    ```


<a id='x-28JOURNAL-3A-40TESTING-20MGL-PAX-3ASECTION-29'></a>

## 10 Testing

Having discussed the [Replay][0dc7] mechanism, next are [Testing][c28b] and
[Persistence][98d3], which rely heavily on replay. Suppose we want to unit
test user registration. Unfortunately, the code communicates with a
database service and also takes input from the user. A natural
solution is to create [mocks][mock-object] for these external
systems to unshackle the test from the cumbersome database
dependency and to allow it to run without user interaction.

We do this below by wrapping external interaction in [`JOURNALED`][4f52] with
`:VERSION` `:INFINITY` (see [Replaying the outcome][b6d1]).

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
of the external `JOURNALED` blocks are replayed from the journal:

```
;; Replay: all external interactions are mocked.
JRN> (test-user-registration)
=> NIL
```

Should the code change, we might want to upgrade carefully (see
[Upgrades and replay][1acb]) or just rerecord from scratch:

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

Note that when this journal is replayed, new [`VERSIONED-EVENT`][ffc4]s are
required to match the replay. So after the original recording, we
can check by eyeballing that the record represents a correct
execution. Then on subsequent replays, even though
`MAYBE-WIN-THE-GRAND-PRIZE` sits behind `REGISTER-USER` and is hard
to test with `ASSERT`s, the replay mechanism verifies that it is
called only for new users.

This record-and-replay style of testing is not the only possibility:
direct inspection of a journal with the low-level events api (see
[Events reference][d9ae]) can facilitate checking non-local invariants.

<a id='x-28JOURNAL-3ADEFINE-FILE-BUNDLE-TEST-20MGL-PAX-3AMACRO-29'></a>

- [macro] **DEFINE-FILE-BUNDLE-TEST** *(NAME &KEY DIRECTORY (EQUIVALENTP T)) &BODY BODY*

    Define a function with `NAME` for record-and-replay testing. The
    function's `BODY` is executed in a [`WITH-BUNDLE`][0ddc] to guarantee
    replayability. The bundle in question is a [`FILE-BUNDLE`][eb5d] created in
    `DIRECTORY`. The function has a single keyword argument, `RERECORD`. If
    `RERECORD` is true, the bundle is deleted with [`DELETE-FILE-BUNDLE`][01aa] to
    start afresh.
    
    Furthermore, if `BODY` returns normally, and it is a replay of a
    previous run, and `EQUIVALENTP`, then it is ASSERTed that the record
    and replay journals are [`EQUIVALENT-REPLAY-JOURNALS-P`][75ef]. If this check
    fails, [`RECORD-JOURNAL`][35c4] is discarded when the function returns. In
    addition to the replay consistency, this checks that no inserts or
    upgrades were performed (see [The replay strategy][3c00]).

<a id='x-28JOURNAL-3A-40TESTING-ON-MULTIPLE-LEVELS-20MGL-PAX-3ASECTION-29'></a>

### 10.1 Testing on multiple levels

Nesting [`REPLAYED`][c2fb]s (that is, [frame][1452]s of [`EXTERNAL-EVENT`][eed7]s) is not
obviously useful since the outer `REPLAYED` will be replayed by
outcome, and the inner one will be just echoed to the record
journal. However, if we turn off [Replaying the outcome][b6d1] for the
outer, the inner will be replayed.

This is useful for testing layered communication. For example, we
might have written code that takes input from an external
system (`READ-LINE`) and does some complicated
processing (`READ-FROM-STRING`) before returning the input in a form
suitable for further processing. Suppose we wrap `REPLAYED` around
`READ-FROM-STRING` for [Persistence][98d3] because putting it around
`READ-LINE` would expose low-level protocol details in the journal,
making protocol changes difficult.

However, upon realizing that `READ-FROM-STRING` was not the best tool
for the job and switching to `PARSE-INTEGER`, we want to test by
replaying all previously recorded journals. For this, we prevent the
outer `REPLAYED` from being replayed by outcome with
[`WITH-REPLAY-FILTER`][fa00]:

```
(let ((bundle (make-in-memory-bundle)))
  ;; Original with READ-FROM-STRING
  (with-bundle (bundle)
    (replayed ("accept-number")
      (values (read-from-string (replayed ("input-number")
                                  (read-line))))))
  ;; Switch to PARSE-INTEGER and test by replay.
  (with-bundle (bundle)
    (with-replay-filter (:no-replay-outcome '("accept-number"))
      (replayed ("accept-number")
        ;; 1+ is our bug.
        (values (1+ (parse-integer (replayed ("input-number")
                                     (read-line)))))))))
```

The inner `input-number` block is replayed by outcome, and
`PARSE-INTEGER` is called with the string `READ-LINE` returned in the
original invocation. The outcome of the outer `accept-number` block
checked as if it was a [`VERSIONED-EVENT`][ffc4] and we get a
[`REPLAY-OUTCOME-MISMATCH`][55b7] due to the bug.

<a id='x-28JOURNAL-3A-40PERSISTENCE-20MGL-PAX-3ASECTION-29'></a>

## 11 Persistence

<a id='x-28JOURNAL-3A-40PERSISTENCE-TUTORIAL-20MGL-PAX-3ASECTION-29'></a>

### 11.1 Persistence tutorial

Let's write a simple game.

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
department. In the transcript below, `PARSE-INTEGER` fails with `junk
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
beginning, [Replaying the outcome][b6d1] of external interactions marked
with [`REPLAYED`][c2fb]:

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
[`IN-MEMORY-BUNDLE`][2314], we used a [`FILE-BUNDLE`][eb5d], the game would have been
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
in `REPLAYED`, which allows it to reconstruct the state of the program
from the recorded events at any point during its execution and
resume from there.

<a id='x-28JOURNAL-3A-40SYNCHRONIZATION-20MGL-PAX-3ASECTION-29'></a>

### 11.2 Synchronization to storage

In the following, we explore how journals can serve as a
persistence mechanism and the guarantees they offer. The high-level
summary is that journals with `SYNC` can serve as a durable and
consistent storage medium. The other two
[ACID](https://en.wikipedia.org/wiki/ACID) properties, atomicity and
isolation, do not apply because Journal is single-client and does
not need transactions.

<a id='x-28JOURNAL-3AABORTED-EXECUTION-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **aborted execution**

    Aborted execution is when the operating system or the application
    crashes, calls `abort()`, is killed by a `SIGKILL` signal or there
    is a power outage. Synchronization guarantees are defined in face of
    aborted execution and do not apply to hardware errors, Lisp or OS
    bugs.

<a id='x-28JOURNAL-3ADATA-EVENT-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **data event**

    Data events are the only events that may be non-deterministic. They
    record information which could change if the same code were run
    multiple times. Data events typically correspond to interactions
    with the user, servers or even the random number generator. Due to
    their non-determinism, they are the only parts of the journal not
    reproducible by rerunning the code. In this sense, only data events
    are not redundant with the code and whether other events are
    persisted does not affect durability. There are two kinds of data
    events:
    
    - An [`EXTERNAL-EVENT`][eed7] that is also an [`OUT-EVENT`][cba8].
    
    - The [`IN-EVENT`][e10e] of an [Invoked][4492] function, which lies outside the
      normal, deterministic control flow.


<a id='x-28JOURNAL-3A-40SYNCHRONIZATION-STRATEGIES-20MGL-PAX-3ASECTION-29'></a>

#### 11.2.1 Synchronization strategies

When a journal or bundle is created (see [`MAKE-IN-MEMORY-JOURNAL`][0605],
[`MAKE-FILE-JOURNAL`][182e], [`MAKE-IN-MEMORY-BUNDLE`][3f41], [`MAKE-FILE-BUNDLE`][7bd7]), the
`SYNC` option determines when - as a [`RECORD-JOURNAL`][35c4] - the recorded
events and [`JOURNAL-STATE`][3631] changes are persisted durably. For
[`FILE-JOURNAL`][f6b2]s, persisting means calling something like `fsync()`,
while for [`IN-MEMORY-JOURNAL`][17a8]s, a user defined function is called to
persist the data.

- `NIL`: Never synchronize. A `FILE-JOURNAL`'s file may be corrupted on
  [aborted execution][33c1]. In `IN-MEMORY-JOURNAL`s, `SYNC-FN` is never called.

- `T`: This is the *no data loss* setting with minimal
  synchronization. It guarantees *consistency* (i.e. no corruption)
  and *durability* up to the most recent [data event][ecce] written in
  `JOURNAL-STATE` `:RECORDING` or for the entire record journal in
  states `:FAILED` and `:COMPLETED`. `:FAILED` or `:COMPLETED` is guaranteed
  when leaving [`WITH-JOURNALING`][234e] at the latest.

- Values other than `NIL` and `T` are reserved for future extensions.
  Using them triggers a [`JOURNAL-ERROR`][571f].


<a id='x-28JOURNAL-3A-40SYNCHRONIZATION-WITH-IN-MEMORY-JOURNALS-20MGL-PAX-3ASECTION-29'></a>

#### 11.2.2 Synchronization with in-memory journals

Unlike [`FILE-JOURNAL`][f6b2]s, [`IN-MEMORY-JOURNAL`][17a8]s do not have any built-in
persistent storage backing them, but with `SYNC-FN`, persistence can
be tacked on. If non-NIL, `SYNC-FN` must be a function of a single
argument, an `IN-MEMORY-JOURNAL`. `SYNC-FN` is called according to
[Synchronization strategies][355b], and upon normal return the journal must
be stored durably.

The following example saves the entire journal history when a new
[data event][ecce] is recorded. Note how `SYNC-TO-DB` is careful to
overwrite `*DB*` only if it is called with a journal that has not
failed the replay (as in [Replay failures][588a]) and is sufficiently
different from the replay journal as determined by
[`JOURNAL-DIVERGENT-P`][f748].

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
journal *for replay* is created and initialized with the saved
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
case returning 2. See [Replaying the outcome][b6d1]. Block `B`, on the
other hand, was rerun because it had an [unexpected outcome][f57e] the first
time around. This time it ran without error, a `DATA-EVENT` was
triggered and `SYNC-FN` invoked.

If we were to invoke the now completed `RUN-WITH-DB` again, it would
simply return 3 without ever invoking `SYNC-FN`:

```
(run-with-db)
=> 3
```

With [`JOURNAL-REPLAY-MISMATCH`][9444], `SYNC-FN` can be optimized to to reuse
the sequence of events in the replay journal up until the point of
divergence.

<a id='x-28JOURNAL-3A-40SYNCHRONIZATION-WITH-FILE-JOURNALS-20MGL-PAX-3ASECTION-29'></a>

#### 11.2.3 Synchronization with file journals

For [`FILE-JOURNAL`][f6b2]s, `SYNC` determines when the events written to the
[`RECORD-JOURNAL`][35c4] and its [`JOURNAL-STATE`][3631] will be persisted durably in
the file. Syncing to the file involves two calls to `fsync()`, and
is not cheap.

Syncing events to files is implemented as follows.

- When the journal file is created, its parent directory is
  immediately fsynced to make sure that the file will not be lost on
  [aborted execution][33c1].

- When an event is about to be written the first time after file
  creation or after a sync, a transaction start marker is written to
  the file.

- Any number of events may be subsequently written until syncing is
  deemed necessary (see [Synchronization strategies][355b]).

- At this point, `fsync()` is called to flush all event data and
  state changes to the file, and the transaction start marker is
  *overwritten* with a transaction completed marker and another
  `fsync()` is performed.

- When reading back this file (e.g. for replay), an open transaction
  marker is treated as the end of file.

Note that this implementation assumes that after writing the start
transaction marker a crash cannot leave any kind of garbage bytes
around: it must leave zeros. This is not true for all filesytems.
For example, ext3/ext4 with `data=writeback` [can leave garbage
around][ext4-writeback].

[ext4-writeback]: https://ext4.wiki.kernel.org/index.php/Ext3_Data=Ordered_vs_Data=Writeback_mode 


<a id='x-28JOURNAL-3A-40SAFETY-20MGL-PAX-3ASECTION-29'></a>

## 12 Safety

##### Thread safety

Changes to journals come in two varieties: adding an event and
changing the [`JOURNAL-STATE`][3631]. Both are performed by [`JOURNALED`][4f52] only
unless the low-level streamlet interface is used (see
[Streamlets reference][2453]). Using `JOURNALED` wrapped in a
[`WITH-JOURNALING`][234e], [`WITH-BUNDLE`][0ddc], or [Log record][3380] without `WITH-JOURNALING`
is thread-safe.

- Every journal is guaranteed to have at most a single writer active
  at any time. Writers are `WITH-JOURNALING` and `WITH-BUNDLE`, but also
  any journals used as `LOG-RECORD` (unless that journal is the
  [`RECORD-JOURNAL`][35c4]) have a log writer stored in the journal object.

- `WITH-JOURNALING` and `WITH-BUNDLE` have dynamic extent as writers,
  but log writers of journals have indefinite extent: once a journal
  is used as a `LOG-RECORD` there remains a writer.

- Attempting to create a second writer triggers a [`JOURNAL-ERROR`][571f].

- Writing to the same journal via `LOG-RECORD` from multiple threads
  concurrently is possible since this doesn't create multiple
  writers. It is ensured with locking that events are written
  atomically. Frames can be interleaved, but these are [`LOG-EVENT`][3a51]s,
  so this does not affect replay.

- The juggling of replay and record journals performed by
  `WITH-BUNDLE` is also thread-safe.

- It is ensured that there is at most one [`FILE-JOURNAL`][f6b2] object in the
  same Lisp image is backed by the same file.

- Similarly, there is at most [`FILE-BUNDLE`][eb5d] object for a directory.

##### Process safety

Currently, there is no protection against multiple OS processes
writing the same `FILE-JOURNAL` or `FILE-BUNDLE`.

##### Signal safety

Journal is *designed* to be [async-unwind][ba76] safe, but *not reentrant*.
Interrupts are disabled only for the most critical cleanup forms. If
a thread is killed without unwinding, that constitutes
[aborted execution][33c1], so guarantees about [Synchronization to storage][a074] apply, but
[`JOURNAL`][86bc] objects written by the thread are not safe to access, and
the Lisp should probably be restarted.

<a id='x-28JOURNAL-3A-40EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29'></a>

## 13 Events reference

Events are normally triggered upon entering and leaving the
dynamic extent of a [`JOURNALED`][4f52] [block][6572] (see [In-events][1d28] and
[Out-events][5721]) and also by [`LOGGED`][0765]. Apart from being part of the
low-level substrate of the Journal library, working with events
directly is sometimes useful when writing tests that inspect
recorded events. Otherwise, skip this entire section.

All `EVENTs` have [`EVENT-NAME`][2b45] and `EVENT-VERSION`([`0`][4f5f] [`1`][917c]), which feature
prominently in [The replay strategy][3c00]. After the examples in
[In-events][1d28] and [Out-events][5721], the following example is a reminder of
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

So a `JOURNALED` `@BLOCK` generates an [`IN-EVENT`][e10e] and an [`OUT-EVENT`][cba8], which
are simple property lists. The following reference lists these
properties, there semantics and the functions to read them.

<a id='x-28JOURNAL-3AEVENT-20TYPE-29'></a>

- [type] **EVENT**

    An event is either an [`IN-EVENT`][e10e], an [`OUT-EVENT`][cba8] or a [`LEAF-EVENT`][fef8].

<a id='x-28JOURNAL-3AEVENT-3D-20FUNCTION-29'></a>

- [function] **EVENT=** *EVENT-1 EVENT-2*

    Return whether `EVENT-1` and `EVENT-2` represent the same event. In-
    and out-events belonging to the same [frame][1452] are *not* the same
    event. [`EVENT-OUTCOME`][95b8] is not compared when `EVENT-EXIT`([`0`][a6d1] [`1`][306a]) is `:ERROR` to
    avoid undue dependence on implementation specific string
    representations. This function is useful in conjunction with
    [`MAKE-IN-EVENT`][4c38] and [`MAKE-OUT-EVENT`][5ca9] to write tests.

<a id='x-28JOURNAL-3AEVENT-NAME-20FUNCTION-29'></a>

- [function] **EVENT-NAME** *EVENT*

    The name of an event can be of any type. It is often a symbol or a
    string. When replaying, names may be compared with `EQUAL`. All `EVENTs`
    have names. The names of the in- and out-events belonging to the
    same [frame][1452] are the same.

<a id='x-28JOURNAL-3A-40EVENT-VERSIONS-20MGL-PAX-3ASECTION-29'></a>

### 13.1 Event versions

<a id='x-28JOURNAL-3AEVENT-VERSION-20FUNCTION-29'></a>

- [function] **EVENT-VERSION** *EVENT*

    Return the version of `EVENT` of type [`EVENT-VERSION`][917c].

<a id='x-28JOURNAL-3ALOG-EVENT-P-20FUNCTION-29'></a>

- [function] **LOG-EVENT-P** *EVENT*

    See if `EVENT` is a [`LOG-EVENT`][3a51].

<a id='x-28JOURNAL-3AVERSIONED-EVENT-P-20FUNCTION-29'></a>

- [function] **VERSIONED-EVENT-P** *EVENT*

    See if `EVENT` is a [`VERSIONED-EVENT`][ffc4].

<a id='x-28JOURNAL-3AEXTERNAL-EVENT-P-20FUNCTION-29'></a>

- [function] **EXTERNAL-EVENT-P** *EVENT*

    See if `EVENT` is an [`EXTERNAL-EVENT`][eed7].

<a id='x-28JOURNAL-3A-40IN-EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29'></a>

### 13.2 In-events

<a id='x-28JOURNAL-3AIN-EVENT-20TYPE-29'></a>

- [type] **IN-EVENT**

    `IN-EVENT`s are triggered upon entering the dynamic extent of a
    [`JOURNALED`][4f52] [block][6572]. `IN-EVENT`s have [`EVENT-NAME`][2b45],
    [`EVENT-VERSION`][4f5f], and [`EVENT-ARGS`][5da8]. See [In-events][1d28] for a more
    introductory treatment.

<a id='x-28JOURNAL-3AIN-EVENT-P-20FUNCTION-29'></a>

- [function] **IN-EVENT-P** *EVENT*

    See if `EVENT` is a [`IN-EVENT`][e10e].

<a id='x-28JOURNAL-3AMAKE-IN-EVENT-20FUNCTION-29'></a>

- [function] **MAKE-IN-EVENT** *&KEY NAME VERSION ARGS*

    Create an [`IN-EVENT`][e10e] with `NAME`, `VERSION` (of type [`EVENT-VERSION`][917c]) and
    `ARGS` as its [`EVENT-NAME`][2b45], [`EVENT-VERSION`][4f5f] and [`EVENT-ARGS`][5da8].

<a id='x-28JOURNAL-3AEVENT-ARGS-20FUNCTION-29'></a>

- [function] **EVENT-ARGS** *IN-EVENT*

    Return the arguments of `IN-EVENT`, normally populated with the `ARGS`
    form in [`JOURNALED`][4f52].

<a id='x-28JOURNAL-3A-40OUT-EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29'></a>

### 13.3 Out-events

<a id='x-28JOURNAL-3AOUT-EVENT-20TYPE-29'></a>

- [type] **OUT-EVENT**

    `OUT-EVENT`s are triggered upon leaving the dynamic extent of the
    [`JOURNALED`][4f52] [block][6572]. `OUT-EVENT`s have [`EVENT-NAME`][2b45],
    [`EVENT-VERSION`][4f5f], [`EVENT-EXIT`][a6d1] and [`EVENT-OUTCOME`][95b8].
    See [Out-events][5721] for a more introductory treatment.

<a id='x-28JOURNAL-3AOUT-EVENT-P-20FUNCTION-29'></a>

- [function] **OUT-EVENT-P** *EVENT*

    See if `EVENT` is an [`OUT-EVENT`][cba8].

<a id='x-28JOURNAL-3AMAKE-OUT-EVENT-20FUNCTION-29'></a>

- [function] **MAKE-OUT-EVENT** *&KEY NAME VERSION EXIT OUTCOME*

    Create an [`OUT-EVENT`][cba8] with `NAME`, `VERSION` (of type [`EVENT-VERSION`][917c]),
    `EXIT` (of type [`EVENT-EXIT`][306a]), and `OUTCOME` as its [`EVENT-NAME`][2b45],
    [`EVENT-VERSION`][4f5f], [`EVENT-EXIT`][a6d1] and
    [`EVENT-OUTCOME`][95b8].

<a id='x-28JOURNAL-3AEVENT-EXIT-20FUNCTION-29'></a>

- [function] **EVENT-EXIT** *OUT-EVENT*

    Return how the journaled [block][6572] finished. See [`EVENT-EXIT`][306a]
    for the possible types.

<a id='x-28JOURNAL-3AEXPECTED-OUTCOME-P-20FUNCTION-29'></a>

- [function] **EXPECTED-OUTCOME-P** *OUT-EVENT*

    See if `OUT-EVENT` has an [expected outcome][32e0].

<a id='x-28JOURNAL-3AUNEXPECTED-OUTCOME-P-20FUNCTION-29'></a>

- [function] **UNEXPECTED-OUTCOME-P** *OUT-EVENT*

    See if `OUT-EVENT` has an [unexpected outcome][f57e].

<a id='x-28JOURNAL-3AEVENT-OUTCOME-20FUNCTION-29'></a>

- [function] **EVENT-OUTCOME** *OUT-EVENT*

    Return the outcome of the [frame][1452] (or loosely speaking of a [block][6572])
    to which `OUT-EVENT` belongs. 

<a id='x-28JOURNAL-3A-40LEAF-EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29'></a>

### 13.4 Leaf-events

<a id='x-28JOURNAL-3ALEAF-EVENT-20TYPE-29'></a>

- [type] **LEAF-EVENT**

    Leaf events are triggered by [`LOGGED`][0765]. Unlike [`IN-EVENT`][e10e]s and
    [`OUT-EVENT`][cba8]s, which represent a [frame][1452], leaf events stand alone and
    thus cannot have children. They are also the poorest of their kind:
    they only have an [`EVENT-NAME`][2b45]. Their `VERSION` is always `NIL`, which
    makes them [`LOG-EVENT`][3a51]s.

<a id='x-28JOURNAL-3ALEAF-EVENT-P-20FUNCTION-29'></a>

- [function] **LEAF-EVENT-P** *EVENT*

    See if `EVENT` is a [`LEAF-EVENT`][fef8].

<a id='x-28JOURNAL-3AMAKE-LEAF-EVENT-20FUNCTION-29'></a>

- [function] **MAKE-LEAF-EVENT** *NAME*

    Create a [`LEAF-EVENT`][fef8] with `NAME`.

<a id='x-28JOURNAL-3A-40JOURNALS-REFERENCE-20MGL-PAX-3ASECTION-29'></a>

## 14 Journals reference

In [Basics][7c19], we covered the bare minimum needed to work with
journals. Here we go into the details.

<a id='x-28JOURNAL-3AJOURNAL-20CLASS-29'></a>

- [class] **JOURNAL**

    A journal is, conceptually, a sequence of events.
    `JOURNAL` is an abstract base class. In case of [`FILE-JOURNAL`][f6b2]s, the
    events are stored in a file, while for [`IN-MEMORY-JOURNAL`][17a8]s, they are
    in a Lisp array. When a journal is opened, it is possible to perform
    I/O on it (see [Streamlets reference][2453]), which is normally taken care
    of by [`WITH-JOURNALING`][234e]. For this reason, the user's involvement with
    journals normally only consists of creating and using them in
    `WITH-JOURNALING`.

<a id='x-28JOURNAL-3AJOURNAL-STATE-20-28MGL-PAX-3AREADER-20JOURNAL-3AJOURNAL-29-29'></a>

- [reader] **JOURNAL-STATE** *JOURNAL* *(:STATE)*

    Return the state of [`JOURNAL`][86bc], which is of type
    [`JOURNAL-STATE`][3631].

<a id='x-28JOURNAL-3AJOURNAL-SYNC-20-28MGL-PAX-3AREADER-20JOURNAL-3AJOURNAL-29-29'></a>

- [reader] **JOURNAL-SYNC** *JOURNAL* *(:SYNC = NIL)*

    The `SYNC` argument specified at instantiation. See
    [Synchronization strategies][355b].

<a id='x-28JOURNAL-3ASYNC-JOURNAL-20FUNCTION-29'></a>

- [function] **SYNC-JOURNAL** *&OPTIONAL (JOURNAL (RECORD-JOURNAL))*

    Durably persist all preceding writes made to `JOURNAL` during an
    enclosing [`WITH-JOURNALING`][234e] or via `LOG-RECORD` from any thread. Writes
    made in a `WITH-JOURNALING` in another thread are not persisted. The
    changes in question are [`WRITE-EVENT`][0448] calls and state changes. This is
    a noop [`JOURNAL-SYNC`][0d06] is `NIL`. This function is safe to call from any
    thread.

<a id='x-28JOURNAL-3AJOURNAL-REPLAY-MISMATCH-20-28MGL-PAX-3AREADER-20JOURNAL-3AJOURNAL-29-29'></a>

- [reader] **JOURNAL-REPLAY-MISMATCH** *JOURNAL* *(= NIL)*

    If [`JOURNAL-DIVERGENT-P`][f748], then this is a list of two
    elements: the [`READ-POSITION`][1548]s in the [`RECORD-JOURNAL`][35c4] and
    [`REPLAY-JOURNAL`][103b] of the first events that were different (ignoring
    [`LOG-EVENT`][3a51]s). It is `NIL`, otherwise.

<a id='x-28JOURNAL-3AJOURNAL-DIVERGENT-P-20FUNCTION-29'></a>

- [function] **JOURNAL-DIVERGENT-P** *JOURNAL*

    See if [`WITH-JOURNALING`][234e] recorded any event so far in this journal
    which was not `EQUAL` to its [replay event][ca07] or it had no corresponding
    replay event. This completely ignores [`LOG-EVENT`][3a51]s in both journals
    being compared, and is updated continuously during [Replay][0dc7]. It plays
    a role in [`WITH-BUNDLE`][0ddc] deciding when a journal is important enough to
    keep, and also in [Synchronization with in-memory journals][86a2].
    
    The position of the first mismatch that is available via
    [`JOURNAL-REPLAY-MISMATCH`][9444].

<a id='x-28JOURNAL-3A-40COMPARING-JOURNALS-20MGL-PAX-3ASECTION-29'></a>

### 14.1 Comparing journals

After replay finished (i.e. [`WITH-JOURNALING`][234e] completed), we can ask
the question whether there were any changes produced. This is
answered in the strictest sense by [`IDENTICAL-JOURNALS-P`][05ca], and
somewhat more functionally by [`EQUIVALENT-REPLAY-JOURNALS-P`][75ef].

Also see [`JOURNAL-DIVERGENT-P`][f748].

<a id='x-28JOURNAL-3AIDENTICAL-JOURNALS-P-20GENERIC-FUNCTION-29'></a>

- [generic-function] **IDENTICAL-JOURNALS-P** *JOURNAL-1 JOURNAL-2*

    Compare two journals in a strict sense: whether
    they have the same [`JOURNAL-STATE`][3631] and the lists of their events (as
    in [`LIST-EVENTS`][3c76]) are `EQUAL`.

<a id='x-28JOURNAL-3AEQUIVALENT-REPLAY-JOURNALS-P-20GENERIC-FUNCTION-29'></a>

- [generic-function] **EQUIVALENT-REPLAY-JOURNALS-P** *JOURNAL-1 JOURNAL-2*

    See if two journals are equivalent when used the
    for `REPLAY` in [`WITH-JOURNALING`][234e]. `EQUIVALENT-REPLAY-JOURNALS-P` is like
    [`IDENTICAL-JOURNALS-P`][05ca], but it ignores [`LOG-EVENT`][3a51]s and allows events
    with `EVENT-EXIT`([`0`][a6d1] [`1`][306a]) `:ERROR` to differ in their outcomes, which may very
    well be implementation specific, anyway. Also, it considers two
    groups of states as different `:NEW`, `:REPLAYING`, `:MISMATCHED`, `:FAILED`
    vs `:RECORDING`, `:LOGGING`, COMPLETED.

The rest of section is about concrete subclasses of [`JOURNAL`][86bc].

<a id='x-28JOURNAL-3A-40IN-MEMORY-JOURNALS-20MGL-PAX-3ASECTION-29'></a>

### 14.2 In-memory journals

<a id='x-28JOURNAL-3AIN-MEMORY-JOURNAL-20CLASS-29'></a>

- [class] **IN-MEMORY-JOURNAL** *[JOURNAL][86bc]*

    `IN-MEMORY-JOURNAL`s are backed by a non-persistent,
    Lisp array of events. Much quicker than [`FILE-JOURNAL`][f6b2]s, they are
    ideal for smallish journals persisted manually (see
    [Synchronization with in-memory journals][86a2] for an example).
    
    They are also useful for writing tests based on what events were
    generated. They differ from `FILE-JOURNAL`s in that events written to
    `IN-MEMORY-JOURNAL`s are not serialized (and deserialized on replay)
    with the following consequences for the objects recorded by
    [`JOURNALED`][4f52] (i.e. its `NAME`, `ARGS` arguments, but also the return `VALUES`
    of the block, or the value returned by `CONDITION`):
    
    - These objects need not be [readable][531d].
    
    - Their identity (`EQ`ness) is not lost.
    
    - They must **must not be mutated** in any way.


<a id='x-28JOURNAL-3AMAKE-IN-MEMORY-JOURNAL-20FUNCTION-29'></a>

- [function] **MAKE-IN-MEMORY-JOURNAL** *&KEY (EVENTS NIL EVENTSP) STATE (SYNC NIL SYNCP) SYNC-FN*

    By default, creates an empty [`IN-MEMORY-JOURNAL`][17a8] in [`JOURNAL-STATE`][3631]
    `:NEW`, which is suitable for recording. To make a replay journal, use
    `:STATE` `:COMPLETED` with some sequence of `EVENTS`:
    
    ```
    (make-in-memory-journal :events '((:in foo :version 1)) :state :completed)
    ```
    
    If the `EVENTS` argument is provided, then `STATE` defaults to `:NEW`,
    else to `:COMPLETED`.
    
    `SYNC` determines when `SYNC-FN` will be invoked on the [`RECORD-JOURNAL`][35c4].
    `SYNC` defaults to `T` if `SYNC-FN`, else to `NIL`. For a description of
    possible values, see [Synchronization strategies][355b]. For more
    discussion, see [Synchronization with in-memory journals][86a2].

<a id='x-28JOURNAL-3AJOURNAL-EVENTS-20-28MGL-PAX-3AREADER-20JOURNAL-3AIN-MEMORY-JOURNAL-29-29'></a>

- [reader] **JOURNAL-EVENTS** *IN-MEMORY-JOURNAL* *(:EVENTS)*

    A sequence of events in the journal. Not to be
    mutated by client code.

<a id='x-28JOURNAL-3AJOURNAL-PREVIOUS-SYNC-POSITION-20-28MGL-PAX-3AREADER-20JOURNAL-3AIN-MEMORY-JOURNAL-29-29'></a>

- [reader] **JOURNAL-PREVIOUS-SYNC-POSITION** *IN-MEMORY-JOURNAL* *(= 0)*

    The length of [`JOURNAL-EVENTS`][4454] at the time of the
    most recent invocation of `SYNC-FN`.

<a id='x-28JOURNAL-3A-40FILE-JOURNALS-20MGL-PAX-3ASECTION-29'></a>

### 14.3 File journals

<a id='x-28JOURNAL-3AFILE-JOURNAL-20CLASS-29'></a>

- [class] **FILE-JOURNAL** *[JOURNAL][86bc]*

    A `FILE-JOURNAL` is a journal whose contents and
    [`JOURNAL-STATE`][3631] are persisted in a file. This is the [`JOURNAL`][86bc]
    subclass with out-of-the-box persistence, but see [File bundles][4e3d] for
    a more full-featured solution for repeated [Replay][0dc7]s.
    
    Since serialization in `FILE-JOURNAL`s is built on top of Lisp `READ`
    and `WRITE`, everything that [`JOURNALED`][4f52] records in events (i.e. its
    `NAME`, `ARGS` arguments, but also the return `VALUES` of the block, or
    the value returned by `CONDITION`) must be [readable][531d].
    
    File journals are human-readable, and editable by hand with some
    care. When editing, the following needs to be remembered:
    
    - The first character of the file represents its `JOURNAL-STATE`. It
      is a `#\Space` (for state `:NEW`, `:REPLAYING`, `:MISMATCHED` and
      `:FAILED`), or a `#\Newline` (for state `:RECORDING`, `:LOGGING` and
      `:COMPLETED`).
    
    - If the journal has `SYNC` (see [Synchronization strategies][355b]), then in
      between events, there may be `#\Del` (also called `#\Rubout`) or
      `#\Ack` characters (`CHAR-CODE` 127 and 6). `#\Del` marks the end of
      the journal contents which may be read back: it's kind of an
      uncommitted-transaction marker for the events that follow it.
      `#\Ack` characters, of which there may be many in the file, mark
      the sequence of events until the next marker of either kind as
      valid (or committed). `#\Ack` characters are ignored when reading
      the journal.
    
    Thus, when editing a file, don't change the first character and
    leave the `#\Del` character, if any, where it is. Also see
    [Synchronization with file journals][eb29].

<a id='x-28JOURNAL-3AMAKE-FILE-JOURNAL-20FUNCTION-29'></a>

- [function] **MAKE-FILE-JOURNAL** *PATHNAME &KEY SYNC*

    Return a [`FILE-JOURNAL`][f6b2] backed by the file with `PATHNAME`. The file is
    created when the journal is opened for writing. For a description of
    `SYNC`, see [Synchronization strategies][355b].
    
    If there is already an existing `FILE-JOURNAL` backed by the same
    file, then that object is returned. If the existing object has
    different options (e.g. it has `SYNC` `T` while the `SYNC` argument is `NIL`
    here), then a [`JOURNAL-ERROR`][571f] is signalled.
    
    If there is already an existing `FILE-JOURNAL` backed by the same
    file, the [`JOURNAL-STATE`][3631] is not `:NEW`, but the file doesn't exist,
    then the existing object is **invalidated**: attempts to write will
    fail with `JOURNAL-ERROR`. If the existing journal object is being
    written, then invalidation fails with a `JOURNAL-ERROR`. After
    invalidation, a new `FILE-JOURNAL` object is created.

<a id='x-28JOURNAL-3APATHNAME-OF-20-28MGL-PAX-3AREADER-20JOURNAL-3AFILE-JOURNAL-29-29'></a>

- [reader] **PATHNAME-OF** *FILE-JOURNAL* *(:PATHNAME)*

    The pathname of the file backing the journal.

<a id='x-28JOURNAL-3A-40PPRINT-JOURNALS-20MGL-PAX-3ASECTION-29'></a>

### 14.4 Pretty-printing journals

<a id='x-28JOURNAL-3APPRINT-JOURNAL-20CLASS-29'></a>

- [class] **PPRINT-JOURNAL** *[JOURNAL][86bc]*

    When an event is written to a `PPRINT-JOURNAL` it
    writes that event to a stream in a customizable format. They are
    intended for producing prettier output for [Logging][77df] and [Tracing][7849],
    but they do not support reads so they cannot be used as a
    [`REPLAY-JOURNAL`][103b], or in [`LIST-EVENTS`][3c76], for example. On the other hand,
    events written to `PPRINT-JOURNAL`s need not be [readable][531d].

<a id='x-28JOURNAL-3AMAKE-PPRINT-JOURNAL-20FUNCTION-29'></a>

- [function] **MAKE-PPRINT-JOURNAL** *&KEY (STREAM (MAKE-SYNONYM-STREAM '\*STANDARD-OUTPUT\*)) (PRETTY T) (PRETTIFIER 'PRETTIFY-EVENT) LOG-DECORATOR*

    Creates a [`PPRINT-JOURNAL`][2123].

<a id='x-28JOURNAL-3APPRINT-JOURNAL-STREAM-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3APPRINT-JOURNAL-29-29'></a>

- [accessor] **PPRINT-JOURNAL-STREAM** *PPRINT-JOURNAL* *(:STREAM = \*STANDARD-OUTPUT\*)*

    The stream where events are dumped. May be set any
    time to another `STREAM`.

<a id='x-28JOURNAL-3APPRINT-JOURNAL-PRETTY-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3APPRINT-JOURNAL-29-29'></a>

- [accessor] **PPRINT-JOURNAL-PRETTY** *PPRINT-JOURNAL* *(:PRETTY = T)*

    Whether to use [`PPRINT-JOURNAL-PRETTIFIER`][a4ee] or write
    events in as the property lists they are. A
    [boolean-valued symbol][dcba].

<a id='x-28JOURNAL-3APPRINT-JOURNAL-PRETTIFIER-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3APPRINT-JOURNAL-29-29'></a>

- [accessor] **PPRINT-JOURNAL-PRETTIFIER** *PPRINT-JOURNAL* *(:PRETTIFIER = 'PRETTIFY-EVENT)*

    A function like [`PRETTIFY-EVENT`][29db] that writes its an
    event to a stream. Only used when [`PPRINT-JOURNAL-PRETTY`][37f3], this is
    the output format customization knob. Also see [decoration][ca1a]s.

<a id='x-28JOURNAL-3A-40BUNDLES-REFERENCE-20MGL-PAX-3ASECTION-29'></a>

## 15 Bundles reference

In [Bundles][5b0f], we covered the repeated replay problem that
[`WITH-BUNDLE`][0ddc] automates. Here we provide a reference for the bundle
classes.

<a id='x-28JOURNAL-3ABUNDLE-20CLASS-29'></a>

- [class] **BUNDLE**

    This is an abstract base class. Direct subclasses
    are [`IN-MEMORY-BUNDLE`][2314] and [`FILE-BUNDLE`][eb5d].
    
    A `BUNDLE` consists of a sequence of journals which are all reruns of
    the same code, hopefully making more and more progress towards
    completion. These journals are [Replay][0dc7]s of the previous successful
    one, extending it with new events. Upon replay (see [`WITH-BUNDLE`][0ddc]),
    the latest journal in the bundle in [`JOURNAL-STATE`][3631] `:COMPLETED` plays
    the role of the replay journal, and a new journal is added to the
    bundle for recording. If the replay succeeds, this new journal
    eventually becomes `:COMPLETED` and takes over the role of the replay
    journal for future replays until another replay succeeds. When the
    bundle is created and it has no journals yet, the replay journal is
    an empty, completed one.

<a id='x-28JOURNAL-3AMAX-N-FAILED-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3ABUNDLE-29-29'></a>

- [accessor] **MAX-N-FAILED** *BUNDLE* *(:MAX-N-FAILED = 1)*

    If `MAX-N-FAILED` is non-NIL, and the number of
    journals of [`JOURNAL-STATE`][3631] `:FAILED` in the bundle exceeds
    its value, then some journals (starting with the oldest) are
    deleted.

<a id='x-28JOURNAL-3AMAX-N-COMPLETED-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3ABUNDLE-29-29'></a>

- [accessor] **MAX-N-COMPLETED** *BUNDLE* *(:MAX-N-COMPLETED = 1)*

    If `MAX-N-COMPLETED` is non-NIL, and the number of
    journals of [`JOURNAL-STATE`][3631] `:COMPLETED` in the bundle exceeds
    its value, then some journals (starting with the oldest) are
    deleted.

<a id='x-28JOURNAL-3A-40IN-MEMORY-BUNDLES-20MGL-PAX-3ASECTION-29'></a>

### 15.1 In-memory bundles

<a id='x-28JOURNAL-3AIN-MEMORY-BUNDLE-20CLASS-29'></a>

- [class] **IN-MEMORY-BUNDLE** *[BUNDLE][8906]*

    An `IN-MEMORY-BUNDLE` is a [`BUNDLE`][8906] that is built on
    [`IN-MEMORY-JOURNAL`][17a8]s. `IN-MEMORY-BUNDLE`s have limited utility as a
    persistence mechanism and are provided mainly for reasons of
    symmetry and for testing. See
    [Synchronization with in-memory journals][86a2] for an example of how to
    achieve persistence without bundles.

<a id='x-28JOURNAL-3AMAKE-IN-MEMORY-BUNDLE-20FUNCTION-29'></a>

- [function] **MAKE-IN-MEMORY-BUNDLE** *&KEY (MAX-N-FAILED 1) (MAX-N-COMPLETED 1) SYNC SYNC-FN*

    Create a new [`IN-MEMORY-BUNDLE`][2314] with [`MAX-N-FAILED`][3cef] and [`MAX-N-COMPLETED`][d472]. `SYNC` and `SYNC-FN`
    are passed on to [`MAKE-IN-MEMORY-JOURNAL`][0605].

<a id='x-28JOURNAL-3A-40FILE-BUNDLES-20MGL-PAX-3ASECTION-29'></a>

### 15.2 File bundles

<a id='x-28JOURNAL-3AFILE-BUNDLE-20CLASS-29'></a>

- [class] **FILE-BUNDLE** *[BUNDLE][8906]*

    A `FILE-BUNDLE` is a [`BUNDLE`][8906] that is built on
    [`FILE-JOURNAL`][f6b2]s. It provides easy replay-based persistence.

<a id='x-28JOURNAL-3ADIRECTORY-OF-20-28MGL-PAX-3AREADER-20JOURNAL-3AFILE-BUNDLE-29-29'></a>

- [reader] **DIRECTORY-OF** *FILE-BUNDLE* *(:DIRECTORY)*

    The directory where the files backing the
    [`FILE-JOURNAL`][f6b2]s in the [`FILE-BUNDLE`][eb5d] are kept.

<a id='x-28JOURNAL-3AMAKE-FILE-BUNDLE-20FUNCTION-29'></a>

- [function] **MAKE-FILE-BUNDLE** *DIRECTORY &KEY (MAX-N-FAILED 1) (MAX-N-COMPLETED 1) SYNC*

    Return a [`FILE-BUNDLE`][eb5d] object backed by [`FILE-JOURNAL`][f6b2]s in `DIRECTORY`.
    See [MAX-N-FAILED][(accessor file-bundle)] and
    [MAX-N-COMPLETED][(accessor file-bundle)]. For a description of
    `SYNC`, see [Synchronization strategies][355b].
    
    If there is already a `FILE-BUNDLE` with the same directory (according
    to `TRUENAME`), return that object is returned if it has the same
    `MAX-N-FAILED`, `MAX-N-COMPLETED` and `SYNC` options, else [`JOURNAL-ERROR`][571f]
    is signalled.

<a id='x-28JOURNAL-3ADELETE-FILE-BUNDLE-20FUNCTION-29'></a>

- [function] **DELETE-FILE-BUNDLE** *DIRECTORY*

    Delete all journal files (`*.jrn`) from `DIRECTORY`. Delete the
    directory if empty after the journal files were deleted, else signal
    an error. Existing [`FILE-BUNDLE`][eb5d] objects are not updated, so
    [`MAKE-FILE-JOURNAL`][182e] with FORCE-RELOAD may be required.

<a id='x-28JOURNAL-3A-40STREAMLETS-REFERENCE-20MGL-PAX-3ASECTION-29'></a>

## 16 Streamlets reference

This section is relevant mostly for implementing new kinds of
`JOURNALs` in addition to [`FILE-JOURNAL`][f6b2]s and [`IN-MEMORY-JOURNAL`][17a8]s. In
normal operation, [`STREAMLET`][4f72]s are not worked with directly.

<a id='x-28JOURNAL-3A-40OPENING-AND-CLOSING-20MGL-PAX-3ASECTION-29'></a>

### 16.1 Opening and closing

<a id='x-28JOURNAL-3ASTREAMLET-20CLASS-29'></a>

- [class] **STREAMLET**

    A `STREAMLET` is a handle to perform I/O on a
    [`JOURNAL`][86bc]. The high-level stuff ([`WITH-JOURNALING`][234e], [`JOURNALED`][4f52], etc) is
    built on top of streamlets.

<a id='x-28JOURNAL-3AJOURNAL-20-28MGL-PAX-3AREADER-20JOURNAL-3ASTREAMLET-29-29'></a>

- [reader] **JOURNAL** *STREAMLET* *(:JOURNAL)*

    The `JOURNAL` that was passed to [`OPEN-STREAMLET`][2411].
    This is the journal `STREAMLET` operates on.

<a id='x-28JOURNAL-3AOPEN-STREAMLET-20GENERIC-FUNCTION-29'></a>

- [generic-function] **OPEN-STREAMLET** *JOURNAL &KEY DIRECTION*

    Return a [`STREAMLET`][4f72] suitable for performing I/O on
    `JOURNAL`. `DIRECTION` (defaults to `:INPUT`) is one of `:INPUT`, `:OUTPUT`,
    `:IO` and it has the same purpose as the similarly named argument of
    `CL:OPEN`.

<a id='x-28JOURNAL-3ACLOSE-STREAMLET-20GENERIC-FUNCTION-29'></a>

- [generic-function] **CLOSE-STREAMLET** *STREAMLET*

    Close `STREAMLET`, which was returned by
    [`OPEN-STREAMLET`][2411]. After closing, `STREAMLET` may not longer be used for
    IO.

<a id='x-28JOURNAL-3AMAKE-STREAMLET-FINALIZER-20GENERIC-FUNCTION-29'></a>

- [generic-function] **MAKE-STREAMLET-FINALIZER** *STREAMLET*

    Return `NIL` or a function of no arguments suitable
    as a finalizer for `STREAMLET`. That is, the function closes
    `STREAMLET`, but holds no reference to it. This is intended for
    streamlets which are not dynamic-extent, so using [`WITH-OPEN-JOURNAL`][8383]
    is not appropriate.

<a id='x-28JOURNAL-3AOPEN-STREAMLET-P-20GENERIC-FUNCTION-29'></a>

- [generic-function] **OPEN-STREAMLET-P** *STREAMLET*

    Return true if `STREAMLET` is open. `STREAMLET`s are
    open until they have been explicitly closed with [`CLOSE-STREAMLET`][bc40].

<a id='x-28JOURNAL-3AINPUT-STREAMLET-P-20FUNCTION-29'></a>

- [function] **INPUT-STREAMLET-P** *STREAMLET*

    See if `STREAMLET` was opened for input (the `DIRECTION` argument of
    [`OPEN-STREAMLET`][2411] was `:INPUT` or `:IO`).

<a id='x-28JOURNAL-3AOUTPUT-STREAMLET-P-20FUNCTION-29'></a>

- [function] **OUTPUT-STREAMLET-P** *STREAMLET*

    See if `STREAMLET` was opened for input (the `DIRECTION` argument of
    [`OPEN-STREAMLET`][2411] was `:OUTPUT` or `:IO`).

<a id='x-28JOURNAL-3AWITH-OPEN-JOURNAL-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-OPEN-JOURNAL** *(VAR JOURNAL &KEY (DIRECTION :INPUT)) &BODY BODY*

    This is like `WITH-OPEN-FILE`. Open the journal designated by
    `JOURNAL` (see [`TO-JOURNAL`][ce19]) with [`OPEN-STREAMLET`][2411], passing `DIRECTION`
    along, and bind `VAR` to the resulting [`STREAMLET`][4f72]. Call [`CLOSE-STREAMLET`][bc40]
    after `BODY` finishes. If `JOURNAL` is `NIL`, then `VAR` is bound to `NIL` and
    no streamlet is created.

<a id='x-28JOURNAL-3ASTREAMLET-ERROR-20CONDITION-29'></a>

- [condition] **STREAMLET-ERROR** *ERROR*

    Like `CL:STREAM-ERROR:` failures regarding trying to
    perform I/O on a closed [`STREAMLET`][4f72] or of the wrong `DIRECTION`. Actual
    I/O errors are *not* encapsulated in `STREAMLET-ERROR`.

<a id='x-28JOURNAL-3A-40READING-FROM-STREAMLETS-20MGL-PAX-3ASECTION-29'></a>

### 16.2 Reading from streamlets

<a id='x-28JOURNAL-3AREAD-EVENT-20GENERIC-FUNCTION-29'></a>

- [generic-function] **READ-EVENT** *STREAMLET &OPTIONAL EOJ-ERROR-P*

    Read the event at the current read position from
    `STREAMLET` and move the read position to the event after. If there
    are no more events, signal [`END-OF-JOURNAL`][9c7e] or return `NIL` depending on
    `EOJ-ERROR-P`. Signals [`STREAMLET-ERROR`][4dc9] if `STREAMLET` is not
    [`INPUT-STREAMLET-P`][f6e2] or not [`OPEN-STREAMLET-P`][d5b1].

<a id='x-28JOURNAL-3AREAD-POSITION-20GENERIC-FUNCTION-29'></a>

- [generic-function] **READ-POSITION** *STREAMLET*

    Return an integer that identifies the position of
    the next event to be read from `STREAMLET`. `SETF`able, see
    [`SET-READ-POSITION`][cb6d].

<a id='x-28JOURNAL-3ASET-READ-POSITION-20GENERIC-FUNCTION-29'></a>

- [generic-function] **SET-READ-POSITION** *STREAMLET POSITION*

    Set the read position of `STREAMLET` to `POSITION`,
    which must have been acquired from [`READ-POSITION`][1548].

<a id='x-28JOURNAL-3ASAVE-EXCURSION-20MGL-PAX-3AMACRO-29'></a>

- [macro] **SAVE-EXCURSION** *(STREAMLET) &BODY BODY*

    Save [`READ-POSITION`][1548] of `STREAMLET`, execute `BODY`, and make sure to
    restore the saved read position.

<a id='x-28JOURNAL-3APEEK-EVENT-20GENERIC-FUNCTION-29'></a>

- [generic-function] **PEEK-EVENT** *STREAMLET*

    Read the next event from `STREAMLET` without changing
    the read position, or return `NIL` if there is no event to be read.

<a id='x-28JOURNAL-3APEEK-EVENT-20-28METHOD-20NIL-20-28JOURNAL-3ASTREAMLET-29-29-29'></a>

- [method] **PEEK-EVENT** *(STREAMLET STREAMLET)*

    This is a slow default implementation, which relies on
    [`SAVE-EXCURSION`][0200] and [`READ-EVENT`][6ed4].

<a id='x-28JOURNAL-3A-40WRITING-TO-STREAMLETS-20MGL-PAX-3ASECTION-29'></a>

### 16.3 Writing to streamlets

<a id='x-28JOURNAL-3AWRITE-EVENT-20GENERIC-FUNCTION-29'></a>

- [generic-function] **WRITE-EVENT** *EVENT STREAMLET*

    Write `EVENT` to `STREAMLET`. Writing always happens at
    the end of `STREAMLET`'s journal regardless of the [`READ-POSITION`][1548] and
    the read position is not changed. Signals [`STREAMLET-ERROR`][4dc9] if
    `STREAMLET` is not [`OUTPUT-STREAMLET-P`][bca2] or not [`OPEN-STREAMLET-P`][d5b1].

<a id='x-28JOURNAL-3AWRITE-EVENT-20-28METHOD-20NIL-20-28T-20JOURNAL-3AJOURNAL-29-29-29'></a>

- [method] **WRITE-EVENT** *EVENT (JOURNAL JOURNAL)*

    For convenience, it is possible to write directly to a `JOURNAL`,
    in which case the journal's internal output streamlet is used.
    This internal streamlet is opened for `:OUTPUT` and may be used by
    `LOG-RECORD`.

<a id='x-28JOURNAL-3AWRITE-POSITION-20GENERIC-FUNCTION-29'></a>

- [generic-function] **WRITE-POSITION** *STREAMLET*

    Return an integer that identifies the position of
    the next event to be written to `STREAMLET`.

<a id='x-28JOURNAL-3AREQUEST-COMPLETED-ON-ABORT-20GENERIC-FUNCTION-29'></a>

- [generic-function] **REQUEST-COMPLETED-ON-ABORT** *STREAMLET*

    Make it so that upon [aborted execution][33c1] `STREAMLET`'s
    [`JOURNAL`][86bc] will be in [`JOURNAL-STATE`][3631] `:COMPLETED` when loaded fresh (e.g.
    [`FILE-JOURNAL`][f6b2] from a file). Any previously written events must be
    persisted before making this change. Before
    `REQUEST-COMPLETED-ON-ABORT` is called, a journal must be reloaded in
    state `:FAILED`.
    
    It is permissible to defer carrying out this request until the next
    [`SYNC-STREAMLET`][7049] call. If the request was carried out, return true. If
    it was deferred, return `NIL`.

<a id='x-28JOURNAL-3ASYNC-STREAMLET-20GENERIC-FUNCTION-29'></a>

- [generic-function] **SYNC-STREAMLET** *STREAMLET*

    Durably persist the effects of all preceding
    [`WRITE-EVENT`][0448] calls made via `STREAMLET` to its journal and any deferred
    [`REQUEST-COMPLETED-ON-ABORT`][a8b4] in this order.

<a id='x-28JOURNAL-3A-40JOURNAL-2FGLOSSARY-20MGL-PAX-3ASECTION-29'></a>

## 17 Glossary

<a id='x-28JOURNAL-3A-3A-40ASYNC-UNWIND-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **async-unwind**

    If an asynchronous event, say a `SIGINT` triggered by `C-c`, is
    delivered to a thread running Lisp or foreign code called from Lisp,
    a Lisp condition is typically signalled. If the handler for this
    condition unwinds the stack, then we have an asynchronous unwind.
    Another example is `BT:INTERRUPT-THREAD` which, as it can execute
    arbitrary code, may unwind the stack in the target thread.

<a id='x-28JOURNAL-3A-3A-40BOOLEAN-VALUED-SYMBOL-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **boolean-valued symbol**

    Imagine writing two `STREAM`s with a spaghetti of functions and
    wanting to have pretty-printed output on one of them. Unfortunately,
    binding `*PRINT-PRETTY*` to `T` will affect writes to both streams.
    
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
    `STREAM-PRINT-PRETTY` to `NIL` or `T` also works, because they are
    self-evaluating.
    
    If not by `CL:STREAM`s, boolean-valued symbols are used by
    [`MAKE-LOG-DECORATOR`][e04b] and [`PPRINT-JOURNAL`][2123]s.

<a id='x-28JOURNAL-3A-3A-40NON-LOCAL-EXIT-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **non-local exit**

    This is a term from the Common Lisp ANSI standard. If a form does
    not return normally, but control is transferred via `GO`, `RETURN`,
    `RETURN-FROM` or `THROW`, then it is said to have performed a non-local
    exit. This definition of a non-local exit includes `EVENT-EXIT`([`0`][a6d1] [`1`][306a])
    `:CONDITION`, `:ERROR` and `:NLX`.

<a id='x-28JOURNAL-3A-3A-40READABLE-20MGL-PAX-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **readable**

    In Common Lisp, readable objects are those that can be printed readably.
    Anything written to stream-based journals needs to be readable.

  [01aa]: #x-28JOURNAL-3ADELETE-FILE-BUNDLE-20FUNCTION-29 "(JOURNAL:DELETE-FILE-BUNDLE FUNCTION)"
  [0200]: #x-28JOURNAL-3ASAVE-EXCURSION-20MGL-PAX-3AMACRO-29 "(JOURNAL:SAVE-EXCURSION MGL-PAX:MACRO)"
  [040a]: #x-28JOURNAL-3A-40IN-EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29 "In-events"
  [0448]: #x-28JOURNAL-3AWRITE-EVENT-20GENERIC-FUNCTION-29 "(JOURNAL:WRITE-EVENT GENERIC-FUNCTION)"
  [05ca]: #x-28JOURNAL-3AIDENTICAL-JOURNALS-P-20GENERIC-FUNCTION-29 "(JOURNAL:IDENTICAL-JOURNALS-P GENERIC-FUNCTION)"
  [0605]: #x-28JOURNAL-3AMAKE-IN-MEMORY-JOURNAL-20FUNCTION-29 "(JOURNAL:MAKE-IN-MEMORY-JOURNAL FUNCTION)"
  [0765]: #x-28JOURNAL-3ALOGGED-20MGL-PAX-3AMACRO-29 "(JOURNAL:LOGGED MGL-PAX:MACRO)"
  [0919]: #x-28JOURNAL-3A-40JOURNAL-UTILITIES-20MGL-PAX-3ASECTION-29 "Utilities"
  [0bc8]: #x-28JOURNAL-3A-40JOURNALED-FOR-REPLAY-20MGL-PAX-3ASECTION-29 "Journaled for replay"
  [0d06]: #x-28JOURNAL-3AJOURNAL-SYNC-20-28MGL-PAX-3AREADER-20JOURNAL-3AJOURNAL-29-29 "(JOURNAL:JOURNAL-SYNC (MGL-PAX:READER JOURNAL:JOURNAL))"
  [0dc7]: #x-28JOURNAL-3A-40REPLAY-20MGL-PAX-3ASECTION-29 "Replay"
  [0ddc]: #x-28JOURNAL-3AWITH-BUNDLE-20MGL-PAX-3AMACRO-29 "(JOURNAL:WITH-BUNDLE MGL-PAX:MACRO)"
  [0f76]: #x-28JOURNAL-3AEVENT-20TYPE-29 "(JOURNAL:EVENT TYPE)"
  [103b]: #x-28JOURNAL-3AREPLAY-JOURNAL-20FUNCTION-29 "(JOURNAL:REPLAY-JOURNAL FUNCTION)"
  [1311]: #x-28JOURNAL-3A-40JOURNAL-2FGLOSSARY-20MGL-PAX-3ASECTION-29 "Glossary"
  [1452]: #x-28JOURNAL-3A-40FRAME-20MGL-PAX-3AGLOSSARY-TERM-29 "(JOURNAL:@FRAME MGL-PAX:GLOSSARY-TERM)"
  [1548]: #x-28JOURNAL-3AREAD-POSITION-20GENERIC-FUNCTION-29 "(JOURNAL:READ-POSITION GENERIC-FUNCTION)"
  [1668]: #x-28JOURNAL-3ADATA-EVENT-LOSSAGE-20CONDITION-29 "(JOURNAL:DATA-EVENT-LOSSAGE CONDITION)"
  [1726]: #x-28JOURNAL-3A-40JOURNAL-LINKS-20MGL-PAX-3ASECTION-29 "Links"
  [17a8]: #x-28JOURNAL-3AIN-MEMORY-JOURNAL-20CLASS-29 "(JOURNAL:IN-MEMORY-JOURNAL CLASS)"
  [182e]: #x-28JOURNAL-3AMAKE-FILE-JOURNAL-20FUNCTION-29 "(JOURNAL:MAKE-FILE-JOURNAL FUNCTION)"
  [1acb]: #x-28JOURNAL-3A-40UPGRADES-AND-REPLAY-20MGL-PAX-3ASECTION-29 "Upgrades and replay"
  [1d28]: #x-28JOURNAL-3A-40IN-EVENTS-20MGL-PAX-3ASECTION-29 "In-events"
  [2123]: #x-28JOURNAL-3APPRINT-JOURNAL-20CLASS-29 "(JOURNAL:PPRINT-JOURNAL CLASS)"
  [21d1]: #x-28JOURNAL-3A-40MATCHING-OUT-EVENTS-20MGL-PAX-3ASECTION-29 "Matching out-events"
  [2314]: #x-28JOURNAL-3AIN-MEMORY-BUNDLE-20CLASS-29 "(JOURNAL:IN-MEMORY-BUNDLE CLASS)"
  [234e]: #x-28JOURNAL-3AWITH-JOURNALING-20MGL-PAX-3AMACRO-29 "(JOURNAL:WITH-JOURNALING MGL-PAX:MACRO)"
  [2411]: #x-28JOURNAL-3AOPEN-STREAMLET-20GENERIC-FUNCTION-29 "(JOURNAL:OPEN-STREAMLET GENERIC-FUNCTION)"
  [2453]: #x-28JOURNAL-3A-40STREAMLETS-REFERENCE-20MGL-PAX-3ASECTION-29 "Streamlets reference"
  [248f]: #x-28JOURNAL-3AERROR-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29 "(JOURNAL:ERROR-OUTCOME MGL-PAX:GLOSSARY-TERM)"
  [254d]: #x-28JOURNAL-3AJTRACE-20MGL-PAX-3AMACRO-29 "(JOURNAL:JTRACE MGL-PAX:MACRO)"
  [28a4]: #x-28JOURNAL-3AREPLAY-ARGS-MISMATCH-20CONDITION-29 "(JOURNAL:REPLAY-ARGS-MISMATCH CONDITION)"
  [29db]: #x-28JOURNAL-3APRETTIFY-EVENT-20FUNCTION-29 "(JOURNAL:PRETTIFY-EVENT FUNCTION)"
  [2b45]: #x-28JOURNAL-3AEVENT-NAME-20FUNCTION-29 "(JOURNAL:EVENT-NAME FUNCTION)"
  [2ee0]: #x-28JOURNAL-3A-40JOURNAL-PORTABILITY-20MGL-PAX-3ASECTION-29 "Portability"
  [2efb]: #x-28JOURNAL-3AREPLAY-VERSION-DOWNGRADE-20CONDITION-29 "(JOURNAL:REPLAY-VERSION-DOWNGRADE CONDITION)"
  [306a]: #x-28JOURNAL-3AEVENT-EXIT-20TYPE-29 "(JOURNAL:EVENT-EXIT TYPE)"
  [31b6]: #x-28JOURNAL-3A-40JOURNAL-BACKGROUND-20MGL-PAX-3ASECTION-29 "Background"
  [32a1]: #x-28JOURNAL-3AREPLAY-INCOMPLETE-20CONDITION-29 "(JOURNAL:REPLAY-INCOMPLETE CONDITION)"
  [32e0]: #x-28JOURNAL-3AEXPECTED-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29 "(JOURNAL:EXPECTED-OUTCOME MGL-PAX:GLOSSARY-TERM)"
  [3380]: #x-28JOURNAL-3A-40LOG-RECORD-20MGL-PAX-3ASECTION-29 "Log record"
  [33c1]: #x-28JOURNAL-3AABORTED-EXECUTION-20MGL-PAX-3AGLOSSARY-TERM-29 "(JOURNAL:ABORTED-EXECUTION MGL-PAX:GLOSSARY-TERM)"
  [355b]: #x-28JOURNAL-3A-40SYNCHRONIZATION-STRATEGIES-20MGL-PAX-3ASECTION-29 "Synchronization strategies"
  [35c4]: #x-28JOURNAL-3ARECORD-JOURNAL-20FUNCTION-29 "(JOURNAL:RECORD-JOURNAL FUNCTION)"
  [3631]: #x-28JOURNAL-3AJOURNAL-STATE-20TYPE-29 "(JOURNAL:JOURNAL-STATE TYPE)"
  [37f3]: #x-28JOURNAL-3APPRINT-JOURNAL-PRETTY-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3APPRINT-JOURNAL-29-29 "(JOURNAL:PPRINT-JOURNAL-PRETTY (MGL-PAX:ACCESSOR JOURNAL:PPRINT-JOURNAL))"
  [3991]: #x-28JOURNAL-3A-40JOURNALS-REFERENCE-20MGL-PAX-3ASECTION-29 "Journals reference"
  [39f4]: #x-28JOURNAL-3AVALUES-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29 "(JOURNAL:VALUES-OUTCOME MGL-PAX:GLOSSARY-TERM)"
  [3a51]: #x-28JOURNAL-3ALOG-EVENT-20TYPE-29 "(JOURNAL:LOG-EVENT TYPE)"
  [3bd9]: #x-28JOURNAL-3APRINT-EVENTS-20FUNCTION-29 "(JOURNAL:PRINT-EVENTS FUNCTION)"
  [3c00]: #x-28JOURNAL-3A-40THE-REPLAY-STRATEGY-20MGL-PAX-3ASECTION-29 "The replay strategy"
  [3c76]: #x-28JOURNAL-3ALIST-EVENTS-20FUNCTION-29 "(JOURNAL:LIST-EVENTS FUNCTION)"
  [3cef]: #x-28JOURNAL-3AMAX-N-FAILED-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3ABUNDLE-29-29 "(JOURNAL:MAX-N-FAILED (MGL-PAX:ACCESSOR JOURNAL:BUNDLE))"
  [3f41]: #x-28JOURNAL-3AMAKE-IN-MEMORY-BUNDLE-20FUNCTION-29 "(JOURNAL:MAKE-IN-MEMORY-BUNDLE FUNCTION)"
  [4182]: #x-28JOURNAL-3A-40JOURNAL-FEATURES-20MGL-PAX-3ASECTION-29 "Distinguishing features"
  [4454]: #x-28JOURNAL-3AJOURNAL-EVENTS-20-28MGL-PAX-3AREADER-20JOURNAL-3AIN-MEMORY-JOURNAL-29-29 "(JOURNAL:JOURNAL-EVENTS (MGL-PAX:READER JOURNAL:IN-MEMORY-JOURNAL))"
  [4492]: #x-28JOURNAL-3AINVOKED-20MGL-PAX-3AGLOSSARY-TERM-29 "(JOURNAL:INVOKED MGL-PAX:GLOSSARY-TERM)"
  [48bb]: #x-28JOURNAL-3AFLET-INVOKED-20MGL-PAX-3AMACRO-29 "(JOURNAL:FLET-INVOKED MGL-PAX:MACRO)"
  [4c38]: #x-28JOURNAL-3AMAKE-IN-EVENT-20FUNCTION-29 "(JOURNAL:MAKE-IN-EVENT FUNCTION)"
  [4dc9]: #x-28JOURNAL-3ASTREAMLET-ERROR-20CONDITION-29 "(JOURNAL:STREAMLET-ERROR CONDITION)"
  [4e3d]: #x-28JOURNAL-3A-40FILE-BUNDLES-20MGL-PAX-3ASECTION-29 "File bundles"
  [4f52]: #x-28JOURNAL-3AJOURNALED-20MGL-PAX-3AMACRO-29 "(JOURNAL:JOURNALED MGL-PAX:MACRO)"
  [4f5f]: #x-28JOURNAL-3AEVENT-VERSION-20FUNCTION-29 "(JOURNAL:EVENT-VERSION FUNCTION)"
  [4f72]: #x-28JOURNAL-3ASTREAMLET-20CLASS-29 "(JOURNAL:STREAMLET CLASS)"
  [51dd]: #x-28JOURNAL-3A-40WORKING-WITH-UNREADABLE-VALUES-20MGL-PAX-3ASECTION-29 "Working with unreadable values"
  [524c]: #x-28JOURNAL-3AREPLAY-FORCE-INSERT-20RESTART-29 "(JOURNAL:REPLAY-FORCE-INSERT RESTART)"
  [531d]: #x-28JOURNAL-3A-3A-40READABLE-20MGL-PAX-3AGLOSSARY-TERM-29 "(JOURNAL::@READABLE MGL-PAX:GLOSSARY-TERM)"
  [5428]: #x-28JOURNAL-3A-2ATRACE-JOURNAL-2A-20VARIABLE-29 "(JOURNAL:*TRACE-JOURNAL* VARIABLE)"
  [55b7]: #x-28JOURNAL-3AREPLAY-OUTCOME-MISMATCH-20CONDITION-29 "(JOURNAL:REPLAY-OUTCOME-MISMATCH CONDITION)"
  [571f]: #x-28JOURNAL-3AJOURNAL-ERROR-20CONDITION-29 "(JOURNAL:JOURNAL-ERROR CONDITION)"
  [5721]: #x-28JOURNAL-3A-40OUT-EVENTS-20MGL-PAX-3ASECTION-29 "Out-events"
  [588a]: #x-28JOURNAL-3A-40REPLAY-FAILURES-20MGL-PAX-3ASECTION-29 "Replay failures"
  [5b0f]: #x-28JOURNAL-3A-40BUNDLES-20MGL-PAX-3ASECTION-29 "Bundles"
  [5ca9]: #x-28JOURNAL-3AMAKE-OUT-EVENT-20FUNCTION-29 "(JOURNAL:MAKE-OUT-EVENT FUNCTION)"
  [5da8]: #x-28JOURNAL-3AEVENT-ARGS-20FUNCTION-29 "(JOURNAL:EVENT-ARGS FUNCTION)"
  [5ef1]: #x-28JOURNAL-3AREPLAY-FORCE-UPGRADE-20RESTART-29 "(JOURNAL:REPLAY-FORCE-UPGRADE RESTART)"
  [61f5]: #x-28JOURNAL-3ANLX-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29 "(JOURNAL:NLX-OUTCOME MGL-PAX:GLOSSARY-TERM)"
  [635d]: #x-28JOURNAL-3ADEFINE-INVOKED-20MGL-PAX-3AMACRO-29 "(JOURNAL:DEFINE-INVOKED MGL-PAX:MACRO)"
  [63a5]: #x-28JOURNAL-3APEEK-REPLAY-EVENT-20FUNCTION-29 "(JOURNAL:PEEK-REPLAY-EVENT FUNCTION)"
  [6572]: #x-28JOURNAL-3A-40BLOCK-20MGL-PAX-3AGLOSSARY-TERM-29 "(JOURNAL:@BLOCK MGL-PAX:GLOSSARY-TERM)"
  [68a8]: #x-28JOURNAL-3A-40IN-MEMORY-JOURNALS-20MGL-PAX-3ASECTION-29 "In-memory journals"
  [6bc6]: #x-28JOURNAL-3ASYNC-JOURNAL-20FUNCTION-29 "(JOURNAL:SYNC-JOURNAL FUNCTION)"
  [6be9]: #x-28JOURNAL-3A-40LOGGING-WITH-LEAVES-20MGL-PAX-3ASECTION-29 "Logging with leaf-events"
  [6db0]: #x-28JOURNAL-3AJOURNALING-FAILURE-20CONDITION-29 "(JOURNAL:JOURNALING-FAILURE CONDITION)"
  [6ed4]: #x-28JOURNAL-3AREAD-EVENT-20GENERIC-FUNCTION-29 "(JOURNAL:READ-EVENT GENERIC-FUNCTION)"
  [7049]: #x-28JOURNAL-3ASYNC-STREAMLET-20GENERIC-FUNCTION-29 "(JOURNAL:SYNC-STREAMLET GENERIC-FUNCTION)"
  [7224]: #x-28JOURNAL-3A-40SAFETY-20MGL-PAX-3ASECTION-29 "Safety"
  [74d2]: #x-28JOURNAL-3APPRINT-EVENTS-20FUNCTION-29 "(JOURNAL:PPRINT-EVENTS FUNCTION)"
  [75ef]: #x-28JOURNAL-3AEQUIVALENT-REPLAY-JOURNALS-P-20GENERIC-FUNCTION-29 "(JOURNAL:EQUIVALENT-REPLAY-JOURNALS-P GENERIC-FUNCTION)"
  [75ff]: #x-28JOURNAL-3A-40COMPARING-JOURNALS-20MGL-PAX-3ASECTION-29 "Comparing journals"
  [77df]: #x-28JOURNAL-3A-40LOGGING-20MGL-PAX-3ASECTION-29 "Logging"
  [7849]: #x-28JOURNAL-3A-40TRACING-20MGL-PAX-3ASECTION-29 "Tracing"
  [7bd1]: #x-28JOURNAL-3A-40FILE-JOURNALS-20MGL-PAX-3ASECTION-29 "File journals"
  [7bd7]: #x-28JOURNAL-3AMAKE-FILE-BUNDLE-20FUNCTION-29 "(JOURNAL:MAKE-FILE-BUNDLE FUNCTION)"
  [7c19]: #x-28JOURNAL-3A-40JOURNAL-BASICS-20MGL-PAX-3ASECTION-29 "Basics"
  [806e]: #x-28JOURNAL-3AFRAMED-20MGL-PAX-3AMACRO-29 "(JOURNAL:FRAMED MGL-PAX:MACRO)"
  [8383]: #x-28JOURNAL-3AWITH-OPEN-JOURNAL-20MGL-PAX-3AMACRO-29 "(JOURNAL:WITH-OPEN-JOURNAL MGL-PAX:MACRO)"
  [86a2]: #x-28JOURNAL-3A-40SYNCHRONIZATION-WITH-IN-MEMORY-JOURNALS-20MGL-PAX-3ASECTION-29 "Synchronization with in-memory journals"
  [86bc]: #x-28JOURNAL-3AJOURNAL-20CLASS-29 "(JOURNAL:JOURNAL CLASS)"
  [8906]: #x-28JOURNAL-3ABUNDLE-20CLASS-29 "(JOURNAL:BUNDLE CLASS)"
  [8d93]: #x-28JOURNAL-3AREPLAY-UNEXPECTED-OUTCOME-20CONDITION-29 "(JOURNAL:REPLAY-UNEXPECTED-OUTCOME CONDITION)"
  [917c]: #x-28JOURNAL-3AEVENT-VERSION-20TYPE-29 "(JOURNAL:EVENT-VERSION TYPE)"
  [9382]: #x-28JOURNAL-3A-40EVENT-VERSIONS-20MGL-PAX-3ASECTION-29 "Event versions"
  [9444]: #x-28JOURNAL-3AJOURNAL-REPLAY-MISMATCH-20-28MGL-PAX-3AREADER-20JOURNAL-3AJOURNAL-29-29 "(JOURNAL:JOURNAL-REPLAY-MISMATCH (MGL-PAX:READER JOURNAL:JOURNAL))"
  [955f]: #x-28JOURNAL-3AREPLAY-FAILURE-20CONDITION-29 "(JOURNAL:REPLAY-FAILURE CONDITION)"
  [958b]: #x-28JOURNAL-3A-2ATRACE-TIME-2A-20VARIABLE-29 "(JOURNAL:*TRACE-TIME* VARIABLE)"
  [95b8]: #x-28JOURNAL-3AEVENT-OUTCOME-20FUNCTION-29 "(JOURNAL:EVENT-OUTCOME FUNCTION)"
  [9607]: #x-28JOURNAL-3A-40PRETTY-PRINTING-20MGL-PAX-3ASECTION-29 "Pretty-printing"
  [98d3]: #x-28JOURNAL-3A-40PERSISTENCE-20MGL-PAX-3ASECTION-29 "Persistence"
  [9c42]: #x-28-23A-28-287-29-20BASE-CHAR-20-2E-20-22journal-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29 "(#A((7) BASE-CHAR . \"journal\") ASDF/SYSTEM:SYSTEM)"
  [9c7e]: #x-28JOURNAL-3AEND-OF-JOURNAL-20CONDITION-29 "(JOURNAL:END-OF-JOURNAL CONDITION)"
  [9dd2]: #x-28JOURNAL-3A-40OUT-EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29 "Out-events"
  [a074]: #x-28JOURNAL-3A-40SYNCHRONIZATION-20MGL-PAX-3ASECTION-29 "Synchronization to storage"
  [a1fe]: #x-28JOURNAL-3A-40BUNDLES-REFERENCE-20MGL-PAX-3ASECTION-29 "Bundles reference"
  [a4ee]: #x-28JOURNAL-3APPRINT-JOURNAL-PRETTIFIER-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3APPRINT-JOURNAL-29-29 "(JOURNAL:PPRINT-JOURNAL-PRETTIFIER (MGL-PAX:ACCESSOR JOURNAL:PPRINT-JOURNAL))"
  [a6d1]: #x-28JOURNAL-3AEVENT-EXIT-20FUNCTION-29 "(JOURNAL:EVENT-EXIT FUNCTION)"
  [a799]: #x-28JOURNAL-3AVALUES--3E-20FUNCTION-29 "(JOURNAL:VALUES-> FUNCTION)"
  [a8b4]: #x-28JOURNAL-3AREQUEST-COMPLETED-ON-ABORT-20GENERIC-FUNCTION-29 "(JOURNAL:REQUEST-COMPLETED-ON-ABORT GENERIC-FUNCTION)"
  [ade5]: #x-28JOURNAL-3A-40OPENING-AND-CLOSING-20MGL-PAX-3ASECTION-29 "Opening and closing"
  [ae7d]: #x-28JOURNAL-3A-40LEAF-EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29 "Leaf-events"
  [afa1]: #x-28JOURNAL-3A-40WRITING-TO-STREAMLETS-20MGL-PAX-3ASECTION-29 "Writing to streamlets"
  [afda]: #x-28JOURNAL-3A-40IN-MEMORY-BUNDLES-20MGL-PAX-3ASECTION-29 "In-memory bundles"
  [b6d1]: #x-28JOURNAL-3A-40REPLAYING-THE-OUTCOME-20MGL-PAX-3ASECTION-29 "Replaying the outcome"
  [b9bc]: #x-28JOURNAL-3AJOURNALING-FAILURE-EMBEDDED-CONDITION-20-28MGL-PAX-3AREADER-20JOURNAL-3AJOURNALING-FAILURE-29-29 "(JOURNAL:JOURNALING-FAILURE-EMBEDDED-CONDITION (MGL-PAX:READER JOURNAL:JOURNALING-FAILURE))"
  [ba76]: #x-28JOURNAL-3A-3A-40ASYNC-UNWIND-20MGL-PAX-3AGLOSSARY-TERM-29 "(JOURNAL::@ASYNC-UNWIND MGL-PAX:GLOSSARY-TERM)"
  [bb0f]: #x-28JOURNAL-3A-40CUSTOMIZING-LOGS-20MGL-PAX-3ASECTION-29 "Customizing logs"
  [bc40]: #x-28JOURNAL-3ACLOSE-STREAMLET-20GENERIC-FUNCTION-29 "(JOURNAL:CLOSE-STREAMLET GENERIC-FUNCTION)"
  [bca2]: #x-28JOURNAL-3AOUTPUT-STREAMLET-P-20FUNCTION-29 "(JOURNAL:OUTPUT-STREAMLET-P FUNCTION)"
  [bce9]: #x-28JOURNAL-3APPRINT-JOURNAL-STREAM-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3APPRINT-JOURNAL-29-29 "(JOURNAL:PPRINT-JOURNAL-STREAM (MGL-PAX:ACCESSOR JOURNAL:PPRINT-JOURNAL))"
  [c28b]: #x-28JOURNAL-3A-40TESTING-20MGL-PAX-3ASECTION-29 "Testing"
  [c2fb]: #x-28JOURNAL-3AREPLAYED-20MGL-PAX-3AMACRO-29 "(JOURNAL:REPLAYED MGL-PAX:MACRO)"
  [c7bd]: #x-28JOURNAL-3A-2ATRACE-PRETTY-2A-20VARIABLE-29 "(JOURNAL:*TRACE-PRETTY* VARIABLE)"
  [ca07]: #x-28JOURNAL-3AREPLAY-EVENT-20MGL-PAX-3AGLOSSARY-TERM-29 "(JOURNAL:REPLAY-EVENT MGL-PAX:GLOSSARY-TERM)"
  [ca1a]: #x-28JOURNAL-3A-40DECORATION-20MGL-PAX-3AGLOSSARY-TERM-29 "(JOURNAL:@DECORATION MGL-PAX:GLOSSARY-TERM)"
  [cb6d]: #x-28JOURNAL-3ASET-READ-POSITION-20GENERIC-FUNCTION-29 "(JOURNAL:SET-READ-POSITION GENERIC-FUNCTION)"
  [cba8]: #x-28JOURNAL-3AOUT-EVENT-20TYPE-29 "(JOURNAL:OUT-EVENT TYPE)"
  [cc3d]: #x-28JOURNAL-3ACHECKED-20MGL-PAX-3AMACRO-29 "(JOURNAL:CHECKED MGL-PAX:MACRO)"
  [ce19]: #x-28JOURNAL-3ATO-JOURNAL-20GENERIC-FUNCTION-29 "(JOURNAL:TO-JOURNAL GENERIC-FUNCTION)"
  [cf72]: #x-28JOURNAL-3ARECORD-UNEXPECTED-OUTCOME-20CONDITION-29 "(JOURNAL:RECORD-UNEXPECTED-OUTCOME CONDITION)"
  [d031]: #x-28JOURNAL-3AREPLAY-NAME-MISMATCH-20CONDITION-29 "(JOURNAL:REPLAY-NAME-MISMATCH CONDITION)"
  [d3f7]: #x-28JOURNAL-3AJOURNAL-LOG-DECORATOR-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3AJOURNAL-29-29 "(JOURNAL:JOURNAL-LOG-DECORATOR (MGL-PAX:ACCESSOR JOURNAL:JOURNAL))"
  [d472]: #x-28JOURNAL-3AMAX-N-COMPLETED-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3ABUNDLE-29-29 "(JOURNAL:MAX-N-COMPLETED (MGL-PAX:ACCESSOR JOURNAL:BUNDLE))"
  [d5b1]: #x-28JOURNAL-3AOPEN-STREAMLET-P-20GENERIC-FUNCTION-29 "(JOURNAL:OPEN-STREAMLET-P GENERIC-FUNCTION)"
  [d6c0]: #x-28JOURNAL-3A-40JOURNAL-SLIME-INTEGRATION-20MGL-PAX-3ASECTION-29 "Slime integration"
  [d95f]: #x-28JOURNAL-3A-40TESTING-ON-MULTIPLE-LEVELS-20MGL-PAX-3ASECTION-29 "Testing on multiple levels"
  [d9ae]: #x-28JOURNAL-3A-40EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29 "Events reference"
  [dcba]: #x-28JOURNAL-3A-3A-40BOOLEAN-VALUED-SYMBOL-20MGL-PAX-3AGLOSSARY-TERM-29 "(JOURNAL::@BOOLEAN-VALUED-SYMBOL MGL-PAX:GLOSSARY-TERM)"
  [e04a]: #x-28JOURNAL-3A-40MATCHING-IN-EVENTS-20MGL-PAX-3ASECTION-29 "Matching in-events"
  [e04b]: #x-28JOURNAL-3AMAKE-LOG-DECORATOR-20FUNCTION-29 "(JOURNAL:MAKE-LOG-DECORATOR FUNCTION)"
  [e099]: #x-28JOURNAL-3A-40READING-FROM-STREAMLETS-20MGL-PAX-3ASECTION-29 "Reading from streamlets"
  [e10e]: #x-28JOURNAL-3AIN-EVENT-20TYPE-29 "(JOURNAL:IN-EVENT TYPE)"
  [e17e]: #x-28JOURNAL-3A-3A-40NON-LOCAL-EXIT-20MGL-PAX-3AGLOSSARY-TERM-29 "(JOURNAL::@NON-LOCAL-EXIT MGL-PAX:GLOSSARY-TERM)"
  [e29b]: #x-28JOURNAL-3AVALUES-3C--20FUNCTION-29 "(JOURNAL:VALUES<- FUNCTION)"
  [eb29]: #x-28JOURNAL-3A-40SYNCHRONIZATION-WITH-FILE-JOURNALS-20MGL-PAX-3ASECTION-29 "Synchronization with file journals"
  [eb5d]: #x-28JOURNAL-3AFILE-BUNDLE-20CLASS-29 "(JOURNAL:FILE-BUNDLE CLASS)"
  [ecce]: #x-28JOURNAL-3ADATA-EVENT-20MGL-PAX-3AGLOSSARY-TERM-29 "(JOURNAL:DATA-EVENT MGL-PAX:GLOSSARY-TERM)"
  [eed7]: #x-28JOURNAL-3AEXTERNAL-EVENT-20TYPE-29 "(JOURNAL:EXTERNAL-EVENT TYPE)"
  [eeda]: #x-28JOURNAL-3A-40PPRINT-JOURNALS-20MGL-PAX-3ASECTION-29 "Pretty-printing journals"
  [f4eb]: #x-28JOURNAL-3ACONDITION-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29 "(JOURNAL:CONDITION-OUTCOME MGL-PAX:GLOSSARY-TERM)"
  [f57e]: #x-28JOURNAL-3AUNEXPECTED-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29 "(JOURNAL:UNEXPECTED-OUTCOME MGL-PAX:GLOSSARY-TERM)"
  [f6b2]: #x-28JOURNAL-3AFILE-JOURNAL-20CLASS-29 "(JOURNAL:FILE-JOURNAL CLASS)"
  [f6e2]: #x-28JOURNAL-3AINPUT-STREAMLET-P-20FUNCTION-29 "(JOURNAL:INPUT-STREAMLET-P FUNCTION)"
  [f6f7]: #x-28JOURNAL-3A-40PERSISTENCE-TUTORIAL-20MGL-PAX-3ASECTION-29 "Persistence tutorial"
  [f748]: #x-28JOURNAL-3AJOURNAL-DIVERGENT-P-20FUNCTION-29 "(JOURNAL:JOURNAL-DIVERGENT-P FUNCTION)"
  [fa00]: #x-28JOURNAL-3AWITH-REPLAY-FILTER-20MGL-PAX-3AMACRO-29 "(JOURNAL:WITH-REPLAY-FILTER MGL-PAX:MACRO)"
  [fb13]: #x-28JOURNAL-3A-40JOURNAL-ERROR-HANDLING-20MGL-PAX-3ASECTION-29 "Error handling"
  [fc8e]: #x-28JOURNAL-3A-2ATRACE-THREAD-2A-20VARIABLE-29 "(JOURNAL:*TRACE-THREAD* VARIABLE)"
  [fef8]: #x-28JOURNAL-3ALEAF-EVENT-20TYPE-29 "(JOURNAL:LEAF-EVENT TYPE)"
  [ffc4]: #x-28JOURNAL-3AVERSIONED-EVENT-20TYPE-29 "(JOURNAL:VERSIONED-EVENT TYPE)"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
