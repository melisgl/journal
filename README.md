<a id="x-28JOURNAL-3A-40JOURNAL-MANUAL-20MGL-PAX-3ASECTION-29"></a>
# Journal manual

## Table of Contents

- [1 `JOURNAL` ASDF System][4a9f]
- [2 Links][aa14]
- [3 Portability][1d0d]
- [4 Background][0114]
- [5 Distinguishing features][9105]
- [6 Basics][f846]
    - [6.1 In-events][186b]
    - [6.2 Out-events][48ef]
    - [6.3 Working with unreadable values][b354]
    - [6.4 Utilities][ba32]
    - [6.5 Pretty-printing][47a7]
    - [6.6 Error handling][bb08]
- [7 Logging][4e53]
    - [7.1 Customizing logs][297c]
    - [7.2 `:LOG-RECORD`][a6ac]
    - [7.3 Logging with `LEAF-EVENT`s][bece]
- [8 Tracing][e03f]
    - [8.1 Slime integration][42a5]
- [9 Replay][041c]
    - [9.1 Journaled for replay][d700]
    - [9.2 Bundles][260d]
    - [9.3 The replay strategy][a8a7]
    - [9.4 Matching in-events][3c21]
        - [9.4.1 Replaying the outcome][7991]
    - [9.5 Matching out-events][7f9d]
    - [9.6 Replay failures][2933]
    - [9.7 Upgrades and replay][750a]
- [10 Testing][7682]
    - [10.1 Testing on multiple levels][9376]
- [11 Persistence][37c4]
    - [11.1 Persistence tutorial][6169]
    - [11.2 Synchronization to storage][046e]
        - [11.2.1 Synchronization strategies][f532]
        - [11.2.2 Synchronization with in-memory journals][12ff]
        - [11.2.3 Synchronization with file journals][674f]
- [12 Safety][7bf3]
- [13 Events reference][faf2]
    - [13.1 Event versions][54c1]
    - [13.2 In-events][f37b]
    - [13.3 Out-events][72cd]
    - [13.4 Leaf-events][86f6]
- [14 Journals reference][fbbb]
    - [14.1 Comparing journals][b7d2]
    - [14.2 In-memory journals][b792]
    - [14.3 File journals][e748]
    - [14.4 Pretty-printing journals][1496]
- [15 Bundles reference][ff8f]
    - [15.1 In-memory bundles][8bc1]
    - [15.2 File bundles][c05a]
- [16 Streamlets reference][f4d5]
    - [16.1 Opening and closing][bfc5]
    - [16.2 Reading from streamlets][adcd]
    - [16.3 Writing to streamlets][14f7]
- [17 Glossary][48f5]

###### \[in package JOURNAL with nicknames JRN\]
<a id="x-28-22journal-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
## 1 `JOURNAL` ASDF System

- Version: 0.1.0
- Description: A library built around explicit execution traces for
  logging, tracing, testing and persistence.
- Licence: MIT, see COPYING.
- Author: Gábor Melis <mega@retes.hu>
- Homepage: [http://github.com/melisgl/journal](http://github.com/melisgl/journal)
- Bug tracker: [http://github.com/melisgl/journal/issues](http://github.com/melisgl/journal/issues)
- Source control: [GIT](https://github.com/melisgl/journal.git)

<a id="x-28JOURNAL-3A-40JOURNAL-LINKS-20MGL-PAX-3ASECTION-29"></a>
## 2 Links

Here is the [official repository](https://github.com/melisgl/journal)
and the [HTML
documentation](http://melisgl.github.io/mgl-pax-world/journal-manual.html)
for the latest version.

<a id="x-28JOURNAL-3A-40JOURNAL-PORTABILITY-20MGL-PAX-3ASECTION-29"></a>
## 3 Portability

Tested on ABCL, CCL, CLISP, CMUCL, ECL, and SBCL. AllegroCL Express
edition runs out of heap while running the tests. On Lisps that seem
to lack support for disabling and enabling of interrupts, such as
ABCL and CLISP, durability is compromised, and any attempt to
[`SYNC-JOURNAL`][b2ff] (see [Synchronization strategies][f532] and [Safety][7bf3]) will be a
runtime error.

<a id="x-28JOURNAL-3A-40JOURNAL-BACKGROUND-20MGL-PAX-3ASECTION-29"></a>
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
single macro at its heart: [`JOURNALED`][6267], which does pretty much what
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

This is basically how recording works. When replaying events from a
previous run, the return values of `BODY` can be checked against the
recorded ones, or we may return the recorded values without even
running `BODY`.

In summary, we can produce selective execution traces by wrapping
code in `JOURNALED` and use those traces for various purposes. The
Journal library is this idea taken to its logical conclusion.

<a id="x-28JOURNAL-3A-40JOURNAL-FEATURES-20MGL-PAX-3ASECTION-29"></a>
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

See [Logging][4e53] for a complete example.

##### Compared to [`CL:TRACE`][548d]

- Ability to handle [non-local exit][3b76]s

- Customizable content and format

- Optional timestamps, internal real- and run-time

```
(FOO 2.1)
  (1+ 2.1)
  => 3.1
=E "SIMPLE-ERROR" "The assertion (INTEGERP 3.1) failed."
```

See [Tracing][e03f] for a complete example.

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

See [Testing][7682] for a complete example.

##### As a solution for persistence

- Event Sourcing: replay interactions with the external world

- Unchanged control flow

- Easy to implement history, undo

```
(defun my-resumable-autosaving-game-with-history ()
  (with-bundle (bundle)
    (play-guess-my-number)))
```

See [Persistence][37c4] for a complete example.

<a id="x-28JOURNAL-3A-40JOURNAL-BASICS-20MGL-PAX-3ASECTION-29"></a>
## 6 Basics

The [`JOURNALED`][6267] macro does both recording and replaying of events,
possibly at the same time. Recording is easy: events generated by
`JOURNALED` are simply written to a journal, which is a sequence of
events much like a file. What events are generated is described in
`JOURNALED`. [Replay][041c] is much more involved, thus it gets its own
section. The journals used for recording and replaying are specified
by [`WITH-JOURNALING`][6131] or by [`WITH-BUNDLE`][12a5].

The [Journals reference][fbbb] is presented later, but for most purposes,
creating them (e.g. with [`MAKE-IN-MEMORY-JOURNAL`][9955], [`MAKE-FILE-JOURNAL`][f0e7])
and maybe querying their contents with [`LIST-EVENTS`][0c1b] will suffice.
Some common cases of journal creation are handled by the convenience
function [`TO-JOURNAL`][e8ed].

Built on top of journals, [Bundles][260d] juggle repeated replay-and-record
cycles focussing on persistence.

<a id="x-28JOURNAL-3ATO-JOURNAL-20GENERIC-FUNCTION-29"></a>
- [generic-function] **TO-JOURNAL** *DESIGNATOR*

    Return the journal designated by `DESIGNATOR` or
    signal an error. The default implementation:
    
    - returns `DESIGNATOR` itself if it is of type [`JOURNAL`][5082],
    
    - returns a new [`IN-MEMORY-JOURNAL`][b668] if `DESIGNATOR` is `T`,
    
    - returns a new [`FILE-JOURNAL`][8428] if `DESIGNATOR` is a `PATHNAME`([`0`][a1ab] [`1`][241f]).


<a id="x-28JOURNAL-3AWITH-JOURNALING-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-JOURNALING** *(&KEY RECORD REPLAY REPLAY-EOJ-ERROR-P) &BODY BODY*

    Turn recording and/or replaying of events on or off for the
    duration of `BODY`. Both `RECORD` and `REPLAY` should be a [`JOURNAL`][5082]
    designator (in the sense of [`TO-JOURNAL`][e8ed]) or `NIL`.
    
    If `RECORD` designates a `JOURNAL`, then events generated by enclosed
    [`JOURNALED`][6267] [block][06a7]s are written to that journal (with exceptions, see
    the `LOG-RECORD` argument of `JOURNALED`). If `REPLAY` designates a
    `JOURNAL`, then the generated events are matched against events from
    that journal according to the rules of [Replay][041c].
    
    A [`JOURNAL-ERROR`][0002] is signalled if `RECORD` is a `JOURNAL` that has been
    previously recorded to by another `WITH-JOURNALING` (that is, if its
    [`JOURNAL-STATE`][03de] is not `:NEW`) or if `REPLAY` is a `JOURNAL` that is not a
    complete recording of successful replay (i.e. its `JOURNAL-STATE` is
    not `:COMPLETED`). These checks are intended to catch mistakes that
    would render the new or existing records unusable for replay. When
    `WITH-JOURNALING` finishes, the `RECORD` journal is marked `:COMPLETED` or
    `:FAILED` in its `JOURNAL-STATE`.
    
    `REPLAY-EOJ-ERROR-P` controls whether [`END-OF-JOURNAL`][3cdb] is signalled when
    a new event is being matched to the replay journal from which there
    are no more events to read. If there was a [`JOURNALING-FAILURE`][3956] or a
    [`REPLAY-FAILURE`][2e9b] during execution, then `END-OF-JOURNAL` is not
    signalled.
    
    If `BODY` completes successfully, but `REPLAY` has unprocessed events,
    then [`REPLAY-INCOMPLETE`][e442] is signalled.
    
    `WITH-JOURNALING` for different `RECORD` journals can be nested and run
    independently.

<a id="x-28JOURNAL-3A-40BLOCK-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **block**

    A journaled block, or simply block, is a number of forms wrapped in
    [`JOURNALED`][6267]. When a block is executed, a [frame][7df7] is created.

<a id="x-28JOURNAL-3A-40FRAME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **frame**

    A frame is an [`IN-EVENT`][1729], [`OUT-EVENT`][637d] pair, which are created when a
    [block][06a7] is entered and left, respectively.

<a id="x-28JOURNAL-3ARECORD-JOURNAL-20FUNCTION-29"></a>
- [function] **RECORD-JOURNAL**

    Return the [`JOURNAL`][5082] in which events are currently being
    recorded (see [`WITH-JOURNALING`][6131] and [`WITH-BUNDLE`][12a5]) or `NIL`.

<a id="x-28JOURNAL-3AREPLAY-JOURNAL-20FUNCTION-29"></a>
- [function] **REPLAY-JOURNAL**

    Return the [`JOURNAL`][5082] from which events are currently being
    replayed (see [`WITH-JOURNALING`][6131] and [`WITH-BUNDLE`][12a5]) or `NIL`.

<a id="x-28JOURNAL-3AJOURNALED-20MGL-PAX-3AMACRO-29"></a>
- [macro] **JOURNALED** *(NAME &KEY (LOG-RECORD :RECORD) VERSION ARGS VALUES CONDITION INSERTABLE REPLAY-VALUES REPLAY-CONDITION) &BODY BODY*

    `JOURNALED` generates events upon entering and leaving the dynamic
    extent of `BODY` (also known as the journaled [block][06a7]), which we call
    the [In-events][186b] and [Out-events][48ef]. Between generating the two events,
    `BODY` is typically executed normally (except for
    [Replaying the outcome][7991]).
    
    Where the generated events are written is determined by the `:RECORD`
    argument of the enclosing [`WITH-JOURNALING`][6131]. If there is no enclosing
    `WITH-JOURNALING` and `LOG-RECORD` is `NIL`, then event recording is
    turned off and `JOURNALED` imposes minimal overhead.
    
    - `NAME` can be of any type except [`NULL`][9daf], not evaluated. For
      names, and for anything that gets written to a journal, a
      non-keyword symbol is a reasonable choice as it can be easily made
      unique. However, it also exposes the package structure, which
      might make reading stuff back more difficult. Keywords and strings
      do not have this problem.
    
    - `ARGS` can be of any type, but is typically a list.
    
    Also see [`:LOG-RECORD`][a6ac] in the [Logging][4e53] section. For a description of
    `VERSION`, `INSERTABLE`, `REPLAY-VALUES` and `REPLAY-CONDITION`, see
    [Journaled for replay][d700].

<a id="x-28JOURNAL-3A-40IN-EVENTS-20MGL-PAX-3ASECTION-29"></a>
### 6.1 In-events

Upon entering a [block][06a7], [`JOURNALED`][6267] generates an [`IN-EVENT`][1729],
which conceptually opens a new [frame][7df7]. These in-events are created
from the `NAME`, `VERSION` and `ARGS` arguments of `JOURNALED`. For example,

```
(journaled (name :version version :args args) ...)
```

creates an event like this:

```
`(:in ,name :version ,version :args ,args)
```

where `:VERSION` and `:ARGS` may be omitted if they are `NIL`. Versions
are used for [Replay][041c].

<a id="x-28JOURNAL-3A-40OUT-EVENTS-20MGL-PAX-3ASECTION-29"></a>
### 6.2 Out-events

Upon leaving a [block][06a7], [`JOURNALED`][6267] generates an [`OUT-EVENT`][637d], closing
the [frame][7df7] opened by the corresponding [`IN-EVENT`][1729]. These out-events
are property lists like this:

```
(:out foo :version 1 :values (42))
```

Their `NAME` and `VERSION` (`FOO` and `1` in the example) are the same
as in the in-event: they come from the corresponding arguments of
`JOURNALED`. `EXIT` and `OUTCOME` are filled in differently depending on
how the block finished its execution.

<a id="x-28JOURNAL-3AEVENT-EXIT-20TYPE-29"></a>
- [type] **EVENT-EXIT**

    One of `:VALUES`, `:CONDITION`, `:ERROR` and `:NLX`. Indicates whether a
    journaled [block][06a7]
    
    - returned normally (`:VALUES`, see [values outcome][3ac1]),
    
    - unwound on an expected condition (`:CONDITION`, see [condition outcome][9d9f]),
    
    - unwound on an unexpected condition (`:ERROR`, see [error outcome][560b]),
    
    - unwound by performing a [non-local exit][3b76] of some other kind such as
      a throw (`:NLX`, see [nlx outcome][68eb]).
    
    The first two are [expected outcome][4657]s, while the latter two are
    [unexpected outcome][d2c1]s.

<a id="x-28JOURNAL-3A-40VALUES-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **values outcome**

    If the [`JOURNALED`][6267] [block][06a7] returns normally, [`EVENT-EXIT`][812a] is
    `:VALUES`, and the outcome is the list of values returned:
    
    ```
    (journaled (foo) (values 7 t))
    ;; generates the out-event
    (:out foo :values (7 t))
    ```
    
    The list of return values of the block is transformed by the `VALUES`
    argument of `JOURNALED`, whose default is `#'IDENTITY`. Also see
    [Working with unreadable values][b354]).

<a id="x-28JOURNAL-3A-40CONDITION-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **condition outcome**

    If the [block][06a7] unwound due to a condition, and [`JOURNALED`][6267]'s
    `CONDITION` argument (a function whose default is `(CONSTANTLY NIL)`)
    returns non-NIL when invoked on it, then [`EVENT-EXIT`][812a] is
    `:CONDITION`, and the outcome is this return value:
    
    ```
    (journaled (foo :condition (lambda (c) (prin1-to-string c)))
      (error "xxx"))
    ;; generates the out-event
    (:out foo :condition "xxx")
    ```
    
    Conditions thus recognized are those that can be considered part of
    normal execution. Just like return values, these expected conditions
    may be required to match what's in the replay journal. Furthermore,
    given a suitable `REPLAY-CONDITION` in `JOURNALED`, they may be replayed
    without running the [block][06a7].

<a id="x-28JOURNAL-3A-40ERROR-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **error outcome**

    If the [`JOURNALED`][6267] [block][06a7] unwound due to a condition, but
    `JOURNALED`'s `CONDITION` argument returns `NIL` when invoked on it, then
    [`EVENT-EXIT`][812a] is `:ERROR` and the outcome the string
    representations of the type of the condition and the condition
    itself.
    
    ```
    (journaled (foo)
      (error "xxx"))
    ;; generates this out-event:
    ;; (:out foo :error ("simple-error" "xxx"))
    ```
    
    The conversion to string is performed with [`PRINC`][f47b] in
    [`WITH-STANDARD-IO-SYNTAX`][c488]. This scheme is intended to avoid leaking
    random implementation details into the journal, which would make
    [`READ`][3d3c]ing it back difficult.
    
    In contrast with [condition outcome][9d9f]s, error outcomes are what the
    code is not prepared to handle or replay in a meaningful way.

<a id="x-28JOURNAL-3A-40NLX-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **nlx outcome**

    If the [`JOURNALED`][6267] [block][06a7] performed a [non-local exit][3b76] that was not
    due to a condition, then [`EVENT-EXIT`][812a] is `:NLX` and the outcome
    is `NIL`.
    
    ```
    (catch 'xxx
      (journaled (foo)
        (throw 'xxx nil)))
    ;; generates the out-event
    (:out foo :nlx nil)
    ```
    
    Note that [condition outcome][9d9f]s and [error outcome][560b]s are also due to
    [non-local exit][3b76]s but are distinct from nlx outcomes.
    
    Currently, nlx outcomes are detected rather heuristically as there
    is no portable way to detect what really caused the unwinding of the
    stack.

There is a further grouping of outcomes into expected and unexpected.

<a id="x-28JOURNAL-3A-40EXPECTED-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **expected outcome**

    An [`OUT-EVENT`][637d] is said to have an expected outcome if it had a
    [values outcome][3ac1] or a [condition outcome][9d9f], or equivalently, when its
    [`EVENT-EXIT`][812a] is `:VALUES` or `:CONDITION`.

<a id="x-28JOURNAL-3A-40UNEXPECTED-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **unexpected outcome**

    An [`OUT-EVENT`][637d] is said to have an unexpected outcome if it had an
    [error outcome][560b] or an [nlx outcome][68eb], or equivalently, when its
    [`EVENT-EXIT`][812a] is `:ERROR` or `:NLX`.

<a id="x-28JOURNAL-3A-40WORKING-WITH-UNREADABLE-VALUES-20MGL-PAX-3ASECTION-29"></a>
### 6.3 Working with unreadable values

The events recorded often need to be [readable][768f]. This is always
required with [`FILE-JOURNAL`][8428]s, often with [`IN-MEMORY-JOURNAL`][b668]s, but
never with [`PPRINT-JOURNAL`][9150]s. By choosing an appropriate identifier or
string representation of the unreadable object to journal, this is
not a problem in practice. [`JOURNALED`][6267] provides the `VALUES`
hook for this purpose.

With [`EXTERNAL-EVENT`][0e53]s, whose outcome is replayed (see
[Replaying the outcome][7991]), we also need to be able to reverse the
transformation of `VALUES`, and this is what the
`REPLAY-VALUES` argument of `JOURNALED` is for.

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

To be able to journal the return values of `GET-MESSAGE`, the `USER`
object must be transformed to something [readable][768f]. On the
`Recording` run, `(VALUES-> #'USER-ID)` replaces the user object
with its id in the [`EVENT-OUTCOME`][c290] recorded, but the original user
object is returned.

When `Replaying`, the journaled [`OUT-EVENT`][637d] is replayed (see
[Replaying the outcome][7991]):

```
(:OUT GET-MESSAGE :VERSION :INFINITY :VALUES (7 "hello"))
```

The user object is looked up according to `:REPLAY-VALUES` and is
returned along with `"hello"`.

<a id="x-28JOURNAL-3AVALUES--3E-20FUNCTION-29"></a>
- [function] **VALUES-\>** *&REST FNS*

    A utility to create a function suitable as the `VALUES`
    argument of [`JOURNALED`][6267]. The [`VALUES`][e88d] function is called with the list
    of values returned by the [block][06a7] and returns a transformed set of
    values that may be recorded in a journal. While arbitrary
    transformations are allowed, `VALUES->` handles the common case of
    transforming individual elements of the list independently by
    calling the functions in FN with the values of the list of the same
    position.
    
    ```
    (funcall (values-> #'1+) '(7 :something))
    => (8 :SOMETHING)
    ```
    
    Note how `#'1+` is applied only to the first element of the values
    list. The list of functions is shorter than the values list, so
    `:SOMETHING` is not transformed. A value can be left explicitly
    untransformed by specifying #'[`IDENTITY`][8804] or `NIL` as the function:
    
    ```
    (funcall (values-> #'1+ nil #'symbol-name)
             '(7 :something :another))
    => (8 :SOMETHING "ANOTHER")
    ```


<a id="x-28JOURNAL-3AVALUES-3C--20FUNCTION-29"></a>
- [function] **VALUES\<-** *&REST FNS*

    The inverse of [`VALUES->`][7ec9], this returns a function suitable as
    the `REPLAY-VALUES` argument of [`JOURNALED`][6267]. It does pretty much what
    `VALUES->` does, but the function returned returns the transformed
    list as multiple values instead of as a list.
    
    ```
    (funcall (values<- #'1-) '(8 :something))
    => 7
    => :SOMETHING
    ```


<a id="x-28JOURNAL-3A-40JOURNAL-UTILITIES-20MGL-PAX-3ASECTION-29"></a>
### 6.4 Utilities

<a id="x-28JOURNAL-3ALIST-EVENTS-20FUNCTION-29"></a>
- [function] **LIST-EVENTS** *&OPTIONAL (JOURNAL (RECORD-JOURNAL))*

    Return a list of all the events in the journal designated by
    `JOURNAL`. Calls [`SYNC-JOURNAL`][b2ff] first to make sure that all writes are
    taken into account.

<a id="x-28JOURNAL-3AEVENTS-TO-FRAMES-20FUNCTION-29"></a>
- [function] **EVENTS-TO-FRAMES** *EVENTS*

    Convert a flat list of events, such as those returned by [`LIST-EVENTS`][0c1b],
    to a nested list representing the [frame][7df7]s. Each frame is a list of
    the form `(<in-event> <nested-frames>* <out-event>?)`. Like in
    [`PRINT-EVENTS`][f379], `EVENTS` may be a [`JOURNAL`][5082].
    
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
    an [`OUT-EVENT`][637d]) are included in the output.

<a id="x-28JOURNAL-3AEXPECTED-TYPE-20FUNCTION-29"></a>
- [function] **EXPECTED-TYPE** *TYPE*

    Return a function suitable as the `CONDITION` argument of [`JOURNALED`][6267],
    which returns the type of its single argument as a string if it is
    of `TYPE`, else `NIL`.

<a id="x-28JOURNAL-3A-40PRETTY-PRINTING-20MGL-PAX-3ASECTION-29"></a>
### 6.5 Pretty-printing

<a id="x-28JOURNAL-3APRINT-EVENTS-20FUNCTION-29"></a>
- [function] **PRINT-EVENTS** *EVENTS &KEY STREAM*

    Print `EVENTS` to `STREAM` as lists, starting a new line for each
    event and indenting them according to their nesting structure.
    `EVENTS` may be a sequence or a [`JOURNAL`][5082], in which case [`LIST-EVENTS`][0c1b] is
    called on it first.
    
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


<a id="x-28JOURNAL-3APPRINT-EVENTS-20FUNCTION-29"></a>
- [function] **PPRINT-EVENTS** *EVENTS &KEY STREAM (PRETTIFIER 'PRETTIFY-EVENT)*

    Like [`PRINT-EVENTS`][f379], but produces terser, more human readable
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
    events. The above output was produced with [`PRETTIFY-EVENT`][11b7]. For a
    description of `PRETTIFIER`'s arguments see `PRETTIFY-EVENT`.

<a id="x-28JOURNAL-3APRETTIFY-EVENT-20FUNCTION-29"></a>
- [function] **PRETTIFY-EVENT** *EVENT DEPTH STREAM*

    Write `EVENT` to `STREAM` in a somewhat human-friendly format.
    This is the function [`PPRINT-JOURNAL`][9150], [`PPRINT-EVENTS`][5833], and [Tracing][e03f] use
    by default. In addition to the basic example in `PPRINT-EVENTS`,
    [decoration][1d11] on events is printed before normal, indented output like
    this:
    
    ```
    (pprint-events '((:leaf "About to sleep" :time "19:57:00" :function "FOO")))
    ..
    .. 19:57:00 FOO: About to sleep
    ```
    
    `DEPTH` is the nesting level of the `EVENT`. Top-level events have depth
    0. `PRETTIFY-EVENT` prints indents the output after printing the
    decorations by 2 spaces per depth.

Instead of collecting events and then printing them, events can
be pretty-printed to a stream as they generated. This is
accomplished with [Pretty-printing journals][1496], discussed in detail later, in
the following way:

```
(let ((journal (make-pprint-journal)))
  (with-journaling (:record journal)
    (journaled (foo) "Hello")))
..
.. (FOO)
.. => "Hello"
```

Note that [Pretty-printing journals][1496] are not tied to [`WITH-JOURNALING`][6131] and are
most often used for [Logging][4e53] and [Tracing][e03f].

<a id="x-28JOURNAL-3A-40JOURNAL-ERROR-HANDLING-20MGL-PAX-3ASECTION-29"></a>
### 6.6 Error handling

<a id="x-28JOURNAL-3AJOURNALING-FAILURE-20CONDITION-29"></a>
- [condition] **JOURNALING-FAILURE** *SERIOUS-CONDITION*

    Signalled during the dynamic extent of
    [`WITH-JOURNALING`][6131] when an error threatens to leave the journaling
    mechanism in an inconsistent state. These include I/O errors
    encountered reading or writing journals by `WITH-JOURNALING`,
    [`JOURNALED`][6267], [`LOGGED`][23c4], [`WITH-REPLAY-FILTER`][0cce], [`SYNC-JOURNAL`][b2ff], and also
    [`STORAGE-CONDITION`][63d3]s, assertion failures, errors calling `JOURNALED`'s
    `VALUES` and `CONDITION` function arguments.
    Crucially, this does not apply to [non-local exit][3b76]s from other code,
    such as `JOURNALED` [block][06a7]s, whose error handling is largely
    unaltered (see [Out-events][48ef] and [Replay failures][2933]).
    
    In general, any [non-local exit][3b76] from critical parts of the code is
    turned into a `JOURNALING-FAILURE` to protect the integrity of the
    [`RECORD-JOURNAL`][3b63]. The condition that caused the unwinding is in
    [`JOURNALING-FAILURE-EMBEDDED-CONDITION`][9f90], or `NIL` if it was a pure
    [non-local exit][3b76] like [`THROW`][3b05]. This is a [`SERIOUS-CONDITION`][e91e], not to be
    handled within `WITH-JOURNALING`.
    
    After a `JOURNALING-FAILURE`, the journaling mechanism cannot be
    trusted anymore. The [`REPLAY-JOURNAL`][838b] might have failed a read and be
    out-of-sync. The `RECORD-JOURNAL` may have missing events (or even
    half-written events with [`FILE-JOURNAL`][8428]s without `SYNC`, see
    [Synchronization strategies][f532]), and further writes to it would risk
    replayability, which is equivalent to database corruption. Thus,
    upon signalling `JOURNALING-FAILURE`, [`JOURNAL-STATE`][03de] is set to
    
    - `:COMPLETED` if the journal is in state `:RECORDING` or `:LOGGING` and
      the transition to `:RECORDING` was reflected in storage,
    
    - else it is set to `:FAILED`.
    
    After a `JOURNALING-FAILURE`, any further attempt within the affected
    `WITH-JOURNALING` to use the critical machinery mentioned
    above (`JOURNALED`, `LOGGED`, etc) resignals the same journal failure
    condition. As a consequence, the record journal cannot be changed,
    and the only way to recover is to leave `WITH-JOURNALING`. This does
    not affect processing in other threads, which by design cannot write
    to the record journal.
    
    Note that in contrast with `JOURNALING-FAILURE` and [`REPLAY-FAILURE`][2e9b],
    which necessitate leaving `WITH-JOURNALING` to recover from, the other
    conditions – [`JOURNAL-ERROR`][0002], and [`STREAMLET-ERROR`][e6b2] – are subclasses of
    [`ERROR`][1895] as the their handling need not be so
    heavy-handed.

<a id="x-28JOURNAL-3AJOURNALING-FAILURE-EMBEDDED-CONDITION-20-28MGL-PAX-3AREADER-20JOURNAL-3AJOURNALING-FAILURE-29-29"></a>
- [reader] **JOURNALING-FAILURE-EMBEDDED-CONDITION** *JOURNALING-FAILURE (:EMBEDDED-CONDITION)*

<a id="x-28JOURNAL-3ARECORD-UNEXPECTED-OUTCOME-20CONDITION-29"></a>
- [condition] **RECORD-UNEXPECTED-OUTCOME**

    Signalled (with [`SIGNAL`][c8d1]: this is not an
    [`ERROR`][1895]) by [`JOURNALED`][6267] when a [`VERSIONED-EVENT`][4c2b] or an
    [`EXTERNAL-EVENT`][0e53] had an UNEXPECTED-OUTCOME while in [`JOURNAL-STATE`][03de]
    `:RECORDING`. Upon signalling this condition, `JOURNAL-STATE` is set to
    `:LOGGING`, thus no more events can be recorded that will affect
    replay of the journal being recorded. The event that triggered this
    condition is recorded in state `:LOGGING`, with its version
    downgraded. Since [Replay][041c] (except [invoked][4212]) is built on the
    assumption that control flow is deterministic, an unexpected outcome
    is significant because it makes this assumption to hold unlikely.
    
    Also see [`REPLAY-UNEXPECTED-OUTCOME`][6699].

<a id="x-28JOURNAL-3ADATA-EVENT-LOSSAGE-20CONDITION-29"></a>
- [condition] **DATA-EVENT-LOSSAGE** *JOURNALING-FAILURE*

    Signalled when a [data event][c015] is about to be recorded
    in [`JOURNAL-STATE`][03de] `:MISMATCHED` or `:LOGGING`. Since the data event will
    not be replayed that constitutes data loss.

<a id="x-28JOURNAL-3AJOURNAL-ERROR-20CONDITION-29"></a>
- [condition] **JOURNAL-ERROR** *ERROR*

    Signalled by [`WITH-JOURNALING`][6131], [`WITH-BUNDLE`][12a5] and by
    [`:LOG-RECORD`][a6ac]. It is also signalled by the low-level streamlet
    interface (see [Streamlets reference][f4d5]).

<a id="x-28JOURNAL-3AEND-OF-JOURNAL-20CONDITION-29"></a>
- [condition] **END-OF-JOURNAL** *JOURNAL-ERROR*

    This might be signalled by the replay mechanism if
    [`WITH-JOURNALING`][6131]'s `REPLAY-EOJ-ERROR-P` is true. Unlike
    [`REPLAY-FAILURE`][2e9b]s, this does not affect [`JOURNAL-STATE`][03de] of
    [`RECORD-JOURNAL`][3b63]. At a lower level, it is signalled by [`READ-EVENT`][adcf] upon
    reading past the end of the [`JOURNAL`][5082] if `EOJ-ERROR-P`.

<a id="x-28JOURNAL-3A-40LOGGING-20MGL-PAX-3ASECTION-29"></a>
## 7 Logging

Before we get into the details, here is a self-contained example
that demonstrates typical use.

```
(defvar *communication-log* nil)
(defvar *logic-log* nil)
(defvar *logic-log-level* 0)

(defun call-with-connection (port fn)
  (framed (call-with-connection :log-record *communication-log*
                                :args `(,port))
    (funcall fn)))

(defun fetch-data (key)
  (let ((value 42))
    (logged ((and (<= 1 *logic-log-level*) *logic-log*))
      "The value of ~S is ~S." key value)
    value))

(defun init-logging (&key (logic-log-level 1))
  (let* ((stream (open "/tmp/xxx.log"
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :append))
         (journal (make-pprint-journal
                   :stream (make-broadcast-stream
                            (make-synonym-stream '*standard-output*)
                            stream))))
    (setq *communication-log* journal)
    (setq *logic-log* journal)
    (setq *logic-log-level* logic-log-level)))

(init-logging)

(call-with-connection 8080 (lambda () (fetch-data :foo)))
..
.. (CALL-WITH-CONNECTION 8080)
..   The value of :FOO is 42.
.. => 42
=> 42

(setq *logic-log-level* 0)
(call-with-connection 8080 (lambda () (fetch-data :foo)))
..
.. (CALL-WITH-CONNECTION 8080)
.. => 42
=> 42

(ignore-errors
  (call-with-connection 8080 (lambda () (error "Something unexpected."))))
..
.. (CALL-WITH-CONNECTION 8080)
.. =E "SIMPLE-ERROR" "Something unexpected."
```

##### Default to muffling

Imagine a utility library called glib.

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

Let's send the logs to a [`PPRINT-JOURNAL`][9150]:

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

To log to a file:

```
(setq *glib-log* (make-pprint-journal
                  :stream (open "/tmp/glib.log"
                                :direction :output
                                :if-does-not-exist :create
                                :if-exists :append)))
```

##### Capturing logs in [`WITH-JOURNALING`][6131]'s [`RECORD-JOURNAL`][3b63]

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
messages are included by default in the `RECORD-JOURNAL`. In this
example, the special `*GLIB-LOG*` acts like a log category for all
the log messages of the glib library (currently one).

##### Rerouting a category

Next, we route `*GLIB-LOG*` to wherever `*APP-LOG*` is pointing by
binding `*GLIB-LOG*` *to the symbol* `*APP-LOG*` (see [`:LOG-RECORD`][a6ac]).

```
(defvar *app-log* nil)

(let ((*glib-log* '*app-log*))
  (setq *app-log* nil)
  (logged (*glib-log*) "This is not written anywhere.")
  (setq *app-log* (make-pprint-journal :pretty nil))
  (sl33p 0.01))
..
.. (:LEAF "Sleeping for 0.01s.")
```

Note how pretty-printing was turned off, and we see the [`LEAF-EVENT`][5cd1]
generated by [`LOGGED`][23c4] in its raw plist form.

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
(let ((*glib-log-level* 1)
      (*glib-log* (make-pprint-journal)))
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

`LOGGED` is for single messages. [`JOURNALED`][6267], or in this example [`FRAMED`][5d05],
can provide nested context:

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


<a id="x-28JOURNAL-3A-40CUSTOMIZING-LOGS-20MGL-PAX-3ASECTION-29"></a>
### 7.1 Customizing logs

Customizing the output format is possible if we don't necessarily
expect to be able to read the logs back programmatically. There is
an example in [Tracing][e03f], which is built on [Pretty-printing journals][1496].

Here, we discuss how to make logs more informative.

<a id="x-28JOURNAL-3A-40DECORATION-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **decoration**

    [`JOURNAL-LOG-DECORATOR`][8a5b] adds additional data to [`LOG-EVENT`][51ce]s as they
    are written to the journal. This data is called decoration, and it is
    to capture the context in which the event was triggered. See
    [`MAKE-LOG-DECORATOR`][e33e] for a typical example. Decorations, since they
    can be on `LOG-EVENT`s only, do not affect [Replay][041c]. Decorations are
    most often used with [Pretty-printing][47a7].

<a id="x-28JOURNAL-3AJOURNAL-LOG-DECORATOR-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3AJOURNAL-29-29"></a>
- [accessor] **JOURNAL-LOG-DECORATOR** *JOURNAL (:LOG-DECORATOR = NIL)*

    If non-NIL, this is a function to add [decoration][1d11]
    to [`LOG-EVENT`][51ce]s before they are written to a journal. The only
    allowed transformation is to *append* a plist to the event, which
    is a plist itself. The keys can be anything.

<a id="x-28JOURNAL-3AMAKE-LOG-DECORATOR-20FUNCTION-29"></a>
- [function] **MAKE-LOG-DECORATOR** *&KEY THREAD TIME REAL-TIME RUN-TIME*

    Return a function suitable as [`JOURNAL-LOG-DECORATOR`][8a5b] that may add
    the name of the thread, a timestamp, the internal real-time or
    run-time (both in seconds) to events. `THREAD`, `TIME`, `REAL-TIME` and
    `RUN-TIME` are [boolean-valued symbol][62678]s.
    
    ```
    (funcall (make-log-decorator :thread t :time t :real-time t :run-time t)
             (make-leaf-event :foo))
    => (:LEAF :FOO :TIME "2020-08-31T13:38:58.129178+02:00"
        :REAL-TIME 66328.82 :RUN-TIME 98.663 :THREAD "worker")
    ```


<a id="x-28JOURNAL-3A-40LOG-RECORD-20MGL-PAX-3ASECTION-29"></a>
### 7.2 `:LOG-RECORD`

[`WITH-JOURNALING`][6131] and [`WITH-BUNDLE`][12a5] control replaying and recording
within their dynamic extent, which is rather a necessity because
[Replay][041c] needs to read the events in the same order as the [`JOURNALED`][6267]
[block][06a7]s are being executed. However, [`LOG-EVENT`][51ce]s do not affect
replay, so we can allow more flexibility in routing them.

The `LOG-RECORD` argument of `JOURNALED` and [`LOGGED`][23c4] controls where
`LOG-EVENT`s are written both within `WITH-JOURNALING` and without. The
algorithm to determine the target journal is this:

1. If `LOG-RECORD` is `:RECORD`, then the [`RECORD-JOURNAL`][3b63] is returned.

2. If `LOG-RECORD` is `NIL`, then it is returned.

3. If `LOG-RECORD` is a [`JOURNAL`][5082], then it is returned.

4. If `LOG-RECORD` is a symbol (other than `NIL`), then the [`SYMBOL-VALUE`][920f]
   of that symbol is assigned to `LOG-RECORD`, and we go to step 1.

If the return value is `NIL`, then the event will not be written
anywhere, else it is written to the journal returned.

This is reminiscent of [`SYNONYM-STREAM`][9acb]s, also in that it is possible
end up in cycles in the resolution. For this reason, the algorithm
stop with a [`JOURNAL-ERROR`][0002] after 100 iterations.

##### Interactions

Events may be written to `LOG-RECORD` even without an enclosing
`WITH-JOURNALING`, and it does not affect the [`JOURNAL-STATE`][03de]. However,
it is a `JOURNAL-ERROR` to write to a `:COMPLETED` journal (see
`JOURNAL-STATE`).

When multiple threads log to the same journal, it is guaranteed that
individual events are written atomically, but frames from different
threads do not necessarily nest. To keep the log informative, the
name of thread may be added to the events as [decoration][1d11].

Also, see notes on thread [Safety][7bf3].

<a id="x-28JOURNAL-3A-40LOGGING-WITH-LEAVES-20MGL-PAX-3ASECTION-29"></a>
### 7.3 Logging with `LEAF-EVENT`s

<a id="x-28JOURNAL-3ALOGGED-20MGL-PAX-3AMACRO-29"></a>
- [macro] **LOGGED** *(&OPTIONAL (LOG-RECORD :RECORD)) FORMAT-CONTROL &REST FORMAT-ARGS*

    `LOGGED` creates a single [`LEAF-EVENT`][5cd1], whose name is the string
    constructed by [`FORMAT`][1f28]. For example:
    
    ```
    (with-journaling (:record t)
      (logged () "Hello, ~A." "world")
      (list-events))
    => ((:LEAF "Hello, world."))
    ```
    
    `LEAF-EVENT`s are [`LOG-EVENT`][51ce]s with no separate in- and out-events. They
    have an [`EVENT-NAME`][9f84] and no other properties. Use `LOGGED` for
    point-in-time textual log messages, and [`JOURNALED`][6267] with `VERSION`
    `NIL` (i.e. [`FRAMED`][5d05]) to provide context.
    
    Also, see [`:LOG-RECORD`][a6ac].

<a id="x-28JOURNAL-3A-40TRACING-20MGL-PAX-3ASECTION-29"></a>
## 8 Tracing

[`JTRACE`][18be] behaves similarly to [`CL:TRACE`][548d] but deals [non-local exit][3b76]s
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

In the above, [`*TRACE-JOURNAL*`][4118] was bound locally to keep the example
from wrecking the global default, but the same effect could be
achieved by [`SETF`][17b7]ing [`PPRINT-JOURNAL-PRETTIFIER`][853d],
[`PPRINT-JOURNAL-STREAM`][34a8] and [`JOURNAL-LOG-DECORATOR`][8a5b].

<a id="x-28JOURNAL-3AJTRACE-20MGL-PAX-3AMACRO-29"></a>
- [macro] **JTRACE** *&REST NAMES*

    Like [`CL:TRACE`][548d], `JTRACE` takes a list of symbols. When functions
    denoted by those `NAMES` are invoked, their names, arguments and
    outcomes are printed in human readable form to [`*TRACE-OUTPUT*`][3ca4]. These
    values may not be [readable][768f], `JTRACE` does not care.
    
    The format of the output is the same as that of [`PPRINT-EVENTS`][5833].
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
    [`SYMBOL-FUNCTION`][3caf]. This solution loses the tracing encapsulation when
    the function is recompiled. On these platforms, `(JTRACE)` also
    retraces all functions that should be traced but aren't.
    
    The main advantage of `JTRACE` over `CL:TRACE` is the ability to trace
    errors, not just normal return values. As it is built on [`JOURNALED`][6267],
    it can also detect – somewhat heuristically – [`THROW`][3b05]s and similar.

<a id="x-28JOURNAL-3AJUNTRACE-20MGL-PAX-3AMACRO-29"></a>
- [macro] **JUNTRACE** *&REST NAMES*

    Like [`CL:UNTRACE`][4823], `JUNTRACE` makes it so that the global functions
    denoted by the symbols `NAMES` are no longer traced by [`JTRACE`][18be]. When
    invoked with no arguments, it untraces all traced functions.

<a id="x-28JOURNAL-3A-2ATRACE-PRETTY-2A-20VARIABLE-29"></a>
- [variable] **\*TRACE-PRETTY\*** *T*

    If `*TRACE-PRETTY*` is true, then [`JTRACE`][18be] produces output like
    [`PPRINT-EVENTS`][5833], else it's like [`PRINT-EVENTS`][f379].

<a id="x-28JOURNAL-3A-2ATRACE-THREAD-2A-20VARIABLE-29"></a>
- [variable] **\*TRACE-THREAD\*** *NIL*

    Controls whether to decorate the trace with the name of the
    originating thread. See [`MAKE-LOG-DECORATOR`][e33e].

<a id="x-28JOURNAL-3A-2ATRACE-TIME-2A-20VARIABLE-29"></a>
- [variable] **\*TRACE-TIME\*** *NIL*

    Controls whether to decorate the trace with a timestamp. See
    [`MAKE-LOG-DECORATOR`][e33e].

<a id="x-28JOURNAL-3A-2ATRACE-REAL-TIME-2A-20VARIABLE-29"></a>
- [variable] **\*TRACE-REAL-TIME\*** *NIL*

    Controls whether to decorate the trace with the internal real-time.
    See [`MAKE-LOG-DECORATOR`][e33e].

<a id="x-28JOURNAL-3A-2ATRACE-RUN-TIME-2A-20VARIABLE-29"></a>
- [variable] **\*TRACE-RUN-TIME\*** *NIL*

    Controls whether to decorate the trace with the internal run-time.
    See [`MAKE-LOG-DECORATOR`][e33e].

<a id="x-28JOURNAL-3A-2ATRACE-JOURNAL-2A-20VARIABLE-29"></a>
- [variable] **\*TRACE-JOURNAL\*** *#\<PPRINT-JOURNAL :NEW 1\>*

    The [`JOURNAL`][5082] where [`JTRACE`][18be] writes [`LOG-EVENT`][51ce]s. By default, it is a
    [`PPRINT-JOURNAL`][9150] that sets up a [`SYNONYM-STREAM`][9acb] to [`*TRACE-OUTPUT*`][3ca4] and
    sends its output there. It pays attention to [`*TRACE-PRETTY*`][825c], and its
    log decorator is affected by [`*TRACE-TIME*`][2765] and [`*TRACE-THREAD*`][9a42].
    However, by changing [`JOURNAL-LOG-DECORATOR`][8a5b] and
    [`PPRINT-JOURNAL-PRETTIFIER`][853d], content and output can be customized.

<a id="x-28JOURNAL-3A-40JOURNAL-SLIME-INTEGRATION-20MGL-PAX-3ASECTION-29"></a>
### 8.1 Slime integration

[Slime](https://common-lisp.net/project/slime/), by default, binds
`C-c C-t` to toggling [`CL:TRACE`][548d]. To integrate [`JTRACE`][18be] into Slime, add
the following ELisp snippet to your Emacs initialization file or
load `src/journal.el`:

<a id="x-28JOURNAL-3AJOURNAL-2EEL-20-28MGL-PAX-3AINCLUDE-20-23P-22-2Fhome-2Fmelisgl-2Fown-2Fjournal-2Fsrc-2Fjournal-2Eel-22-20-3AHEADER-NL-20-22-60-60-60elisp-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29"></a>
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

Since [`JTRACE`][18be] lacks some features of [`CL:TRACE`][548d], most notably that of
tracing non-global functions, it is assigned a separate binding,
`C-c C-j`.

<a id="x-28JOURNAL-3A-40REPLAY-20MGL-PAX-3ASECTION-29"></a>
## 9 Replay

During replay, code is executed normally with special rules for
[block][06a7]s. There are two modes for dealing with blocks: replaying the
code and replaying the outcome. When code is replayed, upon entering
and leaving a block, the events generated are matched to events read
from the journal being replayed. If the events don't match,
[`REPLAY-FAILURE`][2e9b] is signalled, which marks the record journal as having
failed the replay. This is intended to make sure that the state of
the program during the replay matches the state at the time of
recording. In the other mode, when the outcome is replayed, a block
may not be executed at all, but its recorded outcome is
reproduced (i.e. the recorded return values are simply returned).

Replay can be only be initiated with [`WITH-JOURNALING`][6131] (or its close
kin [`WITH-BUNDLE`][12a5]). After the per-event processing described below,
when `WITH-JOURNALING` finishes, it might signal [`REPLAY-INCOMPLETE`][e442] if
there are unprocessed non-log events left in the replay journal.

Replay is deemed successful or failed depending on whether all
events are replayed from the replay journal without a
`REPLAY-FAILURE`. A journal that records events from a successful
replay can be used in place of the journal that was replayed, and so
on. The logic of replacing journals with their successful replays is
automated by [Bundles][260d]. `WITH-JOURNALING` does not allow replay from
journals that were failed replays themselves. The mechanism, in
terms of which tracking success and failure of replays is
implemented, revolves around [`JOURNAL-STATE`][03de] and
[`EVENT-VERSION`][9ed3]s, which we discuss next.

<a id="x-28JOURNAL-3AJOURNAL-STATE-20TYPE-29"></a>
- [type] **JOURNAL-STATE**

    [`JOURNAL`][5082]'s state with respect to replay is updated during
    [`WITH-JOURNALING`][6131]. The possible states are:
    
    - **`:NEW`**: This journal was just created but never recorded to.
    
    - **`:REPLAYING`**: Replaying events has started, some events may have
      been replayed successfully, but there are more non-log events to
      replay.
    
    - **`:MISMATCHED`**: There was a [`REPLAY-FAILURE`][2e9b]. In this state,
      [`VERSIONED-EVENT`][4c2b]s generated are downgraded to [`LOG-EVENT`][51ce]s,
      [`EXTERNAL-EVENT`][0e53]s and [invoked][4212] trigger [`DATA-EVENT-LOSSAGE`][4f2b].
    
    - **`:RECORDING`**: All events from the replay journal were
      successfully replayed, and now new events are being recorded
      without being matched to the replay journal.
    
    - **`:LOGGING`**: There was a [`RECORD-UNEXPECTED-OUTCOME`][8548]. In this
      state, `VERSIONED-EVENT`s generated are downgraded to `LOG-EVENT`s,
      `EXTERNAL-EVENT`s and [invoked][4212] trigger `DATA-EVENT-LOSSAGE`.
    
    - **`:FAILED`**: The journal is to be discarded. It encountered a
      [`JOURNALING-FAILURE`][3956] or a `REPLAY-FAILURE` without completing the
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
    
    `:NEW` is the starting state. It is a [`JOURNAL-ERROR`][0002] to attempt to
    write to journals in `:COMPLETED`. Note that once in `:RECORDING`, the
    only possible terminal state is `:COMPLETED`.

<a id="x-28JOURNAL-3A-40JOURNALED-FOR-REPLAY-20MGL-PAX-3ASECTION-29"></a>
### 9.1 Journaled for replay

The following arguments of [`JOURNALED`][6267] control behaviour under replay.

- `VERSION`: see [`EVENT-VERSION`][9ed3] below.

- `INSERTABLE` controls whether [`VERSIONED-EVENT`][4c2b]s and [`EXTERNAL-EVENT`][0e53]s
  may be replayed with the *insert* replay strategy (see
  [The replay strategy][a8a7]). Does not affect [`LOG-EVENT`][51ce]s, which are
  always \_insert\_ed. Note that inserting `EXTERNAL-EVENT`s while
  `:REPLAYING` is often not meaningful (e.g. asking the user for input
  may lead to a [`REPLAY-FAILURE`][2e9b]). See [`PEEK-REPLAY-EVENT`][eddd] for an
  example on how to properly insert these kinds of `EXTERNAL-EVENT`s.

- `REPLAY-VALUES`, a function or `NIL`, may be called with [`EVENT-OUTCOME`][c290]
  when replaying and `:VERSION` `:INFINITY`. `NIL` is equivalent to
  [`VALUES-LIST`][b0f7]. See [`VALUES<-`][f17d] for an example.

- `REPLAY-CONDITION`, a function or `NIL`, may be called with
  `EVENT-OUTCOME` (the return value of the function provided as
  `:CONDITION`) when replaying and `:VERSION` is `:INFINITY`. `NIL` is
  equivalent to the [`ERROR`][ec01] function. Replaying conditions is
  cumbersome and best avoided.


<a id="x-28JOURNAL-3A-2AFORCE-INSERTABLE-2A-20VARIABLE-29"></a>
- [variable] **\*FORCE-INSERTABLE\*** *NIL*

    The default value of the `INSERTABLE` argument of [`JOURNALED`][6267] for
    [`VERSIONED-EVENT`][4c2b]s. Binding this to `T` allows en-masse structural
    upgrades in combination with [`WITH-REPLAY-FILTER`][0cce]. Does not affect
    [`EXTERNAL-EVENT`][0e53]s. See [Upgrades and replay][750a].

<a id="x-28JOURNAL-3AEVENT-VERSION-20TYPE-29"></a>
- [type] **EVENT-VERSION**

    An event's version is either `NIL`, a positive [`FIXNUM`][ceb9], or `:INFINITY`,
    which correspond to [`LOG-EVENT`][51ce]s, [`VERSIONED-EVENT`][4c2b]s, and
    [`EXTERNAL-EVENT`][0e53]s, respectively, and have an increasingly strict
    behaviour with regards to [Replay][041c]. All [`EVENT`][a394]s have versions. The
    versions of the in- and out-events belonging to the same [frame][7df7] are
    the same.

<a id="x-28JOURNAL-3ALOG-EVENT-20TYPE-29"></a>
- [type] **LOG-EVENT**

    Events with [`EVENT-VERSION`][9ed3] `NIL` called log events. During [Replay][041c],
    they are never matched to events from the replay journal, and log
    events in the replay do not affect events being recorded either.
    These properties allow log events to be recorded in arbitrary
    journals with [`JOURNALED`][6267]'s `LOG-RECORD` argument. The convenience macro
    [`FRAMED`][5d05] is creating frames of log-events, while the [`LOGGED`][23c4] generates
    a log-event that's a [`LEAF-EVENT`][5cd1].

<a id="x-28JOURNAL-3AVERSIONED-EVENT-20TYPE-29"></a>
- [type] **VERSIONED-EVENT**

    Events with a positive integer [`EVENT-VERSION`][9ed3] are called
    versioned events. In [Replay][041c], they undergo consistency checks unlike
    [`LOG-EVENT`][51ce]s, but the rules for them are less strict than for
    [`EXTERNAL-EVENT`][0e53]s. In particular, higher versions are always
    considered compatible with lower versions, they become an *upgrade*
    in terms of the [The replay strategy][a8a7], and versioned events can be
    inserted into the record without a corresponding [replay event][6525] with
    [`JOURNALED`][6267]'s `INSERTABLE`.
    
    If a `VERSIONED-EVENT` has an [unexpected outcome][d2c1],
    [`RECORD-UNEXPECTED-OUTCOME`][8548] is signalled.

<a id="x-28JOURNAL-3AEXTERNAL-EVENT-20TYPE-29"></a>
- [type] **EXTERNAL-EVENT**

    Events with [`EVENT-VERSION`][9ed3] `:INFINITY` are called external events.
    They are like [`VERSIONED-EVENT`][4c2b]s whose version was bumped all the way
    to infinity, which rules out easy, non-matching upgrades. Also, they
    are never inserted to the record without a matching replay
    event (see [The replay strategy][a8a7]).
    
    In return for these restrictions, external events can be replayed
    without running the corresponding [block][06a7] (see
    [Replaying the outcome][7991]). This allows their out-event variety, called
    [data event][c015]s, to be non-deterministic. Data events play a crucial
    role in [Persistence][37c4].
    
    If an `EXTERNAL-EVENT` has an [unexpected outcome][d2c1],
    [`RECORD-UNEXPECTED-OUTCOME`][8548] is signalled.

Built on top of [`JOURNALED`][6267], the macros below record a pair of
[In-events][186b] and [Out-events][48ef] but differ in how they are replayed and
the requirements on their [block][06a7]s. The following table names the
type of [`EVENT`][a394] produced (`Event`), how [In-events][186b] are
replayed (`In-e.`), whether the block is always run (`Run`), how
[Out-events][48ef] are replayed (`Out-e.`), whether the block must be
deterministic (`Det`) or side-effect free (`SEF`).

    |          | Event     | In-e.  | Run | Out-e. | Det | SEF |
    |----------+-----------+--------+-----+--------+-----+-----|
    | FRAMED   | log       | skip   | y   | skip   | n   | n   |
    | CHECKED  | versioned | match  | y   | match  | y   | n   |
    | REPLAYED | external  | match  | n   | replay | n   | y   |
    | INVOKED  | versioned | replay | y   | match  | y   | n   |

Note that the replay-replay combination is not implemented because
there is nowhere to return values from replay-triggered functions.

<a id="x-28JOURNAL-3AFRAMED-20MGL-PAX-3AMACRO-29"></a>
- [macro] **FRAMED** *(NAME &KEY LOG-RECORD ARGS VALUES CONDITION) &BODY BODY*

    A wrapper around [`JOURNALED`][6267] to produce [frame][7df7]s of [`LOG-EVENT`][51ce]s. That
    is, `VERSION` is always `NIL`, and some irrelevant arguments are
    omitted. The related [`LOGGED`][23c4] creates a single [`LEAF-EVENT`][5cd1].
    
    With `FRAMED`, `BODY` is always run and no [`REPLAY-FAILURE`][2e9b]s are
    triggered. `BODY` is not required to be deterministic, and it may have
    side-effects.

<a id="x-28JOURNAL-3ACHECKED-20MGL-PAX-3AMACRO-29"></a>
- [macro] **CHECKED** *(NAME &KEY (VERSION 1) ARGS VALUES CONDITION INSERTABLE) &BODY BODY*

    A wrapper around [`JOURNALED`][6267] to produce [frame][7df7]s of [`VERSIONED-EVENT`][4c2b]s.
    `VERSION` defaults to 1. `CHECKED` is for ensuring that supposedly
    deterministic processing does not veer off the replay.
    
    With `CHECKED`, `BODY` – which must be deterministic – is always run and
    [`REPLAY-FAILURE`][2e9b]s are triggered when the events generated do not match
    the events in the replay journal. `BODY` may have side-effects.
    
    For further discussion of determinism, see [`REPLAYED`][c2b8].

<a id="x-28JOURNAL-3AREPLAYED-20MGL-PAX-3AMACRO-29"></a>
- [macro] **REPLAYED** *(NAME &KEY ARGS VALUES CONDITION INSERTABLE REPLAY-VALUES REPLAY-CONDITION) &BODY BODY*

    A wrapper around [`JOURNALED`][6267] to produce [frame][7df7]s of [`EXTERNAL-EVENT`][0e53]s.
    `VERSION` is `:INFINITY`. `REPLAYED` is for primarily for marking and
    isolating non-deterministic processing.
    
    With `REPLAYED`, the [`IN-EVENT`][1729] is checked for consistency with the
    replay (as with [`CHECKED`][e95a]), but `BODY` is not run (assuming it has a
    recorded [expected outcome][4657]), and the outcome in the [`OUT-EVENT`][637d] is
    reproduced (see [Replaying the outcome][7991]). For this scheme to work,
    `REPLAYED` requires its `BODY` to be side-effect free, but it may be
    non-deterministic.

<a id="x-28JOURNAL-3A-40INVOKED-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **invoked**

    Invoked refers to functions and blocks defined by [`DEFINE-INVOKED`][3153] or
    [`FLET-INVOKED`][6ab5]. Invoked frames may be recorded in response to
    asynchronous events, and at replay the presence of its in-event
    triggers the execution of the function associated with the name of
    the event.
    
    On the one hand, [`FRAMED`][5d05], [`CHECKED`][e95a], [`REPLAYED`][c2b8] or plain [`JOURNALED`][6267] have
    [In-events][186b] that are always predictable from the code and the
    preceding events. The control flow – on the level of recorded frames
    – is deterministic in this sense. On the other hand, Invoked encodes
    in its [`IN-EVENT`][1729] what function to call next, introducing
    non-deterministic control flow.
    
    By letting events choose the code to run, Invoked resembles typical
    [Event Sourcing][event-sourcing] frameworks. When Invoked is used
    exclusively, the journal becomes a sequence of events. In contrast,
    `JOURNALED` and its wrappers put code first, and the journal will be a
    projection of the call tree.

<a id="x-28JOURNAL-3ADEFINE-INVOKED-20MGL-PAX-3AMACRO-29"></a>
- [macro] **DEFINE-INVOKED** *FUNCTION-NAME ARGS (NAME &KEY (VERSION 1) INSERTABLE) &BODY BODY*

    `DEFINE-INVOKED` is intended for recording asynchronous function
    invocations like event or signal handlers. It defines a function
    that records [`VERSIONED-EVENT`][4c2b]s with `ARGS` set to the actual arguments.
    At replay, it is invoked whenever the recorded [`IN-EVENT`][1729] becomes the
    [replay event][6525].
    
    [`DEFUN`][9717] and [`CHECKED`][e95a] rolled into one, `DEFINE-INVOKED` defines a
    top-level function with `FUNCTION-NAME` and `ARGS` (only simple
    positional arguments are allowed) and wraps `CHECKED` with `NAME`, the
    same `ARGS` and `INSERTABLE` around `BODY`. Whenever an `IN-EVENT` becomes
    the [replay event][6525], and it has a `DEFINE-INVOKED` defined with the name
    of the event, `FUNCTION-NAME` is invoked with [`EVENT-ARGS`][3335].
    
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
    
    The above can be alternatively implemented with [`REPLAYED`][c2b8] explicitly
    encapsulating the non-determinism:
    
    ```
    (let ((x (replayed (choose) (random 2))))
      (if (zerop x)
          (checked (foo :args `(,x))
            (setq *state* (1+ x)))
          (checked (bar :args `(,x))
            (setq *state* (+ 2 x)))))
    ```


<a id="x-28JOURNAL-3AFLET-INVOKED-20MGL-PAX-3AMACRO-29"></a>
- [macro] **FLET-INVOKED** *DEFINITIONS &BODY BODY*

    Like [`DEFINE-INVOKED`][3153], but with [`FLET`][7ef8] instead of [`DEFUN`][9717]. The event
    name and the function are associated in the dynamic extent of `BODY`.
    [`WITH-JOURNALING`][6131] does not change the bindings. The example in
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


<a id="x-28JOURNAL-3A-40BUNDLES-20MGL-PAX-3ASECTION-29"></a>
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

With [`FILE-JOURNAL`][8428]s, the motivating example above would be even more
complicated, but [`FILE-BUNDLE`][18955]s work the same way as
[`IN-MEMORY-BUNDLE`][bacd]s.

<a id="x-28JOURNAL-3AWITH-BUNDLE-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-BUNDLE** *(BUNDLE) &BODY BODY*

    This is like [`WITH-JOURNALING`][6131] where the [`REPLAY-JOURNAL`][838b] is the last
    successfully completed one in `BUNDLE`, and the [`RECORD-JOURNAL`][3b63] is a
    new one created in `BUNDLE`. When `WITH-BUNDLE` finishes, the record
    journal is in [`JOURNAL-STATE`][03de] `:FAILED` or `:COMPLETED`.
    
    To avoid accumulating useless data, the new record is immediately
    deleted when `WITH-BUNDLE` finishes if it has not diverged from the
    replay journal (see [`JOURNAL-DIVERGENT-P`][f224]). Because `:FAILED` journals
    are always divergent in this sense, they are deleted instead based
    on whether there is already a previous failed journal in the bundle
    and the new record is identical to that journal (see
    [`IDENTICAL-JOURNALS-P`][4a00]).
    
    It is a [`JOURNAL-ERROR`][0002] to have concurrent or nested `WITH-BUNDLE`s on
    the same bundle.

<a id="x-28JOURNAL-3A-40THE-REPLAY-STRATEGY-20MGL-PAX-3ASECTION-29"></a>
### 9.3 The replay strategy

The replay process for both [In-events][186b] and [Out-events][48ef] starts by
determining how the generated event (the *new* event from now on)
shall be replayed. Roughly, the decision is based on the `NAME` and
`VERSION` of the new event and the [replay event][6525] (the next event to be
read from the replay). There are four possible strategies:

- **match**: A new in-event must match the replay event in its `ARGS`.
  See [Matching in-events][3c21] for details. A new out-event must match
  the replay event's `EXIT` and `OUTCOME`, see [Matching out-events][7f9d].

- **upgrade**: The new event is not matched to any replay event, but
  an event is consumed from the replay journal. This happens if the
  next new event has the same name as the replay event, but its
  version is higher.

- **insert**: The new event is not matched to any replay event, and
  no events are consumed from the replay journal, which may be
  empty. This is always the case for new [`LOG-EVENT`][51ce]s and when there
  are no more events to read from the replay journal (unless
  `REPLAY-EOJ-ERROR-P`). For [`VERSIONED-EVENT`][4c2b]s, it is affected by
  setting [`JOURNALED`][6267]'s `INSERTABLE` to true (see
  [Journaled for replay][d700]).

    The out-event's strategy is always *insert* if the strategy for
    the corresponding in-event was *insert*.

- Also, [`END-OF-JOURNAL`][3cdb], [`REPLAY-NAME-MISMATCH`][6710] and
  [`REPLAY-VERSION-DOWNGRADE`][0fdb] may be signalled. See the algorithm below
  details.

The strategy is determined by the following algorithm, invoked
whenever an event is generated by a journaled [block][06a7]:

1. Log events are not matched to the replay. If the new event is a
   log event or a [`REPLAY-FAILURE`][2e9b] has been signalled before (i.e. the
   record journal's [`JOURNAL-STATE`][03de] is `:MISMATCHED`), then **insert**
   is returned.

2. Else, log events to be read in the replay journal are skipped,
   and the next unread, non-log event is peeked at (without
   advancing the replay journal).

    - **end of replay**: If there are no replay events left, then:

        - If `REPLAY-EOJ-ERROR-P` is `NIL` in [`WITH-JOURNALING`][6131] (the
          default), **insert** is returned.

        - If `REPLAY-EOJ-ERROR-P` is true, then **`END-OF-JOURNAL`**
          is signalled.

    - **mismatched name**: Else, if the next unread replay event's
      name is not [`EQUAL`][96d0] to the name of the new event, then:

        - For `VERSIONED-EVENT`s, **`REPLAY-NAME-MISMATCH`** is
          signalled if `INSERTABLE` is `NIL`, else **insert** is
          returned.

        - For [`EXTERNAL-EVENT`][0e53]s, **`REPLAY-NAME-MISMATCH`** is
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

Version matching (`match-version` above) is based on which event has
a higher version:

     | replay event    | =     | new event |
     |-----------------+-------+-----------|
     | downgrade-error | match | upgrade   |


<a id="x-28JOURNAL-3A-40REPLAY-EVENT-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **replay event**

    The replay event is the next event to be read from [`REPLAY-JOURNAL`][838b]
    which is not to be skipped. There may be no replay event if there
    are no more unread events in the replay journal.
    
    An event in the replay journal is skipped if it is a [`LOG-EVENT`][51ce] or
    there is a [`WITH-REPLAY-FILTER`][0cce] with a matching `:SKIP`. If `:SKIP` is in
    effect, the replay event may be indeterminate.
    
    Events from the replay journal are read when they are `:MATCH`ed or
    `:UPGRADE`d (see [The replay strategy][a8a7]), when nested events are
    echoed while [Replaying the outcome][7991], or when there is an [invoked][4212]
    defined with the same name as the replay event.
    
    The replay event is available via [`PEEK-REPLAY-EVENT`][eddd].

<a id="x-28JOURNAL-3A-40MATCHING-IN-EVENTS-20MGL-PAX-3ASECTION-29"></a>
### 9.4 Matching in-events

If the replay strategy is *match*, then, for in-events, the
matching process continues like this:

- If the [`EVENT-ARGS`][3335] are not [`EQUAL`][96d0], then **[`REPLAY-ARGS-MISMATCH`][1256]**
  signalled.

- At this point, two things might happen:

    - For [`VERSIONED-EVENT`][4c2b]s, the [block][06a7] will be executed as normal
      and its outcome will be matched to the [replay event][6525] (see
      [Matching out-events][7f9d]).

    - For [`EXTERNAL-EVENT`][0e53]s, the corresponding replay [`OUT-EVENT`][637d] is
      looked at. If there is one, meaning that the frame finished
      with an [expected outcome][4657], then its outcome will be
      replayed (see [Replaying the outcome][7991]). If the `OUT-EVENT` is
      missing, then `EXTERNAL-EVENT`s behave like `VERSIONED-EVENT`s,
      and the [block][06a7] is executed.


<a id="x-28JOURNAL-3A-40REPLAYING-THE-OUTCOME-20MGL-PAX-3ASECTION-29"></a>
#### 9.4.1 Replaying the outcome

So, if an in-event is triggered that matches the replay,
`EVENT-VERSION`([`0`][1f5f] [`1`][9ed3]) is `:INFINITY`, then normal execution is altered in the
following manner:

- The journaled [block][06a7] is not executed.

- To keep execution and the replay journal in sync, events of frames
  nested in the current one are skipped over in the replay journal.

- All events (including [`LOG-EVENT`][51ce]s) skipped over are echoed to the
  record journal. This serves to keep a trail of what happened
  during the original recording. Note that functions corresponding
  to [invoked][4212] frames are called when their [`IN-EVENT`][1729] is skipped over.

- The out-event corresponding to the in-event being processed is
  then read from the replay journal and is recorded again (to allow
  recording to function properly).

To be able to reproduce the outcome in the replay journal, some
assistance may be required from `REPLAY-VALUES` and `REPLAY-CONDITION`:

- If the [replay event][6525] has a normal return (i.e. `EVENT-EXIT`([`0`][c04d] [`1`][812a]) `:VALUES`),
  then the recorded return values (in [`EVENT-OUTCOME`][c290]) are returned
  immediately as in `(VALUES-LIST (EVENT-OUTCOME REPLAY-EVENT))`. If
  `REPLAY-VALUES` is specified, it is called instead of [`VALUES-LIST`][b0f7].
  See [Working with unreadable values][b354] for an example.

- Similarly, if the replay event has unwound with an expected
  condition (has `EVENT-EXIT` `:CONDITION`), then the recorded
  condition (in `EVENT-OUTCOME`) is signalled as
  IN `(ERROR (EVENT-OUTCOME REPLAY-EVENT))`. If `REPLAY-CONDITION` is
  specified, it is called instead of `ERROR`([`0`][1895] [`1`][ec01]). `REPLAY-CONDITION` must
  not return normally, and it's a [`JOURNAL-ERROR`][0002] if it does.

[`WITH-REPLAY-FILTER`][0cce]'s `NO-REPLAY-OUTCOME` can selectively turn off
replaying the outcome. See [Testing on multiple levels][9376], for an
example.

<a id="x-28JOURNAL-3A-40MATCHING-OUT-EVENTS-20MGL-PAX-3ASECTION-29"></a>
### 9.5 Matching out-events

If there were no [Replay failures][2933] during the matching of the
[`IN-EVENT`][1729], and the conditions for [Replaying the outcome][7991] were not
met, then the [block][06a7] is executed. When the outcome of the block is
determined, an [`OUT-EVENT`][637d] is triggered and is matched to the replay
journal. The matching of out-events starts out as in
[The replay strategy][a8a7] with checks for [`EVENT-NAME`][9f84] and
[`EVENT-VERSION`][1f5f].

If the replay strategy is *insert* or *upgrade*, then the out-event
is written to [`RECORD-JOURNAL`][3b63], consuming an event with a matching
name from the [`REPLAY-JOURNAL`][838b] in the latter case. If the strategy is
*match*, then:

- If the new event has an [unexpected outcome][d2c1], then
  **[`REPLAY-UNEXPECTED-OUTCOME`][6699]** is signalled. Note that the replay
  event always has an [expected outcome][4657] due to the handling of
  [`RECORD-UNEXPECTED-OUTCOME`][8548].

- If the new event has an [expected outcome][4657], then unless the new and
  [replay event][6525]'s `EVENT-EXIT`([`0`][c04d] [`1`][812a])s are [`EQ`][a1d4] and their [`EVENT-OUTCOME`][c290]s are
  [`EQUAL`][96d0], **[`REPLAY-OUTCOME-MISMATCH`][bbef]** is signalled.

- Else, the replay event is consumed and the new event is written
  the `RECORD-JOURNAL`.

Note that [The replay strategy][a8a7] for the in-event and the out-event of
the same [frame][7df7] may differ if the corresponding out-event is not
present in `REPLAY-JOURNAL`, which may be the case when the recording
process failed hard without unwinding properly, or when an
[unexpected outcome][d2c1] triggered the transition to [`JOURNAL-STATE`][03de]
`:LOGGING`.

<a id="x-28JOURNAL-3A-40REPLAY-FAILURES-20MGL-PAX-3ASECTION-29"></a>
### 9.6 Replay failures

<a id="x-28JOURNAL-3AREPLAY-FAILURE-20CONDITION-29"></a>
- [condition] **REPLAY-FAILURE** *SERIOUS-CONDITION*

    A abstract superclass (never itself signalled) for
    all kinds of mismatches between the events produced and the replay
    journal. Signalled only in [`JOURNAL-STATE`][03de] `:REPLAYING` and only once
    per [`WITH-JOURNALING`][6131]. If a `REPLAY-FAILURE` is signalled for an [`EVENT`][a394],
    then the event will be recorded, but [`RECORD-JOURNAL`][3b63] will transition
    to `JOURNAL-STATE` `:MISMATCHED`. Like [`JOURNALING-FAILURE`][3956], this is a
    serious condition because it is to be handled outside the enclosing
    `WITH-JOURNALING`. If a `REPLAY-FAILURE` were to be handled inside the
    `WITH-JOURNALING`, keep in mind that in `:MISMATCHED`, replay always
    uses the *insert* replay strategy (see [The replay strategy][a8a7]).

<a id="x-28JOURNAL-3AREPLAY-FAILURE-NEW-EVENT-20-28MGL-PAX-3AREADER-20JOURNAL-3AREPLAY-FAILURE-29-29"></a>
- [reader] **REPLAY-FAILURE-NEW-EVENT** *REPLAY-FAILURE (:NEW-EVENT)*

<a id="x-28JOURNAL-3AREPLAY-FAILURE-REPLAY-EVENT-20-28MGL-PAX-3AREADER-20JOURNAL-3AREPLAY-FAILURE-29-29"></a>
- [reader] **REPLAY-FAILURE-REPLAY-EVENT** *REPLAY-FAILURE (:REPLAY-EVENT)*

<a id="x-28JOURNAL-3AREPLAY-FAILURE-REPLAY-JOURNAL-20-28MGL-PAX-3AREADER-20JOURNAL-3AREPLAY-FAILURE-29-29"></a>
- [reader] **REPLAY-FAILURE-REPLAY-JOURNAL** *REPLAY-FAILURE (= '(REPLAY-JOURNAL))*

<a id="x-28JOURNAL-3AREPLAY-NAME-MISMATCH-20CONDITION-29"></a>
- [condition] **REPLAY-NAME-MISMATCH** *REPLAY-FAILURE*

    Signalled when the new event's and [replay event][6525]'s
    [`EVENT-NAME`][9f84] are not [`EQUAL`][96d0]. The [`REPLAY-FORCE-INSERT`][92aa],
    [`REPLAY-FORCE-UPGRADE`][10c8] restarts are provided.

<a id="x-28JOURNAL-3AREPLAY-VERSION-DOWNGRADE-20CONDITION-29"></a>
- [condition] **REPLAY-VERSION-DOWNGRADE** *REPLAY-FAILURE*

    Signalled when the new event and the [replay event][6525]
    have the same [`EVENT-NAME`][9f84], but the new event has a lower version. The
    [`REPLAY-FORCE-UPGRADE`][10c8] restart is provided.

<a id="x-28JOURNAL-3AREPLAY-ARGS-MISMATCH-20CONDITION-29"></a>
- [condition] **REPLAY-ARGS-MISMATCH** *REPLAY-FAILURE*

    Signalled when the new event's and [replay event][6525]'s
    [`EVENT-ARGS`][3335] are not [`EQUAL`][96d0]. The [`REPLAY-FORCE-UPGRADE`][10c8] restart is
    provided.

<a id="x-28JOURNAL-3AREPLAY-OUTCOME-MISMATCH-20CONDITION-29"></a>
- [condition] **REPLAY-OUTCOME-MISMATCH** *REPLAY-FAILURE*

    Signalled when the new event's and [replay event][6525]'s
    `EVENT-EXIT`([`0`][c04d] [`1`][812a]) and/or [`EVENT-OUTCOME`][c290] are not [`EQUAL`][96d0]. The
    [`REPLAY-FORCE-UPGRADE`][10c8] restart is provided.

<a id="x-28JOURNAL-3AREPLAY-UNEXPECTED-OUTCOME-20CONDITION-29"></a>
- [condition] **REPLAY-UNEXPECTED-OUTCOME** *REPLAY-FAILURE*

    Signalled when the new event has an
    [unexpected outcome][d2c1]. Note that the [replay event][6525] always has an
    [expected outcome][4657] due to the logic of [`RECORD-UNEXPECTED-OUTCOME`][8548]. No
    restarts are provided.

<a id="x-28JOURNAL-3AREPLAY-INCOMPLETE-20CONDITION-29"></a>
- [condition] **REPLAY-INCOMPLETE** *REPLAY-FAILURE*

    Signalled if there are unprocessed non-log events in
    [`REPLAY-JOURNAL`][838b] when [`WITH-JOURNALING`][6131] finishes and the body of
    `WITH-JOURNALING` returned normally, which is to prevent this
    condition to cancel an ongoing unwinding. No restarts are provided.

<a id="x-28JOURNAL-3AREPLAY-FORCE-INSERT-20RESTART-29"></a>
- [restart] **REPLAY-FORCE-INSERT**

    This restart forces [The replay strategy][a8a7] to be `:INSERT`, overriding
    [`REPLAY-NAME-MISMATCH`][6710]. This is intended for upgrades, and extreme
    care must be taken not to lose data.

<a id="x-28JOURNAL-3AREPLAY-FORCE-UPGRADE-20RESTART-29"></a>
- [restart] **REPLAY-FORCE-UPGRADE**

    This restart forces [The replay strategy][a8a7] to be `:UPGRADE`, overriding
    [`REPLAY-NAME-MISMATCH`][6710], [`REPLAY-VERSION-DOWNGRADE`][0fdb],
    [`REPLAY-ARGS-MISMATCH`][1256], [`REPLAY-OUTCOME-MISMATCH`][bbef]. This is intended for
    upgrades, and extreme care must be taken not to lose data.

<a id="x-28JOURNAL-3A-40UPGRADES-AND-REPLAY-20MGL-PAX-3ASECTION-29"></a>
### 9.7 Upgrades and replay

The replay mechanism is built on the assumption that the tree of
[frame][7df7]s is the same when the code is replayed as it was when the
replay journal was originally recorded. Thus, non-deterministic
control flow poses a challenge, but non-determinism can be isolated
with [`EXTERNAL-EVENT`][0e53]s. However, when the code changes, we might find
the structure of frames in previous recordings hard to accommodate.
In this case, we might decide to alter the structure, giving up some
of the safety provided by the replay mechanism. There are various
tools at our disposal to control this tradeoff between safety and
flexibility:

- We can insert individual frames with [`JOURNALED`][6267]'s `INSERTABLE`,
  upgrade frames by bumping `JOURNALED`'s `VERSION`, and filter frames
  with [`WITH-REPLAY-FILTER`][0cce]. This option allows for the most
  consistency checks.

- The [`REPLAY-FORCE-UPGRADE`][10c8] and [`REPLAY-FORCE-INSERT`][92aa] restarts allow
  overriding [The replay strategy][a8a7], but their use requires great care
  to be taken.

- Or we may decide to keep the bare minimum of the replay journal
  around and discard everything except for `EXTERNAL-EVENT`s. This
  option is equivalent to

        (let ((*force-insertable* t))
          (with-replay-filter (:skip '((:name nil)))
            42))

- Rerecording the journal without replay might be another option if
  there are no `EXTERNAL-EVENT`s to worry about.

- Finally, we can rewrite the replay journal using the low-level
  interface (see [Streamlets reference][f4d5]). In this case, extreme care
  must be taken not to corrupt the journal (and lose data) as there
  are no consistency checks to save us.

With that, let's see how `WITH-REPLAY-FILTER` works.

<a id="x-28JOURNAL-3AWITH-REPLAY-STREAMLET-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-REPLAY-STREAMLET** *(VAR) &BODY BODY*

    Open [`REPLAY-JOURNAL`][838b] for reading with [`WITH-OPEN-JOURNAL`][6d64], set the
    [`READ-POSITION`][6e60] on it to the event next read by the [Replay][041c]
    mechanism (which is never a [`LOG-EVENT`][51ce]). The low-level
    [Reading from streamlets][adcd] api is then available to inspect the
    contents of the replay. It is an error if `REPLAY-JOURNAL` is `NIL`.

<a id="x-28JOURNAL-3APEEK-REPLAY-EVENT-20FUNCTION-29"></a>
- [function] **PEEK-REPLAY-EVENT**

    Return the [replay event][6525] to be read from [`REPLAY-JOURNAL`][838b]. This is
    roughly equivalent to
    
    ```
    (when (replay-journal)
      (with-replay-streamlet (streamlet)
        (peek-event streamlet))
    ```
    
    except `PEEK-REPLAY-EVENT` takes into account [`WITH-REPLAY-FILTER`][0cce]
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
    [`EXTERNAL-EVENT`][0e53]s is tricky.
    
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


<a id="x-28JOURNAL-3AWITH-REPLAY-FILTER-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-REPLAY-FILTER** *(&KEY MAP SKIP NO-REPLAY-OUTCOME) &BODY BODY*

    `WITH-REPLAY-FILTER` performs journal upgrade during replay by
    allowing events to be transformed as they are read from the replay
    journal or skipped if they match some patterns. For how to add new
    blocks in a code upgrade, see [`JOURNALED`][6267]'s `:INSERTABLE` argument. In
    addition, it also allows some control over [Replaying the outcome][7991].
    
    - `MAP`: A function called with an event read from the replay journal
      which returns a transformed event. See [Events reference][faf2]. `MAP`
      takes effect before before `SKIP`.
    
    - `SKIP`: In addition to filtering out [`LOG-EVENT`][51ce]s (which always
      happens during replay), filter out all events that belong to
      frames that match any of its `SKIP` patterns. Filtered out events
      are never seen by `JOURNALED` as it replays events. `SKIP` patterns
      are of the format `(&KEY NAME VERSION<)`, where `VERSION<` is a
      valid [`EVENT-VERSION`][9ed3], and `NAME` may be `NIL`, which acts as a
      wildcard.
    
        `SKIP` is for when `JOURNALED` [block][06a7]s are removed from the code,
        which would render replaying previously recorded journals
        impossible. Note that, for reasons of safety, it is not possible
        to filter [`EXTERNAL-EVENT`][0e53]s.
    
    - `NO-REPLAY-OUTCOME` is a list of [`EVENT-NAME`][9f84]s. [Replaying the outcome][7991]
      is prevented for frames with [`EQUAL`][96d0] names. See
      [Testing on multiple levels][9376] for an example.
    
    `WITH-REPLAY-FILTER` affects only the immediately enclosing
    [`WITH-JOURNALING`][6131]. A `WITH-REPLAY-FILTER` nested within another in the
    same `WITH-JOURNALING` inherits the `SKIP` patterns of its parent, to
    which it adds its own. The [`MAP`][73ba] function is applied to before the
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


<a id="x-28JOURNAL-3A-40TESTING-20MGL-PAX-3ASECTION-29"></a>
## 10 Testing

Having discussed the [Replay][041c] mechanism, next are [Testing][7682] and
[Persistence][37c4], which rely heavily on replay. Suppose we want to unit
test user registration. Unfortunately, the code communicates with a
database service and also takes input from the user. A natural
solution is to create [mocks][mock-object] for these external
systems to unshackle the test from the cumbersome database
dependency and to allow it to run without user interaction.

We do this below by wrapping external interaction in [`JOURNALED`][6267] with
`:VERSION` `:INFINITY` (see [Replaying the outcome][7991]).

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

Now, we write a test that records these interactions in a file when
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
[Upgrades and replay][750a]) or just rerecord from scratch:

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

Note that when this journal is replayed, new [`VERSIONED-EVENT`][4c2b]s are
required to match the replay. So, after the original recording, we
can check by eyeballing that the record represents a correct
execution. Then on subsequent replays, even though
`MAYBE-WIN-THE-GRAND-PRIZE` sits behind `REGISTER-USER` and is hard
to test with [`ASSERT`][cf68]s, the replay mechanism verifies that it is
called only for new users.

This record-and-replay style of testing is not the only possibility:
direct inspection of a journal with the low-level events api (see
[Events reference][faf2]) can facilitate checking non-local invariants.

<a id="x-28JOURNAL-3ADEFINE-FILE-BUNDLE-TEST-20MGL-PAX-3AMACRO-29"></a>
- [macro] **DEFINE-FILE-BUNDLE-TEST** *(NAME &KEY DIRECTORY (EQUIVALENTP T)) &BODY BODY*

    Define a function with `NAME` for record-and-replay testing. The
    function's `BODY` is executed in a [`WITH-BUNDLE`][12a5] to guarantee
    replayability. The bundle in question is a [`FILE-BUNDLE`][18955] created in
    `DIRECTORY`. The function has a single keyword argument, `RERECORD`. If
    `RERECORD` is true, the bundle is deleted with [`DELETE-FILE-BUNDLE`][c438] to
    start afresh.
    
    Furthermore, if `BODY` returns normally, and it is a replay of a
    previous run, and `EQUIVALENTP`, then it is ASSERTed that the record
    and replay journals are [`EQUIVALENT-REPLAY-JOURNALS-P`][712a]. If this check
    fails, [`RECORD-JOURNAL`][3b63] is discarded when the function returns. In
    addition to the replay consistency, this checks that no inserts or
    upgrades were performed (see [The replay strategy][a8a7]).

<a id="x-28JOURNAL-3A-40TESTING-ON-MULTIPLE-LEVELS-20MGL-PAX-3ASECTION-29"></a>
### 10.1 Testing on multiple levels

Nesting [`REPLAYED`][c2b8]s (that is, [frame][7df7]s of [`EXTERNAL-EVENT`][0e53]s) is not
obviously useful since the outer `REPLAYED` will be replayed by
outcome, and the inner one will be just echoed to the record
journal. However, if we turn off [Replaying the outcome][7991] for the
outer, the inner will be replayed.

This is useful for testing layered communication. For example, we
might have written code that takes input from an external
system ([`READ-LINE`][d97d]) and does some complicated
processing ([`READ-FROM-STRING`][bb62]) before returning the input in a form
suitable for further processing. Suppose we wrap `REPLAYED` around
`READ-FROM-STRING` for [Persistence][37c4] because putting it around
`READ-LINE` would expose low-level protocol details in the journal,
making protocol changes difficult.

However, upon realizing that `READ-FROM-STRING` was not the best tool
for the job and switching to [`PARSE-INTEGER`][026a], we want to test by
replaying all previously recorded journals. For this, we prevent the
outer `REPLAYED` from being replayed by outcome with
[`WITH-REPLAY-FILTER`][0cce]:

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
checked as if it was a [`VERSIONED-EVENT`][4c2b] and we get a
[`REPLAY-OUTCOME-MISMATCH`][bbef] due to the bug.

<a id="x-28JOURNAL-3A-40PERSISTENCE-20MGL-PAX-3ASECTION-29"></a>
## 11 Persistence

<a id="x-28JOURNAL-3A-40PERSISTENCE-TUTORIAL-20MGL-PAX-3ASECTION-29"></a>
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
department. In the transcript below, [`PARSE-INTEGER`][026a] fails with `junk
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
beginning, [Replaying the outcome][7991] of external interactions marked
with [`REPLAYED`][c2b8]:

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
[`IN-MEMORY-BUNDLE`][bacd], we used a [`FILE-BUNDLE`][18955], the game would have been
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
control flow and only requires that non-determinism is packaged up
in `REPLAYED`, which allows it to reconstruct the state of the program
from the recorded events at any point during its execution and
resume from there.

<a id="x-28JOURNAL-3A-40SYNCHRONIZATION-20MGL-PAX-3ASECTION-29"></a>
### 11.2 Synchronization to storage

In the following, we explore how journals can serve as a
persistence mechanism and the guarantees they offer. The high-level
summary is that journals with `SYNC` can serve as a durable and
consistent storage medium. The other two
[ACID](https://en.wikipedia.org/wiki/ACID) properties, atomicity and
isolation, do not apply because Journal is single-client and does
not need transactions.

<a id="x-28JOURNAL-3A-40ABORTED-EXECUTION-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **aborted execution**

    Aborted execution is when the operating system or the application
    crashes, calls `abort()`, is killed by a `SIGKILL` signal or there
    is a power outage. Synchronization guarantees are defined in the
    face of aborted execution and do not apply to hardware errors, Lisp
    or OS bugs.

<a id="x-28JOURNAL-3A-40DATA-EVENT-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **data event**

    Data events are the only events that may be non-deterministic. They
    record information that could change if the same code were run
    multiple times. Data events typically correspond to interactions
    with the user, servers or even the random number generator. Due to
    their non-determinism, they are the only parts of the journal not
    reproducible by rerunning the code. In this sense, only the data
    events are not redundant with the code, and whether other events are
    persisted does not affect durability. There are two kinds of data
    events:
    
    - An [`EXTERNAL-EVENT`][0e53] that is also an [`OUT-EVENT`][637d].
    
    - The [`IN-EVENT`][1729] of an [invoked][4212] function, which lies outside the
      normal, deterministic control flow.


<a id="x-28JOURNAL-3A-40SYNCHRONIZATION-STRATEGIES-20MGL-PAX-3ASECTION-29"></a>
#### 11.2.1 Synchronization strategies

When a journal or bundle is created (see [`MAKE-IN-MEMORY-JOURNAL`][9955],
[`MAKE-FILE-JOURNAL`][f0e7], [`MAKE-IN-MEMORY-BUNDLE`][8d1e], [`MAKE-FILE-BUNDLE`][d6af]), the
`SYNC` option determines when – as a [`RECORD-JOURNAL`][3b63] – the recorded
events and [`JOURNAL-STATE`][03de] changes are persisted durably. For
[`FILE-JOURNAL`][8428]s, persisting means calling something like `fsync`,
while for [`IN-MEMORY-JOURNAL`][b668]s, a user defined function is called to
persist the data.

- `NIL`: Never synchronize. A `FILE-JOURNAL`'s file may be corrupted on
  [aborted execution][78fd]. In `IN-MEMORY-JOURNAL`s, `SYNC-FN` is never
  called.

- `T`: This is the *no data loss* setting with minimal
  synchronization. It guarantees *consistency* (i.e. no corruption)
  and *durability* up to the most recent [data event][c015] written in
  `JOURNAL-STATE` `:RECORDING` or for the entire record journal in
  states `:FAILED` and `:COMPLETED`. `:FAILED` or `:COMPLETED` is guaranteed
  when leaving [`WITH-JOURNALING`][6131] at the latest.

- Values other than `NIL` and `T` are reserved for future extensions.
  Using them triggers a [`JOURNAL-ERROR`][0002].


<a id="x-28JOURNAL-3A-40SYNCHRONIZATION-WITH-IN-MEMORY-JOURNALS-20MGL-PAX-3ASECTION-29"></a>
#### 11.2.2 Synchronization with in-memory journals

Unlike [`FILE-JOURNAL`][8428]s, [`IN-MEMORY-JOURNAL`][b668]s do not have any built-in
persistent storage backing them, but with `SYNC-FN`, persistence can
be tacked on. If non-NIL, `SYNC-FN` must be a function of a single
argument, an `IN-MEMORY-JOURNAL`. `SYNC-FN` is called according to
[Synchronization strategies][f532], and upon normal return the journal must
be stored durably.

The following example saves the entire journal history when a new
[data event][c015] is recorded. Note how `SYNC-TO-DB` is careful to
overwrite `*DB*` only if it is called with a journal that has not
failed the replay (as in [Replay failures][2933]) and is sufficiently
different from the replay journal as determined by
[`JOURNAL-DIVERGENT-P`][f224].

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
an error, say, to simulate some kind of network condition. Now, a
new journal *for replay* is created and initialized with the saved
events, and the whole process is restarted.

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
case returning 2. See [Replaying the outcome][7991]. Block `B`, on the
other hand, was rerun because it had an [unexpected outcome][d2c1] the
first time around. This time it ran without error, a [data event][c015] was
triggered, and `SYNC-FN` was invoked.

If we were to invoke the now completed `RUN-WITH-DB` again, it would
simply return 3 without ever invoking `SYNC-FN`:

```
(run-with-db)
=> 3
```

With [`JOURNAL-REPLAY-MISMATCH`][228c], `SYNC-FN` can be optimized to to reuse
the sequence of events in the replay journal up until the point of
divergence.

<a id="x-28JOURNAL-3A-40SYNCHRONIZATION-WITH-FILE-JOURNALS-20MGL-PAX-3ASECTION-29"></a>
#### 11.2.3 Synchronization with file journals

For [`FILE-JOURNAL`][8428]s, `SYNC` determines when the events written to the
[`RECORD-JOURNAL`][3b63] and its [`JOURNAL-STATE`][03de] will be persisted durably in
the file. Syncing to the file involves two calls to `fsync` and is
not cheap.

Syncing events to files is implemented as follows.

- When the journal file is created, its parent directory is
  immediately fsynced to make sure that the file will not be lost on
  [aborted execution][78fd].

- When an event is about to be written the first time after file
  creation or after a sync, a transaction start marker is written to
  the file.

- Any number of events may be subsequently written until syncing is
  deemed necessary (see [Synchronization strategies][f532]).

- At this point, `fsync` is called to flush all event data and state
  changes to the file, and the transaction start marker is
  *overwritten* with a transaction completed marker and another
  `fsync` is performed.

- When reading back this file (e.g. for replay), an open transaction
  marker is treated as the end of file.

Note that this implementation assumes that after writing the start
transaction marker, a crash cannot leave any kind of garbage bytes
around: it must leave zeros. This is not true for all filesytems.
For example, ext3/ext4 with `data=writeback` [can leave garbage
around][ext4-writeback].

[ext4-writeback]: https://ext4.wiki.kernel.org/index.php/Ext3_Data=Ordered_vs_Data=Writeback_mode 


<a id="x-28JOURNAL-3A-40SAFETY-20MGL-PAX-3ASECTION-29"></a>
## 12 Safety

##### Thread safety

Changes to journals come in two varieties: adding an event and
changing the [`JOURNAL-STATE`][03de]. Both are performed by [`JOURNALED`][6267] only
unless the low-level streamlet interface is used (see
[Streamlets reference][f4d5]). Using `JOURNALED` wrapped in a
[`WITH-JOURNALING`][6131], [`WITH-BUNDLE`][12a5], or [`:LOG-RECORD`][a6ac] without `WITH-JOURNALING`
is thread-safe.

- Every journal is guaranteed to have at most a single writer active
  at any time. Writers are mainly `WITH-JOURNALING` and `WITH-BUNDLE`,
  but any journals directly logged to have a log writer stored in
  the journal object. See [Logging][4e53].

- `WITH-JOURNALING` and `WITH-BUNDLE` have dynamic extent as writers,
  but log writers of journals have indefinite extent: once a journal
  is used as a `LOG-RECORD`, there remains a writer.

- Attempting to create a second writer triggers a [`JOURNAL-ERROR`][0002].

- Writing to the same journal via [`:LOG-RECORD`][a6ac] from multiple threads
  concurrently is possible since this doesn't create multiple
  writers. It is ensured with locking that events are written
  atomically. Frames can be interleaved, but these are [`LOG-EVENT`][51ce]s,
  so this does not affect replay.

- The juggling of replay and record journals performed by
  `WITH-BUNDLE` is also thread-safe.

- It is ensured that there is at most one [`FILE-JOURNAL`][8428] object in the
  same Lisp image is backed by the same file.

- Similarly, there is at most [`FILE-BUNDLE`][18955] object for a directory.

##### Process safety

Currently, there is no protection against multiple OS processes
writing the same `FILE-JOURNAL` or `FILE-BUNDLE`.

##### Signal safety

Journal is *designed* to be [async-unwind][392c] safe but *not reentrant*.
Interrupts are disabled only for the most critical cleanup forms. If
a thread is killed without unwinding, that constitutes
[aborted execution][78fd], so guarantees about [Synchronization to storage][046e] apply, but
[`JOURNAL`][5082] objects written by the thread are not safe to access, and
the Lisp should probably be restarted.

<a id="x-28JOURNAL-3A-40EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29"></a>
## 13 Events reference

Events are normally triggered upon entering and leaving the
dynamic extent of a [`JOURNALED`][6267] [block][06a7] (see [In-events][186b] and
[Out-events][48ef]) and also by [`LOGGED`][23c4]. Apart from being part of the
low-level substrate of the Journal library, working with events
directly is sometimes useful when writing tests that inspect
recorded events. Otherwise, skip this entire section.

All [`EVENT`][a394]s have [`EVENT-NAME`][9f84] and `EVENT-VERSION`([`0`][1f5f] [`1`][9ed3]), which feature
prominently in [The replay strategy][a8a7]. After the examples in
[In-events][186b] and [Out-events][48ef], the following example is a reminder of
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

So, a `JOURNALED` [block][06a7] generates an [`IN-EVENT`][1729] and an [`OUT-EVENT`][637d], which
are simple property lists. The following reference lists these
properties, their semantics and the functions to read them.

<a id="x-28JOURNAL-3AEVENT-20TYPE-29"></a>
- [type] **EVENT**

    An event is either an [`IN-EVENT`][1729], an [`OUT-EVENT`][637d] or a [`LEAF-EVENT`][5cd1].

<a id="x-28JOURNAL-3AEVENT-3D-20FUNCTION-29"></a>
- [function] **EVENT=** *EVENT-1 EVENT-2*

    Return whether `EVENT-1` and `EVENT-2` represent the same event.
    In- and out-events belonging to the same [frame][7df7] are *not* the same
    event. [`EVENT-OUTCOME`][c290]s are not compared when `EVENT-EXIT`([`0`][c04d] [`1`][812a]) is `:ERROR` to
    avoid undue dependence on implementation specific string
    representations. This function is useful in conjunction with
    [`MAKE-IN-EVENT`][9ebd] and [`MAKE-OUT-EVENT`][3439] to write tests.

<a id="x-28JOURNAL-3AEVENT-NAME-20FUNCTION-29"></a>
- [function] **EVENT-NAME** *EVENT*

    The name of an event can be of any type. It is often a symbol or a
    string. When replaying, names are compared with [`EQUAL`][96d0]. All `EVENT`s
    have names. The names of the in- and out-events belonging to the
    same [frame][7df7] are the same.

<a id="x-28JOURNAL-3A-40EVENT-VERSIONS-20MGL-PAX-3ASECTION-29"></a>
### 13.1 Event versions

<a id="x-28JOURNAL-3AEVENT-VERSION-20FUNCTION-29"></a>
- [function] **EVENT-VERSION** *EVENT*

    Return the version of `EVENT` of type [`EVENT-VERSION`][9ed3].

<a id="x-28JOURNAL-3ALOG-EVENT-P-20FUNCTION-29"></a>
- [function] **LOG-EVENT-P** *EVENT*

    See if `EVENT` is a [`LOG-EVENT`][51ce].

<a id="x-28JOURNAL-3AVERSIONED-EVENT-P-20FUNCTION-29"></a>
- [function] **VERSIONED-EVENT-P** *EVENT*

    See if `EVENT` is a [`VERSIONED-EVENT`][4c2b].

<a id="x-28JOURNAL-3AEXTERNAL-EVENT-P-20FUNCTION-29"></a>
- [function] **EXTERNAL-EVENT-P** *EVENT*

    See if `EVENT` is an [`EXTERNAL-EVENT`][0e53].

<a id="x-28JOURNAL-3A-40IN-EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29"></a>
### 13.2 In-events

<a id="x-28JOURNAL-3AIN-EVENT-20TYPE-29"></a>
- [type] **IN-EVENT**

    `IN-EVENT`s are triggered upon entering the dynamic extent of a
    [`JOURNALED`][6267] [block][06a7]. `IN-EVENT`s have [`EVENT-NAME`][9f84],
    [`EVENT-VERSION`][1f5f], and [`EVENT-ARGS`][3335]. See [In-events][186b] for a more
    introductory treatment.

<a id="x-28JOURNAL-3AIN-EVENT-P-20FUNCTION-29"></a>
- [function] **IN-EVENT-P** *EVENT*

    See if `EVENT` is a [`IN-EVENT`][1729].

<a id="x-28JOURNAL-3AMAKE-IN-EVENT-20FUNCTION-29"></a>
- [function] **MAKE-IN-EVENT** *&KEY NAME VERSION ARGS*

    Create an [`IN-EVENT`][1729] with `NAME`, `VERSION` (of type [`EVENT-VERSION`][9ed3]) and
    `ARGS` as its [`EVENT-NAME`][9f84], [`EVENT-VERSION`][1f5f] and [`EVENT-ARGS`][3335].

<a id="x-28JOURNAL-3AEVENT-ARGS-20FUNCTION-29"></a>
- [function] **EVENT-ARGS** *IN-EVENT*

    Return the arguments of `IN-EVENT`, normally populated using the `ARGS`
    form in [`JOURNALED`][6267].

<a id="x-28JOURNAL-3A-40OUT-EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29"></a>
### 13.3 Out-events

<a id="x-28JOURNAL-3AOUT-EVENT-20TYPE-29"></a>
- [type] **OUT-EVENT**

    `OUT-EVENT`s are triggered upon leaving the dynamic extent of the
    [`JOURNALED`][6267] [block][06a7]. `OUT-EVENT`s have [`EVENT-NAME`][9f84],
    [`EVENT-VERSION`][1f5f], [`EVENT-EXIT`][c04d] and [`EVENT-OUTCOME`][c290].
    See [Out-events][48ef] for a more introductory treatment.

<a id="x-28JOURNAL-3AOUT-EVENT-P-20FUNCTION-29"></a>
- [function] **OUT-EVENT-P** *EVENT*

    See if `EVENT` is an [`OUT-EVENT`][637d].

<a id="x-28JOURNAL-3AMAKE-OUT-EVENT-20FUNCTION-29"></a>
- [function] **MAKE-OUT-EVENT** *&KEY NAME VERSION EXIT OUTCOME*

    Create an [`OUT-EVENT`][637d] with `NAME`, `VERSION` (of type [`EVENT-VERSION`][9ed3]),
    `EXIT` (of type [`EVENT-EXIT`][812a]), and `OUTCOME` as its [`EVENT-NAME`][9f84],
    [`EVENT-VERSION`][1f5f], [`EVENT-EXIT`][c04d] and [`EVENT-OUTCOME`][c290].

<a id="x-28JOURNAL-3AEVENT-EXIT-20FUNCTION-29"></a>
- [function] **EVENT-EXIT** *OUT-EVENT*

    Return how the journaled [block][06a7] finished. See [`EVENT-EXIT`][812a]
    for the possible types.

<a id="x-28JOURNAL-3AEXPECTED-OUTCOME-P-20FUNCTION-29"></a>
- [function] **EXPECTED-OUTCOME-P** *OUT-EVENT*

    See if `OUT-EVENT` has an [expected outcome][4657].

<a id="x-28JOURNAL-3AUNEXPECTED-OUTCOME-P-20FUNCTION-29"></a>
- [function] **UNEXPECTED-OUTCOME-P** *OUT-EVENT*

    See if `OUT-EVENT` has an [unexpected outcome][d2c1].

<a id="x-28JOURNAL-3AEVENT-OUTCOME-20FUNCTION-29"></a>
- [function] **EVENT-OUTCOME** *OUT-EVENT*

    Return the outcome of the [frame][7df7] (or loosely speaking of a [block][06a7])
    to which `OUT-EVENT` belongs.

<a id="x-28JOURNAL-3A-40LEAF-EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29"></a>
### 13.4 Leaf-events

<a id="x-28JOURNAL-3ALEAF-EVENT-20TYPE-29"></a>
- [type] **LEAF-EVENT**

    Leaf events are triggered by [`LOGGED`][23c4]. Unlike [`IN-EVENT`][1729]s and
    [`OUT-EVENT`][637d]s, which represent a [frame][7df7], leaf events represent a point
    in execution thus cannot have children. They are also the poorest of
    their kind: they only have an [`EVENT-NAME`][9f84]. Their `VERSION` is always
    `NIL`, which makes them [`LOG-EVENT`][51ce]s.

<a id="x-28JOURNAL-3ALEAF-EVENT-P-20FUNCTION-29"></a>
- [function] **LEAF-EVENT-P** *EVENT*

    See if `EVENT` is a [`LEAF-EVENT`][5cd1].

<a id="x-28JOURNAL-3AMAKE-LEAF-EVENT-20FUNCTION-29"></a>
- [function] **MAKE-LEAF-EVENT** *NAME*

    Create a [`LEAF-EVENT`][5cd1] with `NAME`.

<a id="x-28JOURNAL-3A-40JOURNALS-REFERENCE-20MGL-PAX-3ASECTION-29"></a>
## 14 Journals reference

In [Basics][f846], we covered the bare minimum needed to work with
journals. Here, we go into the details.

<a id="x-28JOURNAL-3AJOURNAL-20CLASS-29"></a>
- [class] **JOURNAL**

    `JOURNAL` is an abstract base class for a sequence of
    events. In case of [`FILE-JOURNAL`][8428]s, the events are stored in a file,
    while for [`IN-MEMORY-JOURNAL`][b668]s, they are in a Lisp array. When a
    journal is opened, it is possible to perform I/O on it (see
    [Streamlets reference][f4d5]), which is normally taken care of by
    [`WITH-JOURNALING`][6131]. For this reason, the user's involvement with
    journals normally only consists of creating and using them in
    `WITH-JOURNALING`.

<a id="x-28JOURNAL-3AJOURNAL-STATE-20-28MGL-PAX-3AREADER-20JOURNAL-3AJOURNAL-29-29"></a>
- [reader] **JOURNAL-STATE** *JOURNAL (:STATE)*

    Return the state of [`JOURNAL`][5082], which is of type
    [`JOURNAL-STATE`][03de].

<a id="x-28JOURNAL-3AJOURNAL-SYNC-20-28MGL-PAX-3AREADER-20JOURNAL-3AJOURNAL-29-29"></a>
- [reader] **JOURNAL-SYNC** *JOURNAL (:SYNC = NIL)*

    The `SYNC` argument specified at instantiation. See
    [Synchronization strategies][f532].

<a id="x-28JOURNAL-3ASYNC-JOURNAL-20FUNCTION-29"></a>
- [function] **SYNC-JOURNAL** *&OPTIONAL (JOURNAL (RECORD-JOURNAL))*

    Durably persist changes made to `JOURNAL` if [`JOURNAL-SYNC`][0752] is `T`.
    The changes that are persisted are 
    
    - [`WRITE-EVENT`][01fd]s and [`JOURNAL-STATE`][03de] changes made in an enclosing
      WITH-JOURNALING; and
    
    - `LOG-RECORD`s from any thread. 
    
    In particular, writes made in a [`WITH-JOURNALING`][6131] in another thread
    are not persisted. `SYNC-JOURNAL` is a noop if `JOURNAL-SYNC` is `NIL`. It
    is safe to call from any thread.

<a id="x-28JOURNAL-3AJOURNAL-REPLAY-MISMATCH-20-28MGL-PAX-3AREADER-20JOURNAL-3AJOURNAL-29-29"></a>
- [reader] **JOURNAL-REPLAY-MISMATCH** *JOURNAL (= NIL)*

    If [`JOURNAL-DIVERGENT-P`][f224], then this is a list of two
    elements: the [`READ-POSITION`][6e60]s in the [`RECORD-JOURNAL`][3b63] and
    [`REPLAY-JOURNAL`][838b] of the first events that were different (ignoring
    [`LOG-EVENT`][51ce]s). It is `NIL`, otherwise.

<a id="x-28JOURNAL-3AJOURNAL-DIVERGENT-P-20FUNCTION-29"></a>
- [function] **JOURNAL-DIVERGENT-P** *JOURNAL*

    See if [`WITH-JOURNALING`][6131] recorded any event so far in this journal
    that was not [`EQUAL`][96d0] to its [replay event][6525] or it had no corresponding
    replay event. This completely ignores [`LOG-EVENT`][51ce]s in both journals
    being compared and can be called any time during [Replay][041c]. It plays a
    role in [`WITH-BUNDLE`][12a5] deciding when a journal is important enough to
    keep and also in [Synchronization with in-memory journals][12ff].
    
    The position of the first mismatch is available via
    [`JOURNAL-REPLAY-MISMATCH`][228c].

<a id="x-28JOURNAL-3A-40COMPARING-JOURNALS-20MGL-PAX-3ASECTION-29"></a>
### 14.1 Comparing journals

After replay finished (i.e. [`WITH-JOURNALING`][6131] completed), we can ask
whether there were any changes produced. This is answered in the
strictest sense by [`IDENTICAL-JOURNALS-P`][4a00] and somewhat more
functionally by [`EQUIVALENT-REPLAY-JOURNALS-P`][712a].

Also see [`JOURNAL-DIVERGENT-P`][f224].

<a id="x-28JOURNAL-3AIDENTICAL-JOURNALS-P-20GENERIC-FUNCTION-29"></a>
- [generic-function] **IDENTICAL-JOURNALS-P** *JOURNAL-1 JOURNAL-2*

    Compare two journals in a strict sense: whether
    they have the same [`JOURNAL-STATE`][03de] and the lists of their events (as
    in [`LIST-EVENTS`][0c1b]) are [`EQUAL`][96d0].

<a id="x-28JOURNAL-3AEQUIVALENT-REPLAY-JOURNALS-P-20GENERIC-FUNCTION-29"></a>
- [generic-function] **EQUIVALENT-REPLAY-JOURNALS-P** *JOURNAL-1 JOURNAL-2*

    See if two journals are equivalent when used the
    for `REPLAY` in [`WITH-JOURNALING`][6131]. `EQUIVALENT-REPLAY-JOURNALS-P` is like
    [`IDENTICAL-JOURNALS-P`][4a00], but it ignores [`LOG-EVENT`][51ce]s and allows events
    with `EVENT-EXIT`([`0`][c04d] [`1`][812a]) `:ERROR` to differ in their outcomes, which may very
    well be implementation specific, anyway. Also, it considers two
    groups of states as different `:NEW`, `:REPLAYING`, `:MISMATCHED`, `:FAILED`
    vs `:RECORDING`, `:LOGGING`, COMPLETED.

The rest of section is about concrete subclasses of [`JOURNAL`][5082].

<a id="x-28JOURNAL-3A-40IN-MEMORY-JOURNALS-20MGL-PAX-3ASECTION-29"></a>
### 14.2 In-memory journals

<a id="x-28JOURNAL-3AIN-MEMORY-JOURNAL-20CLASS-29"></a>
- [class] **IN-MEMORY-JOURNAL** *[JOURNAL][5082]*

    `IN-MEMORY-JOURNAL`s are backed by a non-persistent
    Lisp array of events. Much quicker than [`FILE-JOURNAL`][8428]s, they are
    ideal for smallish journals persisted manually (see
    [Synchronization with in-memory journals][12ff] for an example).
    
    They are also useful for writing tests based on what events were
    generated. They differ from `FILE-JOURNAL`s in that events written to
    `IN-MEMORY-JOURNAL`s are not serialized (and deserialized on replay)
    with the following consequences for the objects recorded by
    [`JOURNALED`][6267] (i.e. its `NAME`, `ARGS` arguments, and also the return `VALUES`([`0`][e88d] [`1`][43ad])
    of the block, or the value returned by [`CONDITION`][dc76]):
    
    - These objects need not be [readable][768f].
    
    - Their identity ([`EQ`][a1d4]ness) is not lost.
    
    - They must **must not be mutated** in any way.


<a id="x-28JOURNAL-3AMAKE-IN-MEMORY-JOURNAL-20FUNCTION-29"></a>
- [function] **MAKE-IN-MEMORY-JOURNAL** *&KEY (EVENTS NIL EVENTSP) STATE (SYNC NIL SYNCP) SYNC-FN*

    Create an [`IN-MEMORY-JOURNAL`][b668].
    
    The returned journal's [`JOURNAL-STATE`][03de] will be set to `STATE`. If `STATE`
    is `NIL`, then it is replaced by a default value, which is `:COMPLETED`
    if the `EVENTS` argument is provided, else it is `:NEW`.
    
    Thus, `(make-in-memory-journal)` creates a journal suitable for
    recording, and to make a replay journal, use `:STATE` `:COMPLETED` with
    some sequence of `EVENTS`:
    
    ```
    (make-in-memory-journal :events '((:in foo :version 1)) :state :completed)
    ```
    
    `SYNC` determines when `SYNC-FN` will be invoked on the [`RECORD-JOURNAL`][3b63].
    `SYNC` defaults to `T` if `SYNC-FN`, else to `NIL`. For a description of
    possible values, see [Synchronization strategies][f532]. For more
    discussion, see [Synchronization with in-memory journals][12ff].

<a id="x-28JOURNAL-3AJOURNAL-EVENTS-20-28MGL-PAX-3AREADER-20JOURNAL-3AIN-MEMORY-JOURNAL-29-29"></a>
- [reader] **JOURNAL-EVENTS** *IN-MEMORY-JOURNAL (:EVENTS)*

    A sequence of events in the journal. Not to be
    mutated by client code.

<a id="x-28JOURNAL-3AJOURNAL-PREVIOUS-SYNC-POSITION-20-28MGL-PAX-3AREADER-20JOURNAL-3AIN-MEMORY-JOURNAL-29-29"></a>
- [reader] **JOURNAL-PREVIOUS-SYNC-POSITION** *IN-MEMORY-JOURNAL (= 0)*

    The length of [`JOURNAL-EVENTS`][5e0a] at the time of the
    most recent invocation of `SYNC-FN`.

<a id="x-28JOURNAL-3A-40FILE-JOURNALS-20MGL-PAX-3ASECTION-29"></a>
### 14.3 File journals

<a id="x-28JOURNAL-3AFILE-JOURNAL-20CLASS-29"></a>
- [class] **FILE-JOURNAL** *[JOURNAL][5082]*

    A `FILE-JOURNAL` is a journal whose contents and
    [`JOURNAL-STATE`][03de] are persisted in a file. This is the [`JOURNAL`][5082]
    subclass with out-of-the-box persistence, but see [File bundles][c05a] for
    a more full-featured solution for repeated [Replay][041c]s.
    
    Since serialization in `FILE-JOURNAL`s is built on top of Lisp [`READ`][3d3c]
    and [`WRITE`][b764], everything that [`JOURNALED`][6267] records in events (i.e. its
    `NAME`, `ARGS` arguments, and also the return `VALUES`([`0`][e88d] [`1`][43ad]) of the block, or
    the value returned by [`CONDITION`][dc76]) must be [readable][768f].
    
    File journals are human-readable and editable by hand with some
    care. When editing, the following needs to be remembered:
    
    - The first character of the file represents its `JOURNAL-STATE`. It
      is a `#\Space` (for state `:NEW`, `:REPLAYING`, `:MISMATCHED` and
      `:FAILED`), or a `#\Newline` (for state `:RECORDING`, `:LOGGING` and
      `:COMPLETED`).
    
    - If the journal has `SYNC` (see [Synchronization strategies][f532]), then
      between two events, there may be `#\Del` (also called `#\Rubout`)
      or `#\Ack` characters ([`CHAR-CODE`][7e2c] 127 and 6). `#\Del` marks the end
      of the journal contents that may be read back: it's kind of an
      uncommitted-transaction marker for the events that follow it.
      `#\Ack` characters, of which there may be many in the file, mark
      the sequence of events until the next marker of either kind as
      valid (or committed). `#\Ack` characters are ignored when reading
      the journal.
    
    Thus, when editing a file, don't change the first character and
    leave the `#\Del` character, if any, where it is. Also see
    [Synchronization with file journals][674f].

<a id="x-28JOURNAL-3AMAKE-FILE-JOURNAL-20FUNCTION-29"></a>
- [function] **MAKE-FILE-JOURNAL** *PATHNAME &KEY SYNC*

    Return a [`FILE-JOURNAL`][8428] backed by the file with `PATHNAME`. The file is
    created when the journal is opened for writing. For a description of
    `SYNC`, see [Synchronization strategies][f532].
    
    If there is already an existing `FILE-JOURNAL` backed by the same
    file, then that object is returned. If the existing object has
    different options (e.g. it has `SYNC` `T` while the `SYNC` argument is `NIL`
    here), then a [`JOURNAL-ERROR`][0002] is signalled.
    
    If there is already an existing `FILE-JOURNAL` backed by the same
    file, the [`JOURNAL-STATE`][03de] is not `:NEW`, but the file doesn't exist,
    then the existing object is **invalidated**: attempts to write will
    fail with `JOURNAL-ERROR`. If the existing journal object is being
    written, then invalidation fails with a `JOURNAL-ERROR`. After
    invalidation, a new `FILE-JOURNAL` object is created.

<a id="x-28JOURNAL-3APATHNAME-OF-20-28MGL-PAX-3AREADER-20JOURNAL-3AFILE-JOURNAL-29-29"></a>
- [reader] **PATHNAME-OF** *FILE-JOURNAL (:PATHNAME)*

    The pathname of the file backing the journal.

<a id="x-28JOURNAL-3A-40PPRINT-JOURNALS-20MGL-PAX-3ASECTION-29"></a>
### 14.4 Pretty-printing journals

<a id="x-28JOURNAL-3APPRINT-JOURNAL-20CLASS-29"></a>
- [class] **PPRINT-JOURNAL** *[JOURNAL][5082]*

    Events written to a `PPRINT-JOURNAL` have a
    customizable output format. `PPRINT-JOURNAL`s are intended for
    producing prettier output for [Logging][4e53] and [Tracing][e03f], but they do not
    support reads, so they cannot be used as a [`REPLAY-JOURNAL`][838b] or in
    [`LIST-EVENTS`][0c1b], for example. On the other hand, events written to
    `PPRINT-JOURNAL`s need not be [readable][768f].

<a id="x-28JOURNAL-3AMAKE-PPRINT-JOURNAL-20FUNCTION-29"></a>
- [function] **MAKE-PPRINT-JOURNAL** *&KEY (STREAM (MAKE-SYNONYM-STREAM '\*STANDARD-OUTPUT\*)) (PRETTY T) (PRETTIFIER 'PRETTIFY-EVENT) LOG-DECORATOR*

    Creates a [`PPRINT-JOURNAL`][9150].

<a id="x-28JOURNAL-3APPRINT-JOURNAL-STREAM-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3APPRINT-JOURNAL-29-29"></a>
- [accessor] **PPRINT-JOURNAL-STREAM** *PPRINT-JOURNAL (:STREAM = \*STANDARD-OUTPUT\*)*

    The stream where events are dumped. May be set any
    time to another [`STREAM`][cbf2].

<a id="x-28JOURNAL-3APPRINT-JOURNAL-PRETTY-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3APPRINT-JOURNAL-29-29"></a>
- [accessor] **PPRINT-JOURNAL-PRETTY** *PPRINT-JOURNAL (:PRETTY = T)*

    Whether to use [`PPRINT-JOURNAL-PRETTIFIER`][853d] or write
    events in as the property lists they are. A
    [boolean-valued symbol][62678].

<a id="x-28JOURNAL-3APPRINT-JOURNAL-PRETTIFIER-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3APPRINT-JOURNAL-29-29"></a>
- [accessor] **PPRINT-JOURNAL-PRETTIFIER** *PPRINT-JOURNAL (:PRETTIFIER = 'PRETTIFY-EVENT)*

    A function like [`PRETTIFY-EVENT`][11b7] that writes an
    event to a stream. Only used when [`PPRINT-JOURNAL-PRETTY`][610f], this is
    the output format customization knob. Also see [decoration][1d11]s.

<a id="x-28JOURNAL-3A-40BUNDLES-REFERENCE-20MGL-PAX-3ASECTION-29"></a>
## 15 Bundles reference

In [Bundles][260d], we covered the repeated replay problem that
[`WITH-BUNDLE`][12a5] automates. Here, we provide a reference for the bundle
classes.

<a id="x-28JOURNAL-3ABUNDLE-20CLASS-29"></a>
- [class] **BUNDLE**

    A `BUNDLE` consists of a sequence of journals which
    are all reruns of the same code, hopefully making more and more
    progress towards completion. These journals are [Replay][041c]s of the
    previous successful one, extending it with new events. Upon
    replay (see [`WITH-BUNDLE`][12a5]), the latest journal in the bundle in
    [`JOURNAL-STATE`][03de] `:COMPLETED` plays the role of the replay journal, and a
    new journal is added to the bundle for recording. If the replay
    succeeds, this new journal eventually becomes `:COMPLETED` and takes
    over the role of the replay journal for future replays until another
    replay succeeds. When the bundle is created and it has no journals
    yet, the replay journal is an empty, completed one.
    
    This is an abstract base class. Direct subclasses are
    [`IN-MEMORY-BUNDLE`][bacd] and [`FILE-BUNDLE`][18955].

<a id="x-28JOURNAL-3AMAX-N-FAILED-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3ABUNDLE-29-29"></a>
- [accessor] **MAX-N-FAILED** *BUNDLE (:MAX-N-FAILED = 1)*

    If `MAX-N-FAILED` is non-NIL, and the number of
    journals of [`JOURNAL-STATE`][03de] `:FAILED` in the bundle exceeds
    its value, then some journals (starting with the oldest) are
    deleted.

<a id="x-28JOURNAL-3AMAX-N-COMPLETED-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3ABUNDLE-29-29"></a>
- [accessor] **MAX-N-COMPLETED** *BUNDLE (:MAX-N-COMPLETED = 1)*

    If `MAX-N-COMPLETED` is non-NIL, and the number of
    journals of [`JOURNAL-STATE`][03de] `:COMPLETED` in the bundle exceeds
    its value, then some journals (starting with the oldest) are
    deleted.

<a id="x-28JOURNAL-3A-40IN-MEMORY-BUNDLES-20MGL-PAX-3ASECTION-29"></a>
### 15.1 In-memory bundles

<a id="x-28JOURNAL-3AIN-MEMORY-BUNDLE-20CLASS-29"></a>
- [class] **IN-MEMORY-BUNDLE** *[BUNDLE][d9b6]*

    An `IN-MEMORY-BUNDLE` is a [`BUNDLE`][d9b6] that is built on
    [`IN-MEMORY-JOURNAL`][b668]s. `IN-MEMORY-BUNDLE`s have limited utility as a
    persistence mechanism and are provided mainly for reasons of
    symmetry and for testing. See
    [Synchronization with in-memory journals][12ff] for an example of how to
    achieve persistence without bundles.

<a id="x-28JOURNAL-3AMAKE-IN-MEMORY-BUNDLE-20FUNCTION-29"></a>
- [function] **MAKE-IN-MEMORY-BUNDLE** *&KEY (MAX-N-FAILED 1) (MAX-N-COMPLETED 1) SYNC SYNC-FN*

    Create a new [`IN-MEMORY-BUNDLE`][bacd] with [`MAX-N-FAILED`][1dc2] and [`MAX-N-COMPLETED`][8073]. `SYNC` and `SYNC-FN`
    are passed on to [`MAKE-IN-MEMORY-JOURNAL`][9955].

<a id="x-28JOURNAL-3A-40FILE-BUNDLES-20MGL-PAX-3ASECTION-29"></a>
### 15.2 File bundles

<a id="x-28JOURNAL-3AFILE-BUNDLE-20CLASS-29"></a>
- [class] **FILE-BUNDLE** *[BUNDLE][d9b6]*

    A `FILE-BUNDLE` is a [`BUNDLE`][d9b6] that is built on
    [`FILE-JOURNAL`][8428]s. It provides easy replay-based persistence.

<a id="x-28JOURNAL-3ADIRECTORY-OF-20-28MGL-PAX-3AREADER-20JOURNAL-3AFILE-BUNDLE-29-29"></a>
- [reader] **DIRECTORY-OF** *FILE-BUNDLE (:DIRECTORY)*

    The directory where the files backing the
    [`FILE-JOURNAL`][8428]s in the [`FILE-BUNDLE`][18955] are kept.

<a id="x-28JOURNAL-3AMAKE-FILE-BUNDLE-20FUNCTION-29"></a>
- [function] **MAKE-FILE-BUNDLE** *DIRECTORY &KEY (MAX-N-FAILED 1) (MAX-N-COMPLETED 1) SYNC*

    Return a [`FILE-BUNDLE`][18955] object backed by [`FILE-JOURNAL`][8428]s in `DIRECTORY`.
    See [`MAX-N-FAILED`][1dc2] and
    [`MAX-N-COMPLETED`][8073]. For a description of `SYNC`, see
    [Synchronization strategies][f532].
    
    If there is already a `FILE-BUNDLE` with the same directory (according
    to [`TRUENAME`][09e7]), return that object is returned if it has the same
    `MAX-N-FAILED`, `MAX-N-COMPLETED` and `SYNC` options, else [`JOURNAL-ERROR`][0002]
    is signalled.

<a id="x-28JOURNAL-3ADELETE-FILE-BUNDLE-20FUNCTION-29"></a>
- [function] **DELETE-FILE-BUNDLE** *DIRECTORY*

    Delete all journal files (`*.jrn`) from `DIRECTORY`. Delete the
    directory if empty after the journal files were deleted, else signal
    an error. Existing [`FILE-BUNDLE`][18955] objects are not updated, so
    [`MAKE-FILE-JOURNAL`][f0e7] with FORCE-RELOAD may be required.

<a id="x-28JOURNAL-3A-40STREAMLETS-REFERENCE-20MGL-PAX-3ASECTION-29"></a>
## 16 Streamlets reference

This section is relevant mostly for implementing new kinds of
[`JOURNAL`][5082]s in addition to [`FILE-JOURNAL`][8428]s and [`IN-MEMORY-JOURNAL`][b668]s. In
normal operation, [`STREAMLET`][7a2f]s are not worked with directly.

<a id="x-28JOURNAL-3A-40OPENING-AND-CLOSING-20MGL-PAX-3ASECTION-29"></a>
### 16.1 Opening and closing

<a id="x-28JOURNAL-3ASTREAMLET-20CLASS-29"></a>
- [class] **STREAMLET**

    A `STREAMLET` is a handle to perform I/O on a
    [`JOURNAL`][5082]. The high-level stuff ([`WITH-JOURNALING`][6131], [`JOURNALED`][6267], etc) is
    built on top of streamlets.

<a id="x-28JOURNAL-3AJOURNAL-20-28MGL-PAX-3AREADER-20JOURNAL-3ASTREAMLET-29-29"></a>
- [reader] **JOURNAL** *STREAMLET (:JOURNAL)*

    The `JOURNAL` that was passed to [`OPEN-STREAMLET`][9d79].
    This is the journal `STREAMLET` operates on.

<a id="x-28JOURNAL-3AOPEN-STREAMLET-20GENERIC-FUNCTION-29"></a>
- [generic-function] **OPEN-STREAMLET** *JOURNAL &KEY DIRECTION*

    Return a [`STREAMLET`][7a2f] suitable for performing I/O on
    `JOURNAL`. `DIRECTION` (defaults to `:INPUT`) is one of `:INPUT`, `:OUTPUT`,
    `:IO`, and it has the same purpose as the similarly named argument of
    [`CL:OPEN`][117a].

<a id="x-28JOURNAL-3ACLOSE-STREAMLET-20GENERIC-FUNCTION-29"></a>
- [generic-function] **CLOSE-STREAMLET** *STREAMLET*

    Close `STREAMLET`, which was returned by
    [`OPEN-STREAMLET`][9d79]. After closing, `STREAMLET` may not longer be used for
    IO.

<a id="x-28JOURNAL-3AMAKE-STREAMLET-FINALIZER-20GENERIC-FUNCTION-29"></a>
- [generic-function] **MAKE-STREAMLET-FINALIZER** *STREAMLET*

    Return `NIL` or a function of no arguments suitable
    as a finalizer for `STREAMLET`. That is, a function that closes
    `STREAMLET` but holds no reference to it. This is intended for
    streamlets that are not dynamic-extent, so using [`WITH-OPEN-JOURNAL`][6d64]
    is not appropriate.

<a id="x-28JOURNAL-3AOPEN-STREAMLET-P-20GENERIC-FUNCTION-29"></a>
- [generic-function] **OPEN-STREAMLET-P** *STREAMLET*

    Return true if `STREAMLET` is open. `STREAMLET`s are
    open until they have been explicitly closed with [`CLOSE-STREAMLET`][7e9f].

<a id="x-28JOURNAL-3AINPUT-STREAMLET-P-20FUNCTION-29"></a>
- [function] **INPUT-STREAMLET-P** *STREAMLET*

    See if `STREAMLET` was opened for input (the `DIRECTION` argument of
    [`OPEN-STREAMLET`][9d79] was `:INPUT` or `:IO`).

<a id="x-28JOURNAL-3AOUTPUT-STREAMLET-P-20FUNCTION-29"></a>
- [function] **OUTPUT-STREAMLET-P** *STREAMLET*

    See if `STREAMLET` was opened for input (the `DIRECTION` argument of
    [`OPEN-STREAMLET`][9d79] was `:OUTPUT` or `:IO`).

<a id="x-28JOURNAL-3AWITH-OPEN-JOURNAL-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-OPEN-JOURNAL** *(VAR JOURNAL &KEY (DIRECTION :INPUT)) &BODY BODY*

    This is like [`WITH-OPEN-FILE`][8bdf] but for `JOURNAL`s.
    Open the journal designated by `JOURNAL` (see [`TO-JOURNAL`][e8ed]) with
    [`OPEN-STREAMLET`][9d79], passing `DIRECTION` along, and bind `VAR` to the
    resulting [`STREAMLET`][7a2f]. Call [`CLOSE-STREAMLET`][7e9f] after `BODY` finishes. If
    `JOURNAL` is `NIL`, then `VAR` is bound to `NIL` and no streamlet is
    created.

<a id="x-28JOURNAL-3ASTREAMLET-ERROR-20CONDITION-29"></a>
- [condition] **STREAMLET-ERROR** *ERROR*

    Like [`CL:STREAM-ERROR:`][432c] failures pertaining to I/O on
    a closed [`STREAMLET`][7a2f] or of the wrong `DIRECTION`. Actual I/O errors are
    *not* encapsulated in `STREAMLET-ERROR`.

<a id="x-28JOURNAL-3A-40READING-FROM-STREAMLETS-20MGL-PAX-3ASECTION-29"></a>
### 16.2 Reading from streamlets

<a id="x-28JOURNAL-3AREAD-EVENT-20GENERIC-FUNCTION-29"></a>
- [generic-function] **READ-EVENT** *STREAMLET &OPTIONAL EOJ-ERROR-P*

    Read the event at the current read position from
    `STREAMLET`, and move the read position to the event after. If there
    are no more events, signal [`END-OF-JOURNAL`][3cdb] or return `NIL` depending on
    `EOJ-ERROR-P`. Signals [`STREAMLET-ERROR`][e6b2] if `STREAMLET` is not
    [`INPUT-STREAMLET-P`][b292] or not [`OPEN-STREAMLET-P`][5da8].

<a id="x-28JOURNAL-3AREAD-POSITION-20GENERIC-FUNCTION-29"></a>
- [generic-function] **READ-POSITION** *STREAMLET*

    Return an integer that identifies the position of
    the next event to be read from `STREAMLET`. [`SETF`][17b7]able, see
    [`SET-READ-POSITION`][f932].

<a id="x-28JOURNAL-3ASET-READ-POSITION-20GENERIC-FUNCTION-29"></a>
- [generic-function] **SET-READ-POSITION** *STREAMLET POSITION*

    Set the read position of `STREAMLET` to `POSITION`,
    which must have been acquired from [`READ-POSITION`][6e60].

<a id="x-28JOURNAL-3ASAVE-EXCURSION-20MGL-PAX-3AMACRO-29"></a>
- [macro] **SAVE-EXCURSION** *(STREAMLET) &BODY BODY*

    Save [`READ-POSITION`][6e60] of `STREAMLET`, execute `BODY`, and make sure to
    restore the saved read position.

<a id="x-28JOURNAL-3APEEK-EVENT-20GENERIC-FUNCTION-29"></a>
- [generic-function] **PEEK-EVENT** *STREAMLET*

    Read the next event from `STREAMLET` without changing
    the read position, or return `NIL` if there is no event to be read.

<a id="x-28JOURNAL-3APEEK-EVENT-20-28METHOD-20NIL-20-28JOURNAL-3ASTREAMLET-29-29-29"></a>
- [method] **PEEK-EVENT** *(STREAMLET STREAMLET)*

    This is a slow default implementation, which relies on
    [`SAVE-EXCURSION`][b283] and [`READ-EVENT`][adcf].

<a id="x-28JOURNAL-3A-40WRITING-TO-STREAMLETS-20MGL-PAX-3ASECTION-29"></a>
### 16.3 Writing to streamlets

<a id="x-28JOURNAL-3AWRITE-EVENT-20GENERIC-FUNCTION-29"></a>
- [generic-function] **WRITE-EVENT** *EVENT STREAMLET*

    Write `EVENT` to `STREAMLET`.
    Writing always happens at the end of `STREAMLET`'s journal regardless
    of the [`READ-POSITION`][6e60], and the read position is not changed. Signals
    [`STREAMLET-ERROR`][e6b2] if `STREAMLET` is not [`OUTPUT-STREAMLET-P`][956a] or not
    [`OPEN-STREAMLET-P`][5da8].

<a id="x-28JOURNAL-3AWRITE-EVENT-20-28METHOD-20NIL-20-28T-20JOURNAL-3AJOURNAL-29-29-29"></a>
- [method] **WRITE-EVENT** *EVENT (JOURNAL JOURNAL)*

    For convenience, it is possible to write directly to a `JOURNAL`,
    in which case the journal's internal output streamlet is used.
    This internal streamlet is opened for `:OUTPUT` and may be used by
    [`:LOG-RECORD`][a6ac].

<a id="x-28JOURNAL-3AWRITE-POSITION-20GENERIC-FUNCTION-29"></a>
- [generic-function] **WRITE-POSITION** *STREAMLET*

    Return an integer that identifies the position of
    the next event to be written to `STREAMLET`.

<a id="x-28JOURNAL-3AREQUEST-COMPLETED-ON-ABORT-20GENERIC-FUNCTION-29"></a>
- [generic-function] **REQUEST-COMPLETED-ON-ABORT** *STREAMLET*

    Make it so that upon [aborted execution][78fd],
    `STREAMLET`'s [`JOURNAL`][5082] will be in [`JOURNAL-STATE`][03de] `:COMPLETED` when loaded
    fresh (e.g. when creating a [`FILE-JOURNAL`][8428] with an existing file). Any
    previously written events must be persisted before making this
    change. Before `REQUEST-COMPLETED-ON-ABORT` is called, a journal must
    be reloaded in state `:FAILED`.
    
    It is permissible to defer carrying out this request until the next
    [`SYNC-STREAMLET`][c9d6] call. If the request was carried out, return true. If
    it was deferred, return `NIL`.

<a id="x-28JOURNAL-3ASYNC-STREAMLET-20GENERIC-FUNCTION-29"></a>
- [generic-function] **SYNC-STREAMLET** *STREAMLET*

    Durably persist the effects of all preceding
    [`WRITE-EVENT`][01fd] calls made via `STREAMLET` to its journal and any deferred
    [`REQUEST-COMPLETED-ON-ABORT`][068a] in this order.

<a id="x-28JOURNAL-3A-40JOURNAL-2FGLOSSARY-20MGL-PAX-3ASECTION-29"></a>
## 17 Glossary

<a id="x-28JOURNAL-3A-40ASYNC-UNWIND-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **async-unwind**

    If an asynchronous event, say a `SIGINT` triggered by `C-c`, is
    delivered to a thread running Lisp or foreign code called from Lisp,
    a Lisp condition is typically signalled. If the handler for this
    condition unwinds the stack, then we have an asynchronous unwind.
    Another example is `BT:INTERRUPT-THREAD`, which, as it can execute
    arbitrary code, may unwind the stack in the target thread.

<a id="x-28JOURNAL-3A-40BOOLEAN-VALUED-SYMBOL-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **boolean-valued symbol**

    Imagine writing two [`STREAM`][cbf2]s with a spaghetti of functions and
    wanting to have pretty-printed output on one of them. Unfortunately,
    binding [`*PRINT-PRETTY*`][4747] to `T` will affect writes to both streams.
    
    One solution would be to have streams look up their own print-pretty
    flag with `(SYMBOL-VALUE (STREAM-PRETTY-PRINT STREAM))` and have the
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
    
    The above hypothetical example demonstrates the concept of
    boolean-valued symbols on `CL:STREAM`s. In Journal, they are used by
    [`MAKE-LOG-DECORATOR`][e33e] and [`PPRINT-JOURNAL`][9150]s.

<a id="x-28JOURNAL-3A-40NON-LOCAL-EXIT-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **non-local exit**

    This is a term from the Common Lisp ANSI standard. If a form does
    not return normally, but control is transferred via [`GO`][7b73], [`RETURN`][35ff],
    [`RETURN-FROM`][b0a1] or [`THROW`][3b05], then it is said to have performed a non-local
    exit. This definition of a non-local exit includes `EVENT-EXIT`([`0`][c04d] [`1`][812a])
    `:CONDITION`, `:ERROR` and `:NLX`.

<a id="x-28JOURNAL-3A-40READABLE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>
- [glossary-term] **readable**

    In Common Lisp, readable objects are those that can be printed readably.
    Anything written to stream-based journals needs to be readable.

  [0002]: #x-28JOURNAL-3AJOURNAL-ERROR-20CONDITION-29 "JOURNAL:JOURNAL-ERROR CONDITION"
  [0114]: #x-28JOURNAL-3A-40JOURNAL-BACKGROUND-20MGL-PAX-3ASECTION-29 "Background"
  [01fd]: #x-28JOURNAL-3AWRITE-EVENT-20GENERIC-FUNCTION-29 "JOURNAL:WRITE-EVENT GENERIC-FUNCTION"
  [026a]: http://www.lispworks.com/documentation/HyperSpec/Body/f_parse_.htm "PARSE-INTEGER FUNCTION"
  [03de]: #x-28JOURNAL-3AJOURNAL-STATE-20TYPE-29 "JOURNAL:JOURNAL-STATE TYPE"
  [041c]: #x-28JOURNAL-3A-40REPLAY-20MGL-PAX-3ASECTION-29 "Replay"
  [046e]: #x-28JOURNAL-3A-40SYNCHRONIZATION-20MGL-PAX-3ASECTION-29 "Synchronization to storage"
  [068a]: #x-28JOURNAL-3AREQUEST-COMPLETED-ON-ABORT-20GENERIC-FUNCTION-29 "JOURNAL:REQUEST-COMPLETED-ON-ABORT GENERIC-FUNCTION"
  [06a7]: #x-28JOURNAL-3A-40BLOCK-20MGL-PAX-3AGLOSSARY-TERM-29 "JOURNAL:@BLOCK MGL-PAX:GLOSSARY-TERM"
  [0752]: #x-28JOURNAL-3AJOURNAL-SYNC-20-28MGL-PAX-3AREADER-20JOURNAL-3AJOURNAL-29-29 "JOURNAL:JOURNAL-SYNC (MGL-PAX:READER JOURNAL:JOURNAL)"
  [09e7]: http://www.lispworks.com/documentation/HyperSpec/Body/f_tn.htm "TRUENAME FUNCTION"
  [0c1b]: #x-28JOURNAL-3ALIST-EVENTS-20FUNCTION-29 "JOURNAL:LIST-EVENTS FUNCTION"
  [0cce]: #x-28JOURNAL-3AWITH-REPLAY-FILTER-20MGL-PAX-3AMACRO-29 "JOURNAL:WITH-REPLAY-FILTER MGL-PAX:MACRO"
  [0e53]: #x-28JOURNAL-3AEXTERNAL-EVENT-20TYPE-29 "JOURNAL:EXTERNAL-EVENT TYPE"
  [0fdb]: #x-28JOURNAL-3AREPLAY-VERSION-DOWNGRADE-20CONDITION-29 "JOURNAL:REPLAY-VERSION-DOWNGRADE CONDITION"
  [10c8]: #x-28JOURNAL-3AREPLAY-FORCE-UPGRADE-20RESTART-29 "JOURNAL:REPLAY-FORCE-UPGRADE RESTART"
  [117a]: http://www.lispworks.com/documentation/HyperSpec/Body/f_open.htm "OPEN FUNCTION"
  [11b7]: #x-28JOURNAL-3APRETTIFY-EVENT-20FUNCTION-29 "JOURNAL:PRETTIFY-EVENT FUNCTION"
  [1256]: #x-28JOURNAL-3AREPLAY-ARGS-MISMATCH-20CONDITION-29 "JOURNAL:REPLAY-ARGS-MISMATCH CONDITION"
  [12a5]: #x-28JOURNAL-3AWITH-BUNDLE-20MGL-PAX-3AMACRO-29 "JOURNAL:WITH-BUNDLE MGL-PAX:MACRO"
  [12ff]: #x-28JOURNAL-3A-40SYNCHRONIZATION-WITH-IN-MEMORY-JOURNALS-20MGL-PAX-3ASECTION-29 "Synchronization with in-memory journals"
  [1496]: #x-28JOURNAL-3A-40PPRINT-JOURNALS-20MGL-PAX-3ASECTION-29 "Pretty-printing journals"
  [14f7]: #x-28JOURNAL-3A-40WRITING-TO-STREAMLETS-20MGL-PAX-3ASECTION-29 "Writing to streamlets"
  [1729]: #x-28JOURNAL-3AIN-EVENT-20TYPE-29 "JOURNAL:IN-EVENT TYPE"
  [17b7]: http://www.lispworks.com/documentation/HyperSpec/Body/m_setf.htm "SETF MGL-PAX:MACRO"
  [186b]: #x-28JOURNAL-3A-40IN-EVENTS-20MGL-PAX-3ASECTION-29 "In-events"
  [1895]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR CONDITION"
  [18955]: #x-28JOURNAL-3AFILE-BUNDLE-20CLASS-29 "JOURNAL:FILE-BUNDLE CLASS"
  [18be]: #x-28JOURNAL-3AJTRACE-20MGL-PAX-3AMACRO-29 "JOURNAL:JTRACE MGL-PAX:MACRO"
  [1d0d]: #x-28JOURNAL-3A-40JOURNAL-PORTABILITY-20MGL-PAX-3ASECTION-29 "Portability"
  [1d11]: #x-28JOURNAL-3A-40DECORATION-20MGL-PAX-3AGLOSSARY-TERM-29 "JOURNAL:@DECORATION MGL-PAX:GLOSSARY-TERM"
  [1dc2]: #x-28JOURNAL-3AMAX-N-FAILED-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3ABUNDLE-29-29 "JOURNAL:MAX-N-FAILED (MGL-PAX:ACCESSOR JOURNAL:BUNDLE)"
  [1f28]: http://www.lispworks.com/documentation/HyperSpec/Body/f_format.htm "FORMAT FUNCTION"
  [1f5f]: #x-28JOURNAL-3AEVENT-VERSION-20FUNCTION-29 "JOURNAL:EVENT-VERSION FUNCTION"
  [228c]: #x-28JOURNAL-3AJOURNAL-REPLAY-MISMATCH-20-28MGL-PAX-3AREADER-20JOURNAL-3AJOURNAL-29-29 "JOURNAL:JOURNAL-REPLAY-MISMATCH (MGL-PAX:READER JOURNAL:JOURNAL)"
  [23c4]: #x-28JOURNAL-3ALOGGED-20MGL-PAX-3AMACRO-29 "JOURNAL:LOGGED MGL-PAX:MACRO"
  [241f]: http://www.lispworks.com/documentation/HyperSpec/Body/t_pn.htm "PATHNAME TYPE"
  [260d]: #x-28JOURNAL-3A-40BUNDLES-20MGL-PAX-3ASECTION-29 "Bundles"
  [2765]: #x-28JOURNAL-3A-2ATRACE-TIME-2A-20VARIABLE-29 "JOURNAL:*TRACE-TIME* VARIABLE"
  [2933]: #x-28JOURNAL-3A-40REPLAY-FAILURES-20MGL-PAX-3ASECTION-29 "Replay failures"
  [297c]: #x-28JOURNAL-3A-40CUSTOMIZING-LOGS-20MGL-PAX-3ASECTION-29 "Customizing logs"
  [2e9b]: #x-28JOURNAL-3AREPLAY-FAILURE-20CONDITION-29 "JOURNAL:REPLAY-FAILURE CONDITION"
  [3153]: #x-28JOURNAL-3ADEFINE-INVOKED-20MGL-PAX-3AMACRO-29 "JOURNAL:DEFINE-INVOKED MGL-PAX:MACRO"
  [3335]: #x-28JOURNAL-3AEVENT-ARGS-20FUNCTION-29 "JOURNAL:EVENT-ARGS FUNCTION"
  [3439]: #x-28JOURNAL-3AMAKE-OUT-EVENT-20FUNCTION-29 "JOURNAL:MAKE-OUT-EVENT FUNCTION"
  [34a8]: #x-28JOURNAL-3APPRINT-JOURNAL-STREAM-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3APPRINT-JOURNAL-29-29 "JOURNAL:PPRINT-JOURNAL-STREAM (MGL-PAX:ACCESSOR JOURNAL:PPRINT-JOURNAL)"
  [35ff]: http://www.lispworks.com/documentation/HyperSpec/Body/m_return.htm "RETURN MGL-PAX:MACRO"
  [37c4]: #x-28JOURNAL-3A-40PERSISTENCE-20MGL-PAX-3ASECTION-29 "Persistence"
  [392c]: #x-28JOURNAL-3A-40ASYNC-UNWIND-20MGL-PAX-3AGLOSSARY-TERM-29 "JOURNAL:@ASYNC-UNWIND MGL-PAX:GLOSSARY-TERM"
  [3956]: #x-28JOURNAL-3AJOURNALING-FAILURE-20CONDITION-29 "JOURNAL:JOURNALING-FAILURE CONDITION"
  [3ac1]: #x-28JOURNAL-3A-40VALUES-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29 "JOURNAL:@VALUES-OUTCOME MGL-PAX:GLOSSARY-TERM"
  [3b05]: http://www.lispworks.com/documentation/HyperSpec/Body/s_throw.htm "THROW MGL-PAX:MACRO"
  [3b63]: #x-28JOURNAL-3ARECORD-JOURNAL-20FUNCTION-29 "JOURNAL:RECORD-JOURNAL FUNCTION"
  [3b76]: #x-28JOURNAL-3A-40NON-LOCAL-EXIT-20MGL-PAX-3AGLOSSARY-TERM-29 "JOURNAL:@NON-LOCAL-EXIT MGL-PAX:GLOSSARY-TERM"
  [3c21]: #x-28JOURNAL-3A-40MATCHING-IN-EVENTS-20MGL-PAX-3ASECTION-29 "Matching in-events"
  [3ca4]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*TRACE-OUTPUT* VARIABLE"
  [3caf]: http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_1.htm "SYMBOL-FUNCTION FUNCTION"
  [3cdb]: #x-28JOURNAL-3AEND-OF-JOURNAL-20CONDITION-29 "JOURNAL:END-OF-JOURNAL CONDITION"
  [3d3c]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_rd.htm "READ FUNCTION"
  [4118]: #x-28JOURNAL-3A-2ATRACE-JOURNAL-2A-20VARIABLE-29 "JOURNAL:*TRACE-JOURNAL* VARIABLE"
  [4212]: #x-28JOURNAL-3A-40INVOKED-20MGL-PAX-3AGLOSSARY-TERM-29 "JOURNAL:@INVOKED MGL-PAX:GLOSSARY-TERM"
  [42a5]: #x-28JOURNAL-3A-40JOURNAL-SLIME-INTEGRATION-20MGL-PAX-3ASECTION-29 "Slime integration"
  [432c]: http://www.lispworks.com/documentation/HyperSpec/Body/e_stm_er.htm "STREAM-ERROR CONDITION"
  [43ad]: http://www.lispworks.com/documentation/HyperSpec/Body/t_values.htm "VALUES TYPE"
  [4657]: #x-28JOURNAL-3A-40EXPECTED-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29 "JOURNAL:@EXPECTED-OUTCOME MGL-PAX:GLOSSARY-TERM"
  [4747]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_pre.htm "*PRINT-PRETTY* VARIABLE"
  [47a7]: #x-28JOURNAL-3A-40PRETTY-PRINTING-20MGL-PAX-3ASECTION-29 "Pretty-printing"
  [4823]: http://www.lispworks.com/documentation/HyperSpec/Body/m_tracec.htm "UNTRACE MGL-PAX:MACRO"
  [48ef]: #x-28JOURNAL-3A-40OUT-EVENTS-20MGL-PAX-3ASECTION-29 "Out-events"
  [48f5]: #x-28JOURNAL-3A-40JOURNAL-2FGLOSSARY-20MGL-PAX-3ASECTION-29 "Glossary"
  [4a00]: #x-28JOURNAL-3AIDENTICAL-JOURNALS-P-20GENERIC-FUNCTION-29 "JOURNAL:IDENTICAL-JOURNALS-P GENERIC-FUNCTION"
  [4a9f]: #x-28-22journal-22-20ASDF-2FSYSTEM-3ASYSTEM-29 '"journal" ASDF/SYSTEM:SYSTEM'
  [4c2b]: #x-28JOURNAL-3AVERSIONED-EVENT-20TYPE-29 "JOURNAL:VERSIONED-EVENT TYPE"
  [4e53]: #x-28JOURNAL-3A-40LOGGING-20MGL-PAX-3ASECTION-29 "Logging"
  [4f2b]: #x-28JOURNAL-3ADATA-EVENT-LOSSAGE-20CONDITION-29 "JOURNAL:DATA-EVENT-LOSSAGE CONDITION"
  [5082]: #x-28JOURNAL-3AJOURNAL-20CLASS-29 "JOURNAL:JOURNAL CLASS"
  [51ce]: #x-28JOURNAL-3ALOG-EVENT-20TYPE-29 "JOURNAL:LOG-EVENT TYPE"
  [548d]: http://www.lispworks.com/documentation/HyperSpec/Body/m_tracec.htm "TRACE MGL-PAX:MACRO"
  [54c1]: #x-28JOURNAL-3A-40EVENT-VERSIONS-20MGL-PAX-3ASECTION-29 "Event versions"
  [560b]: #x-28JOURNAL-3A-40ERROR-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29 "JOURNAL:@ERROR-OUTCOME MGL-PAX:GLOSSARY-TERM"
  [5833]: #x-28JOURNAL-3APPRINT-EVENTS-20FUNCTION-29 "JOURNAL:PPRINT-EVENTS FUNCTION"
  [5cd1]: #x-28JOURNAL-3ALEAF-EVENT-20TYPE-29 "JOURNAL:LEAF-EVENT TYPE"
  [5d05]: #x-28JOURNAL-3AFRAMED-20MGL-PAX-3AMACRO-29 "JOURNAL:FRAMED MGL-PAX:MACRO"
  [5da8]: #x-28JOURNAL-3AOPEN-STREAMLET-P-20GENERIC-FUNCTION-29 "JOURNAL:OPEN-STREAMLET-P GENERIC-FUNCTION"
  [5e0a]: #x-28JOURNAL-3AJOURNAL-EVENTS-20-28MGL-PAX-3AREADER-20JOURNAL-3AIN-MEMORY-JOURNAL-29-29 "JOURNAL:JOURNAL-EVENTS (MGL-PAX:READER JOURNAL:IN-MEMORY-JOURNAL)"
  [610f]: #x-28JOURNAL-3APPRINT-JOURNAL-PRETTY-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3APPRINT-JOURNAL-29-29 "JOURNAL:PPRINT-JOURNAL-PRETTY (MGL-PAX:ACCESSOR JOURNAL:PPRINT-JOURNAL)"
  [6131]: #x-28JOURNAL-3AWITH-JOURNALING-20MGL-PAX-3AMACRO-29 "JOURNAL:WITH-JOURNALING MGL-PAX:MACRO"
  [6169]: #x-28JOURNAL-3A-40PERSISTENCE-TUTORIAL-20MGL-PAX-3ASECTION-29 "Persistence tutorial"
  [6267]: #x-28JOURNAL-3AJOURNALED-20MGL-PAX-3AMACRO-29 "JOURNAL:JOURNALED MGL-PAX:MACRO"
  [62678]: #x-28JOURNAL-3A-40BOOLEAN-VALUED-SYMBOL-20MGL-PAX-3AGLOSSARY-TERM-29 "JOURNAL:@BOOLEAN-VALUED-SYMBOL MGL-PAX:GLOSSARY-TERM"
  [637d]: #x-28JOURNAL-3AOUT-EVENT-20TYPE-29 "JOURNAL:OUT-EVENT TYPE"
  [63d3]: http://www.lispworks.com/documentation/HyperSpec/Body/e_storag.htm "STORAGE-CONDITION CONDITION"
  [6525]: #x-28JOURNAL-3A-40REPLAY-EVENT-20MGL-PAX-3AGLOSSARY-TERM-29 "JOURNAL:@REPLAY-EVENT MGL-PAX:GLOSSARY-TERM"
  [6699]: #x-28JOURNAL-3AREPLAY-UNEXPECTED-OUTCOME-20CONDITION-29 "JOURNAL:REPLAY-UNEXPECTED-OUTCOME CONDITION"
  [6710]: #x-28JOURNAL-3AREPLAY-NAME-MISMATCH-20CONDITION-29 "JOURNAL:REPLAY-NAME-MISMATCH CONDITION"
  [674f]: #x-28JOURNAL-3A-40SYNCHRONIZATION-WITH-FILE-JOURNALS-20MGL-PAX-3ASECTION-29 "Synchronization with file journals"
  [68eb]: #x-28JOURNAL-3A-40NLX-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29 "JOURNAL:@NLX-OUTCOME MGL-PAX:GLOSSARY-TERM"
  [6ab5]: #x-28JOURNAL-3AFLET-INVOKED-20MGL-PAX-3AMACRO-29 "JOURNAL:FLET-INVOKED MGL-PAX:MACRO"
  [6d64]: #x-28JOURNAL-3AWITH-OPEN-JOURNAL-20MGL-PAX-3AMACRO-29 "JOURNAL:WITH-OPEN-JOURNAL MGL-PAX:MACRO"
  [6e60]: #x-28JOURNAL-3AREAD-POSITION-20GENERIC-FUNCTION-29 "JOURNAL:READ-POSITION GENERIC-FUNCTION"
  [712a]: #x-28JOURNAL-3AEQUIVALENT-REPLAY-JOURNALS-P-20GENERIC-FUNCTION-29 "JOURNAL:EQUIVALENT-REPLAY-JOURNALS-P GENERIC-FUNCTION"
  [72cd]: #x-28JOURNAL-3A-40OUT-EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29 "Out-events"
  [73ba]: http://www.lispworks.com/documentation/HyperSpec/Body/f_map.htm "MAP FUNCTION"
  [750a]: #x-28JOURNAL-3A-40UPGRADES-AND-REPLAY-20MGL-PAX-3ASECTION-29 "Upgrades and replay"
  [7682]: #x-28JOURNAL-3A-40TESTING-20MGL-PAX-3ASECTION-29 "Testing"
  [768f]: #x-28JOURNAL-3A-40READABLE-20MGL-PAX-3AGLOSSARY-TERM-29 "JOURNAL:@READABLE MGL-PAX:GLOSSARY-TERM"
  [78fd]: #x-28JOURNAL-3A-40ABORTED-EXECUTION-20MGL-PAX-3AGLOSSARY-TERM-29 "JOURNAL:@ABORTED-EXECUTION MGL-PAX:GLOSSARY-TERM"
  [7991]: #x-28JOURNAL-3A-40REPLAYING-THE-OUTCOME-20MGL-PAX-3ASECTION-29 "Replaying the outcome"
  [7a2f]: #x-28JOURNAL-3ASTREAMLET-20CLASS-29 "JOURNAL:STREAMLET CLASS"
  [7b73]: http://www.lispworks.com/documentation/HyperSpec/Body/s_go.htm "GO MGL-PAX:MACRO"
  [7bf3]: #x-28JOURNAL-3A-40SAFETY-20MGL-PAX-3ASECTION-29 "Safety"
  [7df7]: #x-28JOURNAL-3A-40FRAME-20MGL-PAX-3AGLOSSARY-TERM-29 "JOURNAL:@FRAME MGL-PAX:GLOSSARY-TERM"
  [7e2c]: http://www.lispworks.com/documentation/HyperSpec/Body/f_char_c.htm "CHAR-CODE FUNCTION"
  [7e9f]: #x-28JOURNAL-3ACLOSE-STREAMLET-20GENERIC-FUNCTION-29 "JOURNAL:CLOSE-STREAMLET GENERIC-FUNCTION"
  [7ec9]: #x-28JOURNAL-3AVALUES--3E-20FUNCTION-29 "JOURNAL:VALUES-> FUNCTION"
  [7ef8]: http://www.lispworks.com/documentation/HyperSpec/Body/s_flet_.htm "FLET MGL-PAX:MACRO"
  [7f9d]: #x-28JOURNAL-3A-40MATCHING-OUT-EVENTS-20MGL-PAX-3ASECTION-29 "Matching out-events"
  [8073]: #x-28JOURNAL-3AMAX-N-COMPLETED-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3ABUNDLE-29-29 "JOURNAL:MAX-N-COMPLETED (MGL-PAX:ACCESSOR JOURNAL:BUNDLE)"
  [812a]: #x-28JOURNAL-3AEVENT-EXIT-20TYPE-29 "JOURNAL:EVENT-EXIT TYPE"
  [825c]: #x-28JOURNAL-3A-2ATRACE-PRETTY-2A-20VARIABLE-29 "JOURNAL:*TRACE-PRETTY* VARIABLE"
  [838b]: #x-28JOURNAL-3AREPLAY-JOURNAL-20FUNCTION-29 "JOURNAL:REPLAY-JOURNAL FUNCTION"
  [8428]: #x-28JOURNAL-3AFILE-JOURNAL-20CLASS-29 "JOURNAL:FILE-JOURNAL CLASS"
  [853d]: #x-28JOURNAL-3APPRINT-JOURNAL-PRETTIFIER-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3APPRINT-JOURNAL-29-29 "JOURNAL:PPRINT-JOURNAL-PRETTIFIER (MGL-PAX:ACCESSOR JOURNAL:PPRINT-JOURNAL)"
  [8548]: #x-28JOURNAL-3ARECORD-UNEXPECTED-OUTCOME-20CONDITION-29 "JOURNAL:RECORD-UNEXPECTED-OUTCOME CONDITION"
  [86f6]: #x-28JOURNAL-3A-40LEAF-EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29 "Leaf-events"
  [8804]: http://www.lispworks.com/documentation/HyperSpec/Body/f_identi.htm "IDENTITY FUNCTION"
  [8a5b]: #x-28JOURNAL-3AJOURNAL-LOG-DECORATOR-20-28MGL-PAX-3AACCESSOR-20JOURNAL-3AJOURNAL-29-29 "JOURNAL:JOURNAL-LOG-DECORATOR (MGL-PAX:ACCESSOR JOURNAL:JOURNAL)"
  [8bc1]: #x-28JOURNAL-3A-40IN-MEMORY-BUNDLES-20MGL-PAX-3ASECTION-29 "In-memory bundles"
  [8bdf]: http://www.lispworks.com/documentation/HyperSpec/Body/m_w_open.htm "WITH-OPEN-FILE MGL-PAX:MACRO"
  [8d1e]: #x-28JOURNAL-3AMAKE-IN-MEMORY-BUNDLE-20FUNCTION-29 "JOURNAL:MAKE-IN-MEMORY-BUNDLE FUNCTION"
  [9105]: #x-28JOURNAL-3A-40JOURNAL-FEATURES-20MGL-PAX-3ASECTION-29 "Distinguishing features"
  [9150]: #x-28JOURNAL-3APPRINT-JOURNAL-20CLASS-29 "JOURNAL:PPRINT-JOURNAL CLASS"
  [920f]: http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_5.htm "SYMBOL-VALUE FUNCTION"
  [92aa]: #x-28JOURNAL-3AREPLAY-FORCE-INSERT-20RESTART-29 "JOURNAL:REPLAY-FORCE-INSERT RESTART"
  [9376]: #x-28JOURNAL-3A-40TESTING-ON-MULTIPLE-LEVELS-20MGL-PAX-3ASECTION-29 "Testing on multiple levels"
  [956a]: #x-28JOURNAL-3AOUTPUT-STREAMLET-P-20FUNCTION-29 "JOURNAL:OUTPUT-STREAMLET-P FUNCTION"
  [96d0]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm "EQUAL FUNCTION"
  [9717]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defun.htm "DEFUN MGL-PAX:MACRO"
  [9955]: #x-28JOURNAL-3AMAKE-IN-MEMORY-JOURNAL-20FUNCTION-29 "JOURNAL:MAKE-IN-MEMORY-JOURNAL FUNCTION"
  [9a42]: #x-28JOURNAL-3A-2ATRACE-THREAD-2A-20VARIABLE-29 "JOURNAL:*TRACE-THREAD* VARIABLE"
  [9acb]: http://www.lispworks.com/documentation/HyperSpec/Body/t_syn_st.htm "SYNONYM-STREAM TYPE"
  [9d79]: #x-28JOURNAL-3AOPEN-STREAMLET-20GENERIC-FUNCTION-29 "JOURNAL:OPEN-STREAMLET GENERIC-FUNCTION"
  [9d9f]: #x-28JOURNAL-3A-40CONDITION-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29 "JOURNAL:@CONDITION-OUTCOME MGL-PAX:GLOSSARY-TERM"
  [9daf]: http://www.lispworks.com/documentation/HyperSpec/Body/t_null.htm "NULL TYPE"
  [9ebd]: #x-28JOURNAL-3AMAKE-IN-EVENT-20FUNCTION-29 "JOURNAL:MAKE-IN-EVENT FUNCTION"
  [9ed3]: #x-28JOURNAL-3AEVENT-VERSION-20TYPE-29 "JOURNAL:EVENT-VERSION TYPE"
  [9f84]: #x-28JOURNAL-3AEVENT-NAME-20FUNCTION-29 "JOURNAL:EVENT-NAME FUNCTION"
  [9f90]: #x-28JOURNAL-3AJOURNALING-FAILURE-EMBEDDED-CONDITION-20-28MGL-PAX-3AREADER-20JOURNAL-3AJOURNALING-FAILURE-29-29 "JOURNAL:JOURNALING-FAILURE-EMBEDDED-CONDITION (MGL-PAX:READER JOURNAL:JOURNALING-FAILURE)"
  [a1ab]: http://www.lispworks.com/documentation/HyperSpec/Body/f_pn.htm "PATHNAME FUNCTION"
  [a1d4]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm "EQ FUNCTION"
  [a394]: #x-28JOURNAL-3AEVENT-20TYPE-29 "JOURNAL:EVENT TYPE"
  [a6ac]: #x-28JOURNAL-3A-40LOG-RECORD-20MGL-PAX-3ASECTION-29 "`:LOG-RECORD`"
  [a8a7]: #x-28JOURNAL-3A-40THE-REPLAY-STRATEGY-20MGL-PAX-3ASECTION-29 "The replay strategy"
  [aa14]: #x-28JOURNAL-3A-40JOURNAL-LINKS-20MGL-PAX-3ASECTION-29 "Links"
  [adcd]: #x-28JOURNAL-3A-40READING-FROM-STREAMLETS-20MGL-PAX-3ASECTION-29 "Reading from streamlets"
  [adcf]: #x-28JOURNAL-3AREAD-EVENT-20GENERIC-FUNCTION-29 "JOURNAL:READ-EVENT GENERIC-FUNCTION"
  [b0a1]: http://www.lispworks.com/documentation/HyperSpec/Body/s_ret_fr.htm "RETURN-FROM MGL-PAX:MACRO"
  [b0f7]: http://www.lispworks.com/documentation/HyperSpec/Body/f_vals_l.htm "VALUES-LIST FUNCTION"
  [b283]: #x-28JOURNAL-3ASAVE-EXCURSION-20MGL-PAX-3AMACRO-29 "JOURNAL:SAVE-EXCURSION MGL-PAX:MACRO"
  [b292]: #x-28JOURNAL-3AINPUT-STREAMLET-P-20FUNCTION-29 "JOURNAL:INPUT-STREAMLET-P FUNCTION"
  [b2ff]: #x-28JOURNAL-3ASYNC-JOURNAL-20FUNCTION-29 "JOURNAL:SYNC-JOURNAL FUNCTION"
  [b354]: #x-28JOURNAL-3A-40WORKING-WITH-UNREADABLE-VALUES-20MGL-PAX-3ASECTION-29 "Working with unreadable values"
  [b668]: #x-28JOURNAL-3AIN-MEMORY-JOURNAL-20CLASS-29 "JOURNAL:IN-MEMORY-JOURNAL CLASS"
  [b764]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "WRITE FUNCTION"
  [b792]: #x-28JOURNAL-3A-40IN-MEMORY-JOURNALS-20MGL-PAX-3ASECTION-29 "In-memory journals"
  [b7d2]: #x-28JOURNAL-3A-40COMPARING-JOURNALS-20MGL-PAX-3ASECTION-29 "Comparing journals"
  [ba32]: #x-28JOURNAL-3A-40JOURNAL-UTILITIES-20MGL-PAX-3ASECTION-29 "Utilities"
  [bacd]: #x-28JOURNAL-3AIN-MEMORY-BUNDLE-20CLASS-29 "JOURNAL:IN-MEMORY-BUNDLE CLASS"
  [bb08]: #x-28JOURNAL-3A-40JOURNAL-ERROR-HANDLING-20MGL-PAX-3ASECTION-29 "Error handling"
  [bb62]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_fro.htm "READ-FROM-STRING FUNCTION"
  [bbef]: #x-28JOURNAL-3AREPLAY-OUTCOME-MISMATCH-20CONDITION-29 "JOURNAL:REPLAY-OUTCOME-MISMATCH CONDITION"
  [bece]: #x-28JOURNAL-3A-40LOGGING-WITH-LEAVES-20MGL-PAX-3ASECTION-29 "Logging with `LEAF-EVENT`s"
  [bfc5]: #x-28JOURNAL-3A-40OPENING-AND-CLOSING-20MGL-PAX-3ASECTION-29 "Opening and closing"
  [c015]: #x-28JOURNAL-3A-40DATA-EVENT-20MGL-PAX-3AGLOSSARY-TERM-29 "JOURNAL:@DATA-EVENT MGL-PAX:GLOSSARY-TERM"
  [c04d]: #x-28JOURNAL-3AEVENT-EXIT-20FUNCTION-29 "JOURNAL:EVENT-EXIT FUNCTION"
  [c05a]: #x-28JOURNAL-3A-40FILE-BUNDLES-20MGL-PAX-3ASECTION-29 "File bundles"
  [c290]: #x-28JOURNAL-3AEVENT-OUTCOME-20FUNCTION-29 "JOURNAL:EVENT-OUTCOME FUNCTION"
  [c2b8]: #x-28JOURNAL-3AREPLAYED-20MGL-PAX-3AMACRO-29 "JOURNAL:REPLAYED MGL-PAX:MACRO"
  [c438]: #x-28JOURNAL-3ADELETE-FILE-BUNDLE-20FUNCTION-29 "JOURNAL:DELETE-FILE-BUNDLE FUNCTION"
  [c488]: http://www.lispworks.com/documentation/HyperSpec/Body/m_w_std_.htm "WITH-STANDARD-IO-SYNTAX MGL-PAX:MACRO"
  [c8d1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_signal.htm "SIGNAL FUNCTION"
  [c9d6]: #x-28JOURNAL-3ASYNC-STREAMLET-20GENERIC-FUNCTION-29 "JOURNAL:SYNC-STREAMLET GENERIC-FUNCTION"
  [cbf2]: http://www.lispworks.com/documentation/HyperSpec/Body/t_stream.htm "STREAM TYPE"
  [ceb9]: http://www.lispworks.com/documentation/HyperSpec/Body/t_fixnum.htm "FIXNUM TYPE"
  [cf68]: http://www.lispworks.com/documentation/HyperSpec/Body/m_assert.htm "ASSERT MGL-PAX:MACRO"
  [d2c1]: #x-28JOURNAL-3A-40UNEXPECTED-OUTCOME-20MGL-PAX-3AGLOSSARY-TERM-29 "JOURNAL:@UNEXPECTED-OUTCOME MGL-PAX:GLOSSARY-TERM"
  [d6af]: #x-28JOURNAL-3AMAKE-FILE-BUNDLE-20FUNCTION-29 "JOURNAL:MAKE-FILE-BUNDLE FUNCTION"
  [d700]: #x-28JOURNAL-3A-40JOURNALED-FOR-REPLAY-20MGL-PAX-3ASECTION-29 "Journaled for replay"
  [d97d]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_lin.htm "READ-LINE FUNCTION"
  [d9b6]: #x-28JOURNAL-3ABUNDLE-20CLASS-29 "JOURNAL:BUNDLE CLASS"
  [dc76]: http://www.lispworks.com/documentation/HyperSpec/Body/e_cnd.htm "CONDITION CONDITION"
  [e03f]: #x-28JOURNAL-3A-40TRACING-20MGL-PAX-3ASECTION-29 "Tracing"
  [e33e]: #x-28JOURNAL-3AMAKE-LOG-DECORATOR-20FUNCTION-29 "JOURNAL:MAKE-LOG-DECORATOR FUNCTION"
  [e442]: #x-28JOURNAL-3AREPLAY-INCOMPLETE-20CONDITION-29 "JOURNAL:REPLAY-INCOMPLETE CONDITION"
  [e6b2]: #x-28JOURNAL-3ASTREAMLET-ERROR-20CONDITION-29 "JOURNAL:STREAMLET-ERROR CONDITION"
  [e748]: #x-28JOURNAL-3A-40FILE-JOURNALS-20MGL-PAX-3ASECTION-29 "File journals"
  [e88d]: http://www.lispworks.com/documentation/HyperSpec/Body/f_values.htm "VALUES FUNCTION"
  [e8ed]: #x-28JOURNAL-3ATO-JOURNAL-20GENERIC-FUNCTION-29 "JOURNAL:TO-JOURNAL GENERIC-FUNCTION"
  [e91e]: http://www.lispworks.com/documentation/HyperSpec/Body/e_seriou.htm "SERIOUS-CONDITION CONDITION"
  [e95a]: #x-28JOURNAL-3ACHECKED-20MGL-PAX-3AMACRO-29 "JOURNAL:CHECKED MGL-PAX:MACRO"
  [ec01]: http://www.lispworks.com/documentation/HyperSpec/Body/f_error.htm "ERROR FUNCTION"
  [eddd]: #x-28JOURNAL-3APEEK-REPLAY-EVENT-20FUNCTION-29 "JOURNAL:PEEK-REPLAY-EVENT FUNCTION"
  [f0e7]: #x-28JOURNAL-3AMAKE-FILE-JOURNAL-20FUNCTION-29 "JOURNAL:MAKE-FILE-JOURNAL FUNCTION"
  [f17d]: #x-28JOURNAL-3AVALUES-3C--20FUNCTION-29 "JOURNAL:VALUES<- FUNCTION"
  [f224]: #x-28JOURNAL-3AJOURNAL-DIVERGENT-P-20FUNCTION-29 "JOURNAL:JOURNAL-DIVERGENT-P FUNCTION"
  [f379]: #x-28JOURNAL-3APRINT-EVENTS-20FUNCTION-29 "JOURNAL:PRINT-EVENTS FUNCTION"
  [f37b]: #x-28JOURNAL-3A-40IN-EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29 "In-events"
  [f47b]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRINC FUNCTION"
  [f4d5]: #x-28JOURNAL-3A-40STREAMLETS-REFERENCE-20MGL-PAX-3ASECTION-29 "Streamlets reference"
  [f532]: #x-28JOURNAL-3A-40SYNCHRONIZATION-STRATEGIES-20MGL-PAX-3ASECTION-29 "Synchronization strategies"
  [f846]: #x-28JOURNAL-3A-40JOURNAL-BASICS-20MGL-PAX-3ASECTION-29 "Basics"
  [f932]: #x-28JOURNAL-3ASET-READ-POSITION-20GENERIC-FUNCTION-29 "JOURNAL:SET-READ-POSITION GENERIC-FUNCTION"
  [faf2]: #x-28JOURNAL-3A-40EVENTS-REFERENCE-20MGL-PAX-3ASECTION-29 "Events reference"
  [fbbb]: #x-28JOURNAL-3A-40JOURNALS-REFERENCE-20MGL-PAX-3ASECTION-29 "Journals reference"
  [ff8f]: #x-28JOURNAL-3A-40BUNDLES-REFERENCE-20MGL-PAX-3ASECTION-29 "Bundles reference"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
