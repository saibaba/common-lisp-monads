;;;; package.lisp

(defpackage #:monads
  (:export #:do-monad
           #:id
           #:m-identity-bind
           #:identity-m
           #:maybe-m
           #:m-maybe-return
           #:m-maybe-bind
           #:flatten
           #:id-s
           #:sequence-m
           #:state-m
           #:prepare
           #:m-sequence-bind
           #:even-items
           #:odd-items
           #:repeat
           #:gensyms
           #:do-lift
           )
  (:use #:cl))

(defpackage :monads-tests
  (:export #:inc
           #:nullifier
           #:test-state-m
           #:state-return-2
           #:state-bind-2)
  (:use :common-lisp :lisp-unit :monads))

