;;;; package.lisp

(defpackage #:monads
  (:export #:do-monad
           #:id
           #:m-identity-bind
           #:identity-m
           #:maybe-m
           #:m-maybe-bind
           #:flatten
           #:id-s
           #:sequence-m
           #:state-m
           #:m-sequence-bind
           )
  (:use #:cl))

(defpackage :monads-tests
  (:export #:inc #:nullifier)
  (:use :common-lisp :lisp-unit :monads))

