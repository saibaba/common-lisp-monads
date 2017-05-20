;;;; monads.asd

(asdf:defsystem #:monads
  :description "Describe monads here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:lisp-unit)
  :components ((:file "package")
               (:file "monads")
               (:file "monads-tests")
               (:file "sequence-monad-tests")
               (:file "state-monad-tests")
               (:file "knight-quest")))

