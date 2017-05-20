#!/usr/local/bin/sbcl --script
(load "~/quicklisp/setup.lisp")
(ql:quickload "monads")
(in-package :monads-tests)
(setq *print-failures* t)
(print-errors (lisp-unit:run-tests))
