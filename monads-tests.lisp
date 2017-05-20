(in-package :monads-tests)

(defun inc (x) (+ 1 x))

(defun let-expr ()
  (let* (
         (a 1)
         (b (inc a)))
    (* a b)))

(define-test let-expr-test (assert-equal 2 (let-expr)))

(defun let-fn-expr ()
  ( (lambda (a) ( (lambda (b) (* a b)) (inc a)) ) 1 ))

(define-test let-fn-test (assert-equal 2 (let-fn-expr)))

(defun bind-expr ()
  (monads:m-identity-bind   1     (lambda (a)
  (monads:m-identity-bind (inc a) (lambda (b)
          (* a b))))))
(define-test bind-expr-test (assert-equal 2 (bind-expr)))

;(cl:macroexpand-1 `(do-monad monads:identity-m [ a 1 b (monads-tests:inc a) ] (* a b) ))

(define-test do-monad-expr-test
  (assert-equal 2
    (do-monad monads:identity-m [ a 1 b (monads-tests:inc a) ]
      (* a b))))

(defun nullifier (x) nil)

(define-test do-monad-maybe-non-null-test
  (assert-equal 2
    (do-monad monads:maybe-m [ a 1 b (monads-tests:inc a) ]
      (* a b))))

(define-test do-monad-maybe-null-test
  (assert-equalp nil
    (do-monad monads:maybe-m [ a 1 b (monads-tests:nullifier a) ]
      (* a b))))

