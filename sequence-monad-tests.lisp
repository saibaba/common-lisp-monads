(in-package :monads-tests)

(defun for-loop-expr ()
  (apply 'concatenate 'list (loop
    for a from 0 to 4
    collect (apply 'concatenate 'list (loop
      for b from 0 to (- a 1)
      collect (list (* a b)))))))

(define-test for-loop-test (assert-equal (list 0 0 2 0 3 6 0 4 8 12) (for-loop-expr)))

(defun range (m)
  (loop
    for v from 0 to (- m 1)
    collect v))

(defun for-bind-expr (l f)
  (apply 'concatenate 'list (map 'list (lambda (x) (apply f (list x))) l)))

(defun sample-for-bind-expr ()
  (for-bind-expr (range 5) (lambda (a) 
  (for-bind-expr (range a) (lambda (b)
                 (list (* a b)))))))

(define-test for-bind-test (assert-equal (list 0 0 2 0 3 6 0 4 8 12) (sample-for-bind-expr)))

(define-test sequence-monad-test
  (assert-equal (list 0 0 2 0 3 6 0 4 8 12)
                (do-monad monads:sequence-m
                  [ a (range 5) 
                    b (range a) ]
                  (* a b))))

(define-test sequence-monad-test-with-input-seq
  (assert-equal (list '(0 0) '(1 -1) '(2 -2) '(3 -3) '(4 -4))
                (do-monad monads:sequence-m
                  [ a (range 5) ]
                  (list a (* -1 a)))))

