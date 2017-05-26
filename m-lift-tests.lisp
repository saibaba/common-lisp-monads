(in-package :monads-tests)

(defun nil-respecting-addition-1(m n)
  (if (or (null m) (null n))
    nil
    (+ m n)))

(define-test nil-respecting-addition-1-non-nulls-test
  (assert-equal 3
    (nil-respecting-addition-1 1 2)))

(define-test nil-respecting-addition-1-nulls-test
  (assert-nil
    (nil-respecting-addition-1 1 nil)))

(defun nil-respecting-addition-2(m n)
  (m-maybe-bind m (lambda (mv) (m-maybe-bind n (lambda(nv) (m-maybe-return (+ mv nv)))))))

(define-test nil-respecting-addition-2-non-nulls-test
  (assert-equal 3
    (nil-respecting-addition-2 1 2)))

(define-test nil-respecting-addition-2-nulls-test
  (assert-nil
    (nil-respecting-addition-2 1 nil)))

(defun m-lift-1 (f)
  (lambda (m n) (m-maybe-bind m (lambda (mv) (m-maybe-bind n (lambda(nv) (m-maybe-return (+ mv nv))))))))

(defun nil-respecting-addition-3(m n)
  (apply (m-lift-1 :+) (list m n)))

(define-test nil-respecting-addition-3-non-nulls-test
  (assert-equal 3
    (nil-respecting-addition-3 1 2)))

(define-test nil-respecting-addition-3-nulls-test
  (assert-nil
    (nil-respecting-addition-3 1 nil)))

; (format t "~A~%" (cl:macroexpand-1 `(do-lift monads:maybe-m + 2)))

(defun nil-respecting-addition-4 ()
  (do-lift maybe-m + 2))

(define-test nil-respecting-addition-4-non-nulls-test
  (assert-equal 3
    (apply (nil-respecting-addition-4) (list 1 2 ))))

(define-test nil-respecting-addition-4-nulls-test
  (assert-nil
    (apply (nil-respecting-addition-4) (list 1 nil ))))
