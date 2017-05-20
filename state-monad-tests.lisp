(in-package :monads-tests)


; state functions (returns a function that takes state and returns value and state)
; alist stores state

(defun fetch-val (key)
  (lambda (s) (list (cdr (assoc key s :test 'equal)) s)))

(defun set-val (key val)
  (lambda (s)
    (let* (
           (old-val (assoc key s :test 'equal))
           (new-s   (acons key val s)))
      (list old-val new-s))))

(defun state-naive ()
  (let* (
         (init-state            '((:a . 1) (:b . 2)))
         (from-state-and-val    (apply (fetch-val :b) (list init-state)))
         (from-val              (first from-state-and-val))
         (from-state            (second from-state-and-val))
         (old-state-and-to-val  (apply (set-val :a from-val) (list init-state)))
         (old-to-val            (first old-state-and-to-val))
         (final-state           (second old-state-and-to-val)))
    final-state))

(define-test state-naive-test
  (assert-equal '((:A . 2) (:A . 1) (:B . 2))
    (state-naive)))

;;;;;;; progression 1
(defun state-fun-expr()
  (
  (lambda (init-state)
    ( (lambda (from-state-and-val)
        (let* (
               (from-val (first from-state-and-val))
               (from-state (second from-state-and-val)))
          ( (lambda (old-state-and-to-val)
              (let* (
                     (old-to-val (first old-state-and-to-val))
                     (final-state (second old-state-and-to-val)))
                 final-state) )
              (apply (set-val :a from-val) (list init-state)))))
        (apply (fetch-val :b) (list init-state)) )) '((:a . 1) (:b . 2))))

(define-test state-fun-expr-test
  (assert-equal '((:A . 2) (:A . 1) (:B . 2))
    (state-fun-expr)))

;;;;;;; end of progression 1



;;;;;;; progression 2
;;;

(defun m-state-bind-prog2 (v f)
  (apply f (list v)))

(defun state-bind-expr-prog2()
  (m-state-bind-prog2 '((:a . 1) (:b . 2))         (lambda (init-state) 
  (m-state-bind-prog2 (apply (fetch-val :b)        (list init-state))   (lambda (from-state-and-val) (let* ( (from-val (first from-state-and-val)) (from-state (second from-state-and-val))) 
  (m-state-bind-prog2 (apply (set-val :a from-val) (list init-state))   (lambda (old-state-and-to-val) (let* ( (old-to-val (first old-state-and-to-val)) (final-state (second old-state-and-to-val)))
                final-state          )))))))))

(define-test state-bind-expr-test-prog2
  (assert-equal '((:A . 2) (:A . 1) (:B . 2))
    (state-bind-expr-prog2)))

;;;;;;; end of progression 2

;;;;;;; progression 3
(defun m-state-bind-prog3 (v f)
  (apply f (list v)))

(defun state-bind-expr-prog3()
  ((lambda (init-state) 
    (m-state-bind-prog3 (apply (fetch-val :b)        (list init-state)) (lambda (from-state-and-val) (let* ( (from-val (first from-state-and-val)) (from-state (second from-state-and-val))) 
    (m-state-bind-prog3 (apply (set-val :a from-val) (list init-state)) (lambda (old-state-and-to-val) (let* ( (old-to-val (first old-state-and-to-val)) (final-state (second old-state-and-to-val))) final-state          ))))))) '((:a . 1) (:b . 2))   ))



(define-test state-bind-expr-test-prog3
  (assert-equal '((:A . 2) (:A . 1) (:B . 2))
    (state-bind-expr-prog3)))
;;;;;;; end of progression 3

;;;;;;; progression 4
(defun m-state-bind-prog4 (v f)
  (apply f (list v)))


(defun state-bind-expr-prog4()
  ((lambda (init-state) 
    (apply (lambda (st) (m-state-bind-prog4 (apply (fetch-val :b)        (list init-state)) (lambda (from-state-and-val) (let* ( (from-val (first from-state-and-val)) (from-state (second from-state-and-val))) 
    (apply (lambda (state) (m-state-bind-prog4 (apply (set-val :a from-val) (list state)) (lambda (old-state-and-to-val) (let* ( (old-to-val (first old-state-and-to-val)) (final-state (second old-state-and-to-val))) final-state          )))) (list st)))))) (list init-state))) '((:a . 1) (:b . 2))   ))

(define-test state-bind-expr-test-prog4
  (assert-equal '((:A . 2) (:A . 1) (:B . 2))
    (state-bind-expr-prog4)))

;;;;;;; end of progression 4

;;;;;;; progression 5

(defun xm-state-bind (v f)
  (lambda (s)
    (let* (
           (result (apply v (list s)))
           (new-st (second result))
           (val    (first result)))
      (apply (apply f (list val)) (list new-st)))))


(defun xm-state-return (v)
  (lambda (s) (list v s)))

(defun xstate-bind-expr()
  (apply 
    (lambda (st1)
      (apply (xm-state-bind (fetch-val :b) (lambda (from-val) (lambda (st2) (apply (xm-state-bind (set-val :a from-val) (lambda (old-to-val) (lambda (st3) st3) )) (list st2)))) ) (list st1)))
    (list '((:A . 1) (:B . 2)) ) ) )

(define-test xstate-bind-expr-test
  (assert-equal '((:A . 2) (:A . 1) (:B . 2))
    (xstate-bind-expr)))
;;;;;;; end of progression 5



(defun copy-val (fv tv)
  (do-monad monads:state-m
    [ from-val   (fetch-val fv)
      old-to-val (set-val tv from-val) ]
    old-to-val))

(defun state-monad-example ()
  (let* (
         (initial-state  '((:a . 1) (:b . 2)))
         (computation    (copy-val :b :a))
         (r              (apply computation (list initial-state)))
         (result         (first r))
         (final-state    (second r)))
    final-state))

;(format t "~A~%" (cl:macroexpand-1 `(do-monad state-m [ from-val   (fetch-val fv) old-to-val (set-val tv from-val) ] old-to-val)))

;(format t "~A~%" (copy-val :b :a))

;(apply (copy-val :b :a) (list '((:a . 1) (:b . 2))))

(define-test state-monad-test
  (assert-equal '((:A . 2) (:A . 1) (:B . 2))
    (state-monad-example)))
