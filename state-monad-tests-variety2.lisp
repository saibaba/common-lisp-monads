(in-package :monads-tests)


; state functions (returns a function that takes state and returns list (value state))
; alist stores state

(defun fetch-val (key)
  (lambda (s) (list (cdr (assoc key s :test 'equal)) s)))

(defun set-val (key val)
  (lambda (s)
    (let* (
           (old-val (assoc key s :test 'equal))
           (new-s   (acons key val s)))
      (list old-val new-s))))

(defun state-naive (init-state)
  (let* (
         (from-state-and-val    (apply (fetch-val :b) (list init-state)))

         (from-val              (first from-state-and-val))
         (from-state            (second from-state-and-val))

         (old-state-and-to-val  (apply (set-val :a from-val) (list from-state)))

         (old-to-val            (first old-state-and-to-val))
         (final-state           (second old-state-and-to-val)))
    final-state))

;(defun state-desired (init-state)
;  (apply do-monad( [
;                     (from-val (fetch-val :b))
;                     (to-val   (set-val :a from-val))
;                   ]
;                   to-val)
;    (list init-state)))
;
;Returned is (list to-val <latest state>)

(defun variety2-state-nested-let-expr(initial-state)
  (let* (
         (v-and-s-1 (apply (fetch-val :b) (list initial-state)))
         (from-val  (first v-and-s-1))
         (from-state  (second v-and-s-1)))
    (let* (
           (v-and-s-2 (apply (set-val :a from-val) (list from-state)))
           (old-to-val (first v-and-s-2))
           (final-state (second v-and-s-2)))
      final-state)))

(define-test variety2-state-nested-let-expr-test
  (assert-equal '((:A . 2) (:A . 1) (:B . 2))
    (variety2-state-nested-let-expr '((:a . 1) (:b . 2)))))

(defun variety2-state-fun-replaces-let-expr(start-state)
  (
   (lambda (initial-state)
     (
      (lambda (v-and-s-1)
        (
         (lambda (from-val)
           (
            (lambda (from-state)
              (
               (lambda(v-and-s-2)
                 (
                  (lambda (old-to-val)
                    (
                     (lambda(final-state)
                       (list old-to-val final-state))
                     (second v-and-s-2) )
                    ) (first v-and-s-2))
               ) (apply (set-val :a from-val) (list from-state)))
              ) (second v-and-s-1))
           ) (first v-and-s-1))
        ) (apply (fetch-val :b) (list initial-state)))) start-state))

(define-test variety2-state-fun-replaces-let-expr-test
  (assert-equal '((:A . 2) (:A . 1) (:B . 2))
    (second (variety2-state-fun-replaces-let-expr '((:a . 1) (:b . 2))))))


;;;; progression 1
(defun state-bind-1 (v f)
  (lambda (s)
    (apply f (list (apply v (list s))))))

(defun variety2-state-with-bind-1(start-state)
  ((lambda (initial-state)
     (apply (state-bind-1 (fetch-val :b)
                 (lambda (v-and-s-1)
                   ((lambda (from-val)
                      ((lambda (from-state)
                         (apply (state-bind-1 (set-val :a from-val)
                                            (lambda(v-and-s-2)
                                              ((lambda (old-to-val)
                                                 ((lambda(final-state)
                                                    (list old-to-val final-state)) (second v-and-s-2)))
                                               (first v-and-s-2))))
                                (list from-state)))
                       (second v-and-s-1)))
                    (first v-and-s-1))))
            (list initial-state)))
   start-state))

(define-test variety2-state-with-bind-1-test
  (assert-equal '((:A . 2) (:A . 1) (:B . 2))
    (second (variety2-state-with-bind-1 '((:a . 1) (:b . 2))))))

;;;; progression 2 (sc = state computation)
(defun state-return (v)
  (lambda (s) (list v s)))

(defun state-bind-2 (sc f)
  (lambda (s)
    (let* (
           (v-and-s (apply sc (list s)))
           (v (first v-and-s))
           (ns (second v-and-s)))
      (apply (apply f (list v)) (list ns)))))

(defun variety2-state-with-bind-2(start-state)
  (apply (state-bind-2 (fetch-val :b)
      (lambda (from-val)
        (state-bind-2 (set-val :a from-val)
          (lambda (old-to-val) (state-return old-to-val))))) (list start-state)))

; now just use do-monad on above return/bind-2
(define-test variety2-state-with-bind-2-test
  (assert-equal '((:A . 2) (:A . 1) (:B . 2))
    (second (variety2-state-with-bind-2 '((:a . 1) (:b . 2))))))
