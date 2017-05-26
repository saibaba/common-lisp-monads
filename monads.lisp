;;;; monads.lisp

(in-package #:monads)

;;; "monads" goes here. Hacks and glory await!
;;;


(defun id (x) x)

(defun m-identity-bind(v f)
  (apply f (list v)))


(defun prepare (expr bindings bfn)
  (let* (
         (body (first bindings))
         (var (second bindings))
         (r (rest (rest bindings))))
    (if (> (length bindings) 0)
      (let (
            (ne `(apply ',bfn (list ,body (lambda (,var ) ,expr)))))
        (prepare ne r bfn))
      expr)))

(defmacro do-monad (m &rest items)
  (let* (
         (mr (apply m ()))
         (ret (first mr))
         (bind (car (last mr)))
         (body (car (last items)))
         (bindings (reverse (butlast (butlast (rest items)))))
         (expr `(apply ',ret (list ,body))))
    (progn
      ;(format t "~A~%" (prepare expr bindings bind))
      (prepare expr bindings bind))))

; some monads
(defun identity-m ()
  (list 'id 'm-identity-bind))

; maybe
(defun m-maybe-bind (v f)
  (if v
    (apply f (list v))
    nil))

(defun m-maybe-return (x)
  (id x))

(defun maybe-m ()
  (list 'm-maybe-return 'm-maybe-bind))

; sequence
(defun really-flatten (li)
  (cond ((null li) nil)
    ((atom li) `(,li) )
    (t (mapcan #'flatten li))))

(defun flatten (tree)
  (cond ((null tree) ())
    ((consp (car tree)) (concatenate 'list (car tree) 
                          (flatten (cdr tree))))
    (t (cons (car tree) (flatten (cdr tree))))))

; more than anything, the most important education is when
; figuring out this ... for example instead of (concatenate 'list ...
; why (apply 'concatenate 'list ..) ?
; because we have a list of lists and want to remove one level of 
; nesting. (NIL (0) (0 2) ... ) to ( 0  0 2 ..)
; instead of doing some kind of flatting magic, 
; just concatenate multiple lists NIL, (0), (0 2) ... into
; one using concatenate, but we have one nested list not multiple 
; lists (NIL, (0), (0 2)...). But, remember apply takes a list of arguments? 
; So use (apply 'concatenate)!
(defun m-sequence-bind (l f)
  (apply 'concatenate 'list (map 'list (lambda (x) (apply f (list x))) l)))

(defun id-s (x) (list x))

(defun sequence-m ()
  (list 'id-s 'm-sequence-bind))

;;; TODO :when guard, m-lift, m-zero, m-plus


; state monad
(defun m-state-bind (v f)
  (lambda (s)
    (let* (
           (result (apply v (list s)))   ; i should use gensym to avoid serious bugs!
           (new-st (second result))
           (val    (first result)))
      (apply (apply f (list val)) (list new-st)))))

(defun m-state-return (v)
  (lambda (s) (list v s)))

(defun state-m()
  (list 'm-state-return 'm-state-bind))

; m-lift
(defun repeat (f y)
  (if (zerop y)
    nil
    (cons (apply f (list)) (repeat f (1- y)))))

(defun gensyms (c)
  (repeat 'gensym c))

(defun odd-items (l)
  (loop :for c :in l :by #'cddr :collect c))

(defun even-items (l)
  (loop :for c :in (cdr l) :by #'cddr :collect c))

(defmacro do-lift (m f nargs)
  (let* (
         (mr (apply m ()))
         (ret (first mr))
         (bind (car (last mr)))
         (items (gensyms (* 2 nargs)))
         (d1 (format t "2* variables: ~A~%" items))
         (nl (even-items items))
         (body `(,f ,@nl ))
         (d (format t "body: ~A~%" body))
         (bindings (reverse items))
         (expr `(apply ',ret (list ,body))))
    (progn
      `(lambda ,nl ,(prepare expr bindings bind) ) )))
