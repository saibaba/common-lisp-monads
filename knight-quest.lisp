(in-package :monads-tests)

(defun possible-knight-position-p (pos)
  (let* (
         (x (first pos))
         (y (second pos)))
    (and (> x 0) (> y 0) (< x 9) (< y 9))))

(defun move-knight (pos)
  (let* (
         (x (first pos))
         (y (second pos)))
    (remove-if-not #'possible-knight-position-p
      (list (list (+ x 2) (- y 1))
          (list (+ x 2) (+ y 1))
          (list (- x 2) (- y 1))
          (list (- x 2) (+ y 1))
          (list (+ x 1) (- y 2))
          (list (+ x 1) (+ y 2))
          (list (- x 1) (- y 2))
          (list (- x 1) (+ y 2))))))

(defun move-knight-3-times (pos)
  (do-monad monads:sequence-m
    [ a (move-knight pos)
      b (move-knight a)
      c (move-knight b) ]
      c ))

; todo :when guard for possible-knigt-position-p

(defun same-position (p1 p2)
  (let* (
         (x1 (first p1))
         (y1 (second p1))
         (x2 (first p2))
         (y2 (second p2)))
    (and (equal x1 x2) (equal y1 y2))))

(defun can-reach-in-3-moves (from-pos to-pos)
  (let* (
         (possibles (move-knight-3-times from-pos)))
    (not (null (member to-pos possibles :test 'same-position )))))

(define-test can-reach-in-3-moves-positive-test
  (assert-true (can-reach-in-3-moves (list 6 2) (list 6 1))))

(define-test can-reach-in-3-moves-negative-test
  (assert-false (can-reach-in-3-moves (list 6 2) (list 7 3))))
