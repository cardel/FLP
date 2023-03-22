#lang eopl

(let
    (
     (a (let ((a 3) (b 4)) (+ a b)))
     (b 3)
     (c (lambda (c) (+ c 3)))
     )
  (let*
      (
       (a b)
       (b b)
       (c (c a))
       )
    (+ a b c))) 

;20  19