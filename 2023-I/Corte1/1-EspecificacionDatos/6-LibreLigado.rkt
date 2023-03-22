#lang eopl
(define ocurre-libre
  (lambda (exp var)
    (cond
      [(symbol? exp) (eqv? exp var)]
      [(eqv? (car exp) 'lambda)
       (and
        (not (eqv? var (caadr exp)))
        (ocurre-libre (caddr exp) var)
        )
       ]
      [else
       (or
        (ocurre-libre (car exp) var)
        (ocurre-libre (cadr exp) var))

       ]
      )
    )
  )

(display (ocurre-libre 'x 'x)) (newline)
(display (ocurre-libre '(x (lambda (x) (x x))) 'x)) (newline)
(display (ocurre-libre '((lambda (x) (x x)) (lambda (y) (lambda (x) (x y )))) 'x)) (newline)
         