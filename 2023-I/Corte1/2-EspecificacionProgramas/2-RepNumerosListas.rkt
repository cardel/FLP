#lang eopl

(define zero '())

(define zero?
  (lambda (n)
    (equal? n zero)))

(define pred
  (lambda (n)
    (if
     (equal? n zero)
     (eopl:error " No se puede tener el predecesor de cero")
     (cdr n))))

(define succ
  (lambda (n)
    (cons #t n)))

;;Area del programador


(define suma
  (lambda (a b)
    (if
     (zero? b)
     a
     (succ (suma a (pred b))))))

(suma (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t)
      (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t
            #t #t #t #t #t #t #t #t #t #t #t #t #t #t
            #t #t #t #t #t #t #t #t #t #t #t #t #t #t))