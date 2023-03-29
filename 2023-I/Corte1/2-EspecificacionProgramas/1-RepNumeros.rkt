#lang eopl
;
; 0 es un naturales y si n es natural, entonces n + 1 es un entero

(define zero 0)

(define zero?
  (lambda (n)
    (= n 0)))

(define pred
  (lambda (n)
    (if
     (zero? n)
     (eopl:error "No se puede sacar el predecesor de cero")
     (- n 1))))

(define succ
  (lambda (n)
    (+ n 1)))

;;Programador es usar los procedimientos de la interfaz

(define suma
  (lambda (a b)
    (if
     (zero? b)
     a
     (succ (suma a (pred b))))))

(suma 100023 2323213)