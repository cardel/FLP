#lang eopl

;;COnstructores

;;Datatype record
(define empty-record
  (lambda ()
    (list 'empty-record)))

(define non-empty-record
  (lambda (key elm rec)
    (list 'non-empty-record key elm rec)))

;;Datatype item
(define simple-item
  (lambda (dat)
    (list 'simple-item dat)))

(define list-item
  (lambda (datum lnode)
    (list 'list-item datum lnode)))

;;Datatype simple-item
(define item-num
  (lambda (dat)
    (list 'item-num dat)))

(define item-sym
  (lambda (dat)
    (list 'item-sym dat)))

;;Datatype-list item
(define empty-list-item
  (lambda ()
    (list 'empty-list-item)))

(define non-empty-list-item
  (lambda (elm lst)
    (list 'non-empty-list-item elm lst)))

;;Ejemplo

(define registro1
  (non-empty-record
   'a
   (simple-item (item-num 4))
   (non-empty-record
    'b
    (list-item
     (simple-item (item-num 22))
     (non-empty-list-item
      (simple-item (item-sym 'x))
      (non-empty-list-item
       (simple-item (item-num 4))
       (empty-list-item))))
    (empty-record)
    )))

(display registro1)

;;Observadores

;;predicado

(define item-sym?
  (lambda (reg)
    (eqv? (car reg) 'item-sym)))

(define non-empty-record->elm
  (lambda (reg)
    (caddr reg)))
       