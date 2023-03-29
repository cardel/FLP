#lang eopl

(define-datatype enviroment enviroment?
  (empty-env)
  (extend-env (ld (list-of symbol?))
              (lv (list-of value?))
              (old-env enviroment?)))

(define value?
  (lambda (n)
    #T))

;;Area del programador

(define ambiente1
  (extend-env '(x y z) '(1 2 3)
              (extend-env '(x a b c) '(10 4 5 6)
                          (empty-env))))


(define apply-env
  (lambda (env val)
    (cases enviroment env
      (empty-env () (eopl:error "No encuentro la variable"))
      (extend-env (ld lv old-env)
                  (letrec
                      (
                       (buscar-lista
                        (lambda (ld lv)
                          (cond
                            [(null? ld) #F]
                            [(eqv? (car ld) val) (car lv)]
                            [else (buscar-lista (cdr ld) (cdr lv))])))
                       )
                    (let
                        (
                         (encontrado (buscar-lista ld lv))
                         )
                      (if (eqv? encontrado #F)
                          (apply-env old-env val)
                          encontrado)
                      )
                    )
                  )
      )
    )
  )