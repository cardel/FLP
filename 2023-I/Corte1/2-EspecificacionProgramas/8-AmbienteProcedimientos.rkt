#lang eopl
;;Constructores

(define empty-env
  (lambda ()
    (lambda (s)
      (cond
        [(= s 0) 'empty-env]
        [else (eopl:error "Señal invalida")])
      )))

(define extend-env
  (lambda (ld lv old-env)
    (lambda (s)
      (cond
        [(= s 0) 'extend-env]
        [(= s 1) ld]
        [(= s 2) lv]
        [(= s 3) old-env]))))

;;Observadores

;;Predicados
(define empty-env?
  (lambda (n)
    (eqv? 'empty-env (n 0))))

(define extend-env?
  (lambda (n)
    (eqv? 'extend-env (n 0))))

;;Extractores

(define extend-env->ld
  (lambda (n)
    (n 1)))

(define extend-env->lv
  (lambda (n)
    (n 2)))

(define extend-env->env-old
  (lambda (n)
    (n 3)))

;;;Area del programador :D

(define apply-env
  (lambda (env val)
    (cond
      [(empty-env? env) (eopl:error "No encuentro esa variable")]
      [(extend-env? env)
       (letrec
           (
            (lid (extend-env->ld env))
            (lval (extend-env->lv env))
            (buscar-lista
             (lambda (ld lv)
               (cond
                 [(null? ld) #F]
                 [(eqv? (car ld) val) (car lv)]
                 [else (buscar-lista (cdr ld) (cdr lv))])))
            )
         (let
             (
              (encontrado (buscar-lista lid lval))
              )
           (if (eqv? encontrado #F)
               (apply-env (extend-env->env-old env) val)
               encontrado)))]
      [else (eopl:error "Ambiente no valido")]
      )))

(define ambiente1
  (extend-env '(x y z) '(1 2 3)
              (extend-env '(x a b c) '(10 4 5 6)
                          (empty-env))))