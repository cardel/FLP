#lang eopl
#|
Indique la especificación léxica para el siguiente interpretador



- Comentarios, inician con # y terminan en final de linea

- Números enteros decimales (numdec): <digito>(+) | - <digito>(+)

Ejemplo 

15215

-123

2123

- Números binarios enteros positivos (numbin): (0|1)*

1000

01010

1101010

- Números decimales flotantes (numflot)<digito>(+),<digito>(+) | - <digito>(+),<digito>(+)

01111.1100

00011.11

11.0

- Identificadores: <symbol>(+)

a

abb

ccd
<programa> ::= <expresion>

(un-programa)



<expresion> ::= <num-dec>

(dec-exp (datum))

::= <num-bin>

(bin-exp (datum))

::= <num-flot>

(float-exp (datum))

::= "[" <primitiva> <expresion>(*) (,)"]"

(prim-exp (prim lexp))



<primitiva> ::= "+"

<sum-exp>

::= "-"

<rest-exp>

::= "*"

<mult-exp>

::= "/"

<div-exp>
|#

(define lexica
  '(
    (espacio (whitespace) skip)
    (comentario ("#" (arbno (not #\newline))) skip)
    (numentero (digit (arbno digit)) number)
    (numentero ("-" digit (arbno digit)) number)
    (numfloat (digit (arbno digit) "." digit (arbno digit)) number)
    (numfloat ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (numbin ("#b" (or "0" "1") (arbno (or "0" "1"))) number)
    (numbin ("#b" (or "0 ""1") (arbno (or "0" "1")) "."  (or "0" "1") (arbno (or "0" "1"))) number)
    )
  )

(define gramatical
  '(
    (programa (expresion) un-programa)
    (expresion (numentero) dec-exp)
    (expresion (numbin) bin-exp)
    (expresion (numfloat) float-exp)
    (expresion ("[" primitiva (separated-list expresion ",") "]") prim-exp)
    (primitiva ("+") sum-prim)
    (primitiva ("-") rest-prim)
    (primitiva ("*") mul-prim)
    (primitiva ("/") div-prim)))

;;Definir los datatypes, construir los datos
(sllgen:make-define-datatypes lexica gramatical)

;;Construir INTERPRETE :D
(define interpretador
    (sllgen:make-rep-loop
     "-- xXx --"
     (lambda (exp) (evaluar-programa exp))
     (sllgen:make-stream-parser lexica gramatical)
     ))
;;Evaluar programa

;;Ambiente
(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido
   (lid (list-of symbol?))
   (lval (list-of value?))
   (ambiente-viejo ambiente?)))

(define value?
  (lambda (e)
    #T))

(define ambiente-inicial
  (ambiente-extendido '(x y z) '(1 2 3)
                      (ambiente-extendido '(a b c) '(4 5 6) (ambiente-vacio))))

(define aplicar-ambiente
  (lambda (amb var)
    (cases ambiente amb
      (ambiente-vacio () (eopl:error "No se encuentra la variable" var))
      (ambiente-extendido (lid lval amb-ant)
                          (letrec
                              (
                               (buscar-id (lambda (lidd lvall vall)
                                            (cond
                                              [(null? lidd) (aplicar-ambiente amb-ant vall)]
                                              [(eqv? (car lidd) vall) (car lvall)]
                                              [else (buscar-id (cdr lidd) (cdr lvall) vall)])))
                               )
                            (buscar-id lid lval var))))))
                                            
   
;;Evaluar programa
(define evaluar-programa
  (lambda (exp)
    (cases programa exp
      (un-programa (e)
                   (evaluar-expresion e ambiente-inicial)))))


;;Evaluar expresion
(define evaluar-expresion
  (lambda (exp amb)
   (cases expresion exp
    (dec-exp (dato) dato)
    (bin-exp (dato) dato)
    (float-exp (dato) dato)
    (prim-exp (prim lexp) "No implementado"))))
  
(interpretador)
    



    