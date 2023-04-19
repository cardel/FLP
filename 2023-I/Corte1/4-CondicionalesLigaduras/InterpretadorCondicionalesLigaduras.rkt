#lang eopl

#|
UV-FLP-LENGPRO
Comentarios: "//" (final de linea)
Numeros enteros decimales (positivos, negativos)
Numeros enteros octales oc <numero>
Cadenas de texto " "

Valores expresados: numero + texto
Valores denotados: numero + texto

<programa> ::= <expresion>

<expresion> ::= <identificador>
             var-exp(id)
            ::= <numero>
             lit-exp(datum)
            ::= " <identificador> "
             cad-exp(datum)
            ::= "opera" <primitiva> "[" <expresiones>* "]"
             operar-exp(prim lexp)

<primitiva> ::= "+"
                sum-exp()
            ::= "-"
                menor-exp()
            ::= "&"
                concat-exp()
              

|#
;;Especificación lexica: ¿Cuales son las unidades significativas de mi lenguaje?
(define lexica
  '(
    (espacio (whitespace) skip)
    (comentario ("//" (arbno (not #\newline))) skip)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero ("#o" digit (arbno digit)) number)
    (numero ("-" "#o" digit (arbno digit)) number)
    (identificador (letter (arbno (or digit letter))) symbol)))

;;Especificación gramatical: ¿Cómo voy a escribir el código en mi lenguaje?
(define gramatica
  '(
    (programa (expresion) un-programa)
    (expresion (identificador) var-exp)
    (expresion (numero) lit-exp)
    (expresion ("falso") falso-exp)
    (expresion ("verdadero") verdadero-exp)
    (expresion ("\"" identificador "\"") cadena-exp)
    (expresion ("opera" primitiva "[" (separated-list expresion ",") "]") operar-exp)
    (primitiva ("+") sum-prim)
    (primitiva ("-") rest-prim)
    (primitiva ("&") concat-prim)
    (primitiva ("y") y-prim)
    ))

;;Definir los datatypes, construir los datos
(sllgen:make-define-datatypes lexica gramatica)


;;Construir el escaner, que me extrae las unidades significativas
;; Aqui NO se revisa sintaxis, solo se extrae la información
(define escaner
  (lambda (exp)
    ( (sllgen:make-string-scanner lexica gramatica)
      exp)))

;;COnstruir el parser, para transformar el codigo en AST
(define parser
  (lambda (exp)
    ( (sllgen:make-string-parser lexica gramatica)
      exp)))

;;Construir INTERPRETE :D
(define interpretador
    (sllgen:make-rep-loop
     "--xXx lenguaje pro xXx --"
     (lambda (exp) (evaluar-programa exp))
     (sllgen:make-stream-parser lexica gramatica)
     ))

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

(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      (var-exp (id) (aplicar-ambiente amb id))
      (lit-exp (num) num)
      (cadena-exp (txt) (symbol->string txt))
      (falso-exp () #F)
      (verdadero-exp () #T)
      (operar-exp (prim lexp)
                  (let
                      (
                       (lexp-evaluada (map (lambda (x) (evaluar-expresion x amb))
                                             lexp))
                       )
                    (evaluar-primitiva prim lexp-evaluada)
                    )

                  )
      )
    )
  )

(define evaluar-primitiva
  (lambda (prim lval)
    (cases primitiva prim
      (sum-prim () (operar lval 0 +))
      (rest-prim () (operar lval 0 -))
      (concat-prim () (operar lval "" string-append))
      (y-prim () (operar lval #t (lambda (x y) (and x y))))
      )))



(define operar
  (lambda (lst acc op)
    (cond
      [(null? lst) acc]
      [else (op
             (car lst)
             (operar (cdr lst) acc op))])))



(interpretador)
