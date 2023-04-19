#lang eopl

#|
UV-FLP-LENGPRO
Comentarios: "//" (final de linea)
Numeros enteros decimales (positivos, negativos)
Numeros enteros octales oc <numero>
Cadenas de texto " "

Valores expresados: numero + texto + booleanos
Valores denotados: numero + texto + booleanos

<programa> ::= <expresion>

<expresion> ::= <identificador>
             var-exp(id)
            ::= <numero>
             lit-exp(datum)
            ::= " <identificador> "
             cad-exp(datum)
            ::= "verdadero"
            verdad-exp()
            ::= "falso"
            falso-exp()
            ::= "opera" <primitiva> "[" <expresiones>* "]"
             operar-exp(prim lexp)
            ::= "si" <expresion> "entonces" <expresion> "sino" <expresion>
            condicional-exp(cond-exp verd-exp falso-exp)
            ::= "local" (<identificador> "=" <expresion>)* "en" <expresion>
            local-exp (lid lexp)
            ::= "proc" "(" (<identificador> ",")* ")" <expresion>
            proc-exp(lid exp)
            ::=  "("<expresion> (<expresion>)* ")"
            invocar-exp(rator, rands)

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
    (expresion ("si" expresion "entonces" expresion "sino" expresion) condicional-exp)
    (expresion ("local" (arbno identificador "=" expresion) "en" expresion) local-exp)
    (expresion ("proc" "(" (separated-list identificador ",") ")" expresion) proc-exp)
    (expresion ("localrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)
                           "en" expresion) localrec-exp)
    (expresion ( "(" expresion (arbno expresion) ")") invocar-exp) 
    (primitiva ("+") sum-prim)
    (primitiva ("-") rest-prim)
    (primitiva ("&") concat-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("&&") y-prim)
    (primitiva (">") mayor-prim)
    (primitiva ("<") menor-prim)
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
   (ambiente-viejo ambiente?))
  (ambiente-extendido-recursivo
   (procnames (list-of symbol?))
   (lids (list-of (list-of symbol?)))
   (lexp (list-of expresion?))
   (ambiente-viejo ambiente?))
)

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
                            (buscar-id lid lval var)))
      (ambiente-extendido-recursivo (proc-names llids lcuerpos amb-viejo)
                                    (letrec
                                        (
                                         (buscar-var
                                          (lambda (lproc lids lcuerpos)
                                            (cond
                                              [(null? lproc) (aplicar-ambiente amb-viejo var)]
                                              [(eqv? (car lproc) var)
                                               (clausura
                                                (car lids)
                                                (car lcuerpos)
                                                amb)]
                                              [else (buscar-var (cdr lproc)
                                                                (cdr lids)
                                                                (cdr lcuerpos))])))
                                         )
                                      (buscar-var proc-names llids lcuerpos)
                                      )
                                    )
      
      )))
                                            
   
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
      (condicional-exp (pregunta exp-verdad exp-falso)
                       (if
                        (evaluar-expresion pregunta amb)
                        (evaluar-expresion exp-verdad amb)
                        (evaluar-expresion exp-falso amb)))
      (local-exp (lid lexp exp)
                 (let
                     (
                      (lexpeval (map (lambda (x) (evaluar-expresion x amb)) lexp))
                      )
                 (evaluar-expresion
                  exp
                  (ambiente-extendido lid  lexpeval amb))))
      (proc-exp (lid exp)
                (clausura lid exp amb)
                )
      (invocar-exp (exp lexps)
                   (let
                       (
                        (proc (evaluar-expresion exp amb))
                        (lrands (map (lambda (x) (evaluar-expresion x amb)) lexps))
                        )
                     (if
                      (proc-val? proc)
                      (cases proc-val proc
                        (clausura (lid exp amb-viejo)
                                  (if (=
                                       (length lid)
                                       (length lrands))
                                      (evaluar-expresion
                                       exp
                                       (ambiente-extendido lid lrands amb-viejo)
                                       )
                                      (eopl:error "El procedimiento esperaba " (length lid)
                                                  " enviaste " (length lrands))
                                      )))
                      (eopl:error "Debe enviar un procedimiento para evaluar, enviaste" exp)
                      )
                     ))
      (localrec-exp
       (procnames llid lcuerpos exp)
       (evaluar-expresion
        exp
        (ambiente-extendido-recursivo procnames llid lcuerpos amb)))
      )
    
    
    )
  )

(define evaluar-primitiva
  (lambda (prim lval)
    (cases primitiva prim
      (sum-prim () (operar lval 0 +))
      (rest-prim () (operar lval 0 -))
      (mult-prim () (operar lval 1 *))
      (concat-prim () (operar lval "" string-append))
      (y-prim () (operar lval #t (lambda (x y) (and x y))))
      (mayor-prim () (> (car lval) (cadr lval)))
      (menor-prim () (< (car lval) (cadr lval)))
      )))



(define operar
  (lambda (lst acc op)
    (cond
      [(null? lst) acc]
      [else (op
             (car lst)
             (operar (cdr lst) acc op))])))


(define-datatype proc-val proc-val?
  (clausura
   (lid (list-of symbol?))
   (exp expresion?)
   (amb ambiente?)))

(interpretador)
