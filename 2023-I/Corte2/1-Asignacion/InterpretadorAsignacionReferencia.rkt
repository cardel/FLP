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
            ::= "begin" (<expresion> ",")+
                "begin"  <expresion> ("," <expresion>)*
            begin-exp(exps exp)
            ::= set <identificador> "=" <expresion>
            set-exp(id exp)

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
    (expresion ("cond" (separated-list "(" expresion "->" expresion ")" ",") "else" expresion) cond-exp)
    (expresion ("begin" expresion (arbno  "," expresion) "end")  begin-exp)
    (expresion ( "(" expresion (arbno expresion) ")") invocar-exp)
    (expresion ("set" identificador "=" expresion) set-exp)
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
   (lval vector?)
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

;;Referencias
(define expval?
  (lambda (dat)
    (or
     (boolean? dat)
     (number? dat)
     (proc-val? dat))))


(define-datatype target target?
  (direct-target (a expval?))
  (indirect-target
   (ref ref-target-direct?)))

(define ambiente-inicial
  (ambiente-extendido '(x y z) (list->vector (list (direct-target 1)
                                                   (direct-target 2)
                                                   (direct-target 3)))
                      (ambiente-extendido '(a b c) (list->vector
                                                    (list
                                                     (direct-target 4)
                                                     (direct-target 5)
                                                     (direct-target 6)
                                                     )) (ambiente-vacio))))


(define aplicar-ambiente
  (lambda (amb var)
    (de-ref
     (aplicar-ambiente-ref amb var)))
)

(define aplicar-ambiente-ref
  (lambda (amb var)
    (cases ambiente amb
      (ambiente-vacio () (eopl:error "No se encuentra la variable" var))
      (ambiente-extendido (lid lval amb-ant)
                          (letrec
                              (
                               (buscar-id (lambda (lidd lvall vall [pos 0])
                                            (cond
                                              [(null? lidd) (aplicar-ambiente-ref amb-ant vall)]
                                              [(eqv? (car lidd) vall)
                                               (a-ref
                                                pos
                                                lvall)
                                               ]
                                              [else (buscar-id (cdr lidd) lvall vall (+ 1 pos))])))
                               )
                            (buscar-id lid lval var)))
      (ambiente-extendido-recursivo (proc-names llids lcuerpos amb-viejo)
                                    (letrec
                                        (
                                         (buscar-var
                                          (lambda (lproc lids lcuerpos)
                                            (cond
                                              [(null? lproc) (aplicar-ambiente-ref amb-viejo var)]
                                              [(eqv? (car lproc) var)
                                               (a-ref
                                                0
                                                (list->vector
                                                 (list (direct-target (clausura
                                                                       (car lids)
                                                                       (car lcuerpos)
                                                                       amb)))))]
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
                      (lexpeval (map (lambda (x) (direct-target (evaluar-expresion x amb))) lexp))
                      )
                 (evaluar-expresion
                  exp
                  (ambiente-extendido lid (list->vector lexpeval) amb))))
      (proc-exp (lid exp)
                (clausura lid exp amb)
                )
      (invocar-exp (exp lexps)
                   (let
                       (
                        (proc (evaluar-expresion exp amb))
                        (lrands (map 
                                 (lambda (x)
                                   (cases expresion x
                                     (var-exp (id)
                                              (indirect-target (aplicar-ambiente-ref amb id))
                                              )
                                     (else (direct-target (evaluar-expresion x amb)))))
                                 lexps))
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
                                       (ambiente-extendido lid (list->vector lrands) amb-viejo)
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

      (cond-exp (q-exps t-exps else-exp)
                (letrec
                    (
                     (evaluar (lambda (q-exps t-exps else-exp)
                                (cond
                                  [(null? q-exps) (evaluar-expresion else-exp amb)]
                                  [(evaluar-expresion (car q-exps) amb)
                                   (evaluar-expresion (car t-exps) amb)]
                                  [else
                                   (evaluar (cdr q-exps)
                                            (cdr t-exps)
                                            else-exp)])))
                     )
                  (evaluar q-exps t-exps else-exp)))
      (begin-exp (exp exps)
                 (if (null? exps)
                     (evaluar-expresion exp amb)
                     (begin
                       (evaluar-expresion exp amb)
                       (letrec
                           (                            
                            (evaluar (lambda (exps)
                                       (cond
                                         [(null? (cdr exps)) (evaluar-expresion (car exps) amb)]
                                         [else
                                          (begin
                                            (evaluar-expresion (car exps) amb)
                                            (evaluar (cdr exps)))])))
                            )
                         (evaluar exps)))
                     ))
      (set-exp (id exp)
               (let
                   (
                    (ref (aplicar-ambiente-ref amb id))
                    (val (evaluar-expresion exp amb))
                    )
                  (begin (set-ref ref val) 1)))
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

;;Referencia: entero (pos), vector

(define-datatype referencia referencia?
  (a-ref
   (pos number?)
   (vec vector?)))

(define de-ref
  (lambda (ref)
    (let
        (
         (target-val (cases referencia ref
                       (a-ref (pos vec)
                              (vector-ref vec pos))))
         )
      (cases target target-val
        (direct-target (v) v)
        (indirect-target (ref)
                         (cases target (cases referencia ref
                                         (a-ref (pos vec)
                                              (vector-ref vec pos)))
                           (direct-target (v) v)
                           (else (eopl:error "El target dentro de la referencia debe ser directo")))
                         )))
      ))

(define set-ref
  (lambda (ref val)
      (let
          (
           (refvalor
            (cases target
              (cases referencia ref
                (a-ref (pos vec)
                (vector-ref vec pos)))
              (direct-target (v) ref)
              (indirect-target (ref1) ref1)))
           )
        (cases referencia refvalor
          (a-ref
           (pos vec)
           (vector-set! vec pos (direct-target val)))))))
        






(define ref-target-direct?
  (lambda (x)
    (and
     (referencia? x)
     (cases referencia x
       (a-ref (pos vec)
              (cases target (vector-ref vec pos)
                (direct-target (v) #T)
                (else #F)))))))

(interpretador)
