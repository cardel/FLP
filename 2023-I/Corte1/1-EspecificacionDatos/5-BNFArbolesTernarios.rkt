#lang eopl
"""
<arbol-t> ::= <numero>
                 ::= <simbolo> <arbol-t> <arbol-t> <arbol-t>


'(sim arb1 arb2 arb3)
sim = car
arb1 = cadr
arb2 = caddr
arb3 = cadddr
"""

(define arbol1
  '(A
    (B 9 8 3)
    (C 4 5 6)
    7
    )
  )

(define suma-hojas
  (lambda (arb)
    (cond
      [(number? arb) arb]
      [else
       (+
        (suma-hojas (cadr arb))
        (suma-hojas (caddr arb))
        (suma-hojas (cadddr arb))
        )
       ]
      )
    )
  )

(display (suma-hojas arbol1))


(define mapeo-extremo
  (lambda (arb f)
    (cond
      [(number? arb) (f arb)]
      [else
       (list
        (car arb)
        (mapeo-extremo (cadr arb) f)
        (mapeo-extremo (caddr arb) f)
        (mapeo-extremo (cadddr arb) f)

        )
       ]
      )
    )
  )

(newline)
(display (mapeo-extremo arbol1 (lambda (n)
                                 (if (even? n)  (expt n 2) (expt n 3)
                                 ))))


(define recorrido-inorden
  (lambda (arb1)
    (cond
      [(number? arb1) (list arb1)]
      [else
       (append
        (recorrido-inorden (cadr arb1))
        (recorrido-inorden (caddr arb1))
        (recorrido-inorden (cadddr arb1))
        )
       ]
      )
    )

  )
(newline)
(display (recorrido-inorden arbol1))

(define ordenar
  (lambda (l)
    (cond
      [(null? l) '()]
      [else
       (ordenar-aux (car l) (ordenar (cdr l)))
       ]
    )
  )
  )

(define ordenar-aux
  (lambda (n l)
    (cond
      [(null? l) (list n)]
      [(> (car l) n) (cons n l)]
      [else
       (cons
        (car l)
        (ordenar-aux n (cdr l))
        )
       ]
      )
    )
  )

(newline)
(display (ordenar (recorrido-inorden arbol1)))