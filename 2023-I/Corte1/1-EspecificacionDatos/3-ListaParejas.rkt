#lang eopl
;;;Multiplos de 5
(define in-A?
  (lambda (n)
    (cond
      [(= n 5) #T]
      [(< n 5) #F]
      [else
       (in-A? (- n 5))
       ]
      )
    )
  )



;;;Multiplos de 7
(define in-B?
  (lambda (n)
    (cond
      [(= n 7) #T]
      [(< n 7) #F]
      [else
       (in-B? (- n 7))
       ]
      )
    )
  )

;;Lista de parejas

(define in-S?
  (lambda (lp)
    (cond
      [(null? lp) #T]
      [(and
        (in-A? (caar lp))
        (in-B? (cadar lp))
        )
       (in-S? (cdr lp))
       ]
      [else #F]
      )
    
    )
)

(display (in-S? '((25 77) (500000 7000000) (5 7))))
(newline)
(display (in-S? '((25 77) (500000 7000006) (5 7))))