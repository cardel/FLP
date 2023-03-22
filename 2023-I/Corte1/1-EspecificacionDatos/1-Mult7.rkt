#lang eopl
(define in-S?
  (lambda (n)
    (cond
     [(= n 7) #T]
     [(< n 7) #F]
     [else
      (in-S? (- n 7))
      ]
     )
    )
)

(display (in-S? 9))
(display "\n")
(display (in-S? 7000))