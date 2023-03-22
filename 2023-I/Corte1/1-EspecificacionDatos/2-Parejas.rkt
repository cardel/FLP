#lang eopl
(define in-S?
  (lambda (p)
    (cond
      [(and
        (= (car p) 0)
        (= (cadr p) 0)
        ) #T]
      [(or
        (< (car p) 0)
        (< (cadr p) 0)
        )
       #F]
      [else
       (in-S?
        (list
         (- (car p) 1)
         (- (cadr p) 2)
        ))
       ]
     )
    )

)

(display (in-S? '(2000 4000)))
(display "\n")
(display (in-S? '(30000 40000)))