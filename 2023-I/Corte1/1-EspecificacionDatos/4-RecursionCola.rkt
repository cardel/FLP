#lang eopl


(define f-head
  (lambda (n)
    (cond
      [(= n 0) 1]
      [(= n 1) 1]
      [else
       (+
        (f-head (- n 1))
        (f-head (- n 2))
        )
       ]
      )
    )
  )

;;(f-head 6)

(define f-tail
  (lambda (n [a 1] [b 1])
    (cond
      [(= n 0) a]
      [(= n 1) b]
      [else
       (f-tail (- n 1) b (+ a b))
       ]
      )
    )
  )

(f-tail 6)

