#lang scheme

(define (let->lambda-parameters lets)
  (map car lets)
)

(define (let->arguments lets)
  (map cdr lets)
)

(define (let->combination exp)
  (let 
    (
      (let-keyword (car exp))
      (lets (cadr exp))
      (body (caddr))
    )
    (apply (make-lambda (let->lambda-parameters lets) body) (sequence->exp (let->arguments lets)))
  )
)

; something like this...