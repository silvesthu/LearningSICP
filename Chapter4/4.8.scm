#lang scheme

(define (let->lambda-parameters lets)
  (map car lets)
)

(define (let->arguments lets)
  (map cdr lets)
)

(define (let->combination exp)
  (if (null? (cadr (cdddr exp)))
    (let 
      (
        (let-keyword (car exp))
        (lets (cadr exp))
        (body (caddr))
      )
      (apply (make-lambda (let->lambda-parameters lets) body) (sequence->exp (let->arguments lets)))
    )
    (let
      (
        (let-keyword (car exp))
        (let-name (cadr exp))
        (lets (caddr exp))
        (body (cadr (cddr exp)))
      )
      (let ((f (make-lambda (let->lambda-parameters lets) body) (sequence->exp (let->arguments lets))))
       (sequence->exp 
        (list 'define let-name f)
        (apply f)
       ) 
      )           
    ) 
  )  
)
