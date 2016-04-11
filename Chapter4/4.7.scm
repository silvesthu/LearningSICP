#lang scheme

(define (make-let lets body)
  (list 'let lets body)
)

(define (let*->let-clause lets body)
    (let ((first (car lets))
          (rest (cdr lets)))
          (if (null? rest)
            (make-let (cons first '())
              body))
            (make-let (cons first '())
              (let*->let-clause rest body))
          ))
          

(define (let*->let exp)
  (let*->let-clause (cadr exp) (caddr exp))
)

; no need to be non-derived expression