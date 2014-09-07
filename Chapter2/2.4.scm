#lang scheme

(define (cons x y)
	(lambda (m) (m x y))
)

(define (car z)
	(z (lambda (p q) p))
)

(define (cdr z)
	(z (lambda (p q) q))
)

; substitution !

; store data in lambda + no mutable data

(car (cons 1 2))
(cdr (cons 1 2))

(car (cons x y))
(z (lambda (p q) p))
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (p q) p) x y)
(x)