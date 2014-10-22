#lang scheme

(map (lambda (x) (* x x)) (list 1 2 3 4 5))

(map abs (list 1 2 -3 4 -5))

(define (square-list items)
	(if (null? items)
		items
		(cons (* (car items) (car items)) (square-list (cdr items)))
	)
)

(square-list (list 1 2 3 4 5))

(define (square-list-map items)
	(map (lambda (x) (* x x)) items)
)

(square-list-map (list 1 2 3 4 5))