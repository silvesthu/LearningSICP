#lang scheme

(define (sum term a next b)
	(define (iter a result)
		(if (> a b)
				result
				(iter (next a) (+ result (term a)))
		)
	)
	(iter a 0)
)

(sum 
	(lambda (x) x)
	1
	(lambda (x) (+ x 1))
	100
)

(sum 
	(lambda (x) (* x x))
	1
	(lambda (x) (+ x 1))
	100
)

; verified on wolframe alpha with keyword "sum square 1 to 100" only
; a real handy tool !