#lang scheme

(define (make-rat n d)
	(let ((g (gcd n d)))
		(cond 
			((= d 0) (error "div 0"))
			((negative? d) (cons (/ (- n) g) (/ (- d) g)))
			((positive? d) (cons (/ n g) (/ d g)))
		)
	)
)


(make-rat 1 2)
(make-rat -1 -2)
(make-rat 1 -2)
(make-rat -1 2)

(make-rat 1 0)