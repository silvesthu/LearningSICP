#lang scheme

(define (product term a next b)
	(if (> a b)
			1
			(*	(term a)
				 	(product term (next a) next b))))

(define (product-iter term a next b)
	(define (iter a result)
		(if (> a b)
				result
				(iter (next a) (* result (term a)))
		)
	)
	(iter a 1)
)

(define (calc4pi product count)
	(define (term index)
		(cond ((= index 0) (/ 2 3))
					((= index count) 1.0)
					((even? index) (/ (+ 2 index) (+ (+ 2 index) 1)))
					(else 
						(let ((x (+ 2 (+ index 1))))
							(/ x (- x 1))
						)
					)
		)
	)
	(product
		term
		0
		(lambda (x) (+ x 1))
		count
	)
)

(time (calc4pi product 10000))
(time (calc4pi product-iter 10000))

; why iterative version is slower ?