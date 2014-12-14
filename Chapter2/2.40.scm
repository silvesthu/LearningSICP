#lang scheme

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))
	)
)

(define (enumerate-interval l r)
	(define (enumerate-interval-inner l r result)
		(if (< r l)
			result
			(enumerate-interval-inner l (- r 1) (cons r result))
		)
	)
	(enumerate-interval-inner l r null)
)

;(enumerate-interval 1 7)

(define (flatmap proc seq)
	(accumulate append null (map proc seq))
)

(define (unique-pairs n)
	(flatmap 
		(lambda (i) 
			(map 
				(lambda (j) (list i j))
				(enumerate-interval 1 (- i 1))
			)
		)
		(enumerate-interval 1 n)
	)
)

(display "All pairs = ")
(unique-pairs 5)
(newline)

(require "./../common/prime.scm")

(define (prime-sum? pair)
	(prime? (+ (car pair) (cadr pair)))
)

(define (make-pair-sum pair)
	(list (car pair) (cadr pair) (+ (car pair) (cadr pair)))
)

(define (prime-sum-pairs n)
	(map make-pair-sum
		(filter prime-sum? (unique-pairs n))
	)
)

(display "Sum-prime pairs = ")
(prime-sum-pairs 5)
(newline)