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

(define (make-triplet-sum triplet)
	(list (car triplet) (cadr triplet) (caddr triplet) (+ (car triplet) (cadr triplet) (caddr triplet)))
)

; 1 <= k < j < i <= n
(define (unique-triplets n)
	(flatmap 
		(lambda (i) 
			(flatmap ; select-many as in linq
				(lambda (j)
					(map
						(lambda (k) (list i j k))
						(enumerate-interval 1 (- j 1))
					)
				)
				(enumerate-interval 1 (- i 1))
			)
		)
		(enumerate-interval 1 n)
	)
)

(define (equal-sum-triplet n s)
	(filter (lambda (triplet) (= (accumulate + 0 triplet) s)) (unique-triplets n))
)

(unique-triplets 5)
(equal-sum-triplet 5 10)