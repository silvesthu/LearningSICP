#lang scheme

(define (cont-frac-r n d k)
	(define (cont-frac-recursive n d i)
		(if (= i k)
			(/ (n i) (d i))
			(/ (n i) (+ (d i) (cont-frac-recursive n d (+ i 1))))
		)
	)
	(cont-frac-recursive n d 1)
)

(cont-frac-r (lambda (i) 1.0)
		     (lambda (i) 1.0)
		     11) ; minimun to get 0.6180

(define (cont-frac-i n d k)
	(define (cont-frac-iterative n d term-i iNext)
		(if (= iNext 0)
			term-i
			(cont-frac-iterative n d (/ (n iNext) (+ (d iNext) term-i)) (- iNext 1))
		)
	)
	(cont-frac-iterative n d (/ (n k) (d k)) (- k 1))
)

(cont-frac-i (lambda (i) 1.0)
		     (lambda (i) 1.0)
		     11) ; minimun to get 0.6180