#lang scheme

(define (cont-frac-r n d k)
	(define (cont-frac-recursive n d i)
		(if (= i k)
			(/ (n i) (d i))
			(/ (n i) (+ (d i) (cont-frac-recursive n d (+ i 1))))
		)
	)
	(cont-frac-recursive n d 0) ; be ware of boundary condition
)
(define (cont-frac-i n d k)
	(define (cont-frac-iterative n d term-i iNext)
		;(display term-i)(display " ")(display iNext)(newline)
		(if (< iNext 0) ; be ware of boundary condition
			term-i
			(cont-frac-iterative n d (/ (n iNext) (+ (d iNext) term-i)) (- iNext 1))
		)
	)
	(cont-frac-iterative n d (/ (n k) (d k)) (- k 1))
)

; make both n and d minus -> same as switch plus/minus in cont-frac
(define (tan-cf x k)
	(cont-frac-i 
		(lambda (i)
			(if (= i 0.0)
				(- x)
				(- (* x x))
			)
		)
     	(lambda (i) 
     		(- (+ (* i 2.0) 1.0))
     	)
	100)
)

(tan-cf 15 10)
(tan 15)