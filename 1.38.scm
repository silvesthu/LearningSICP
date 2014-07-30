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
		(display term-i)(display " ")(display iNext)(newline)
		(if (< iNext 0) ; be ware of boundary condition
			term-i
			(cont-frac-iterative n d (/ (n iNext) (+ (d iNext) term-i)) (- iNext 1))
		)
	)
	(cont-frac-iterative n d (/ (n k) (d k)) (- k 1))
)

(define (DFC-d i)
			     	(cond ((= i 0) 1)
		     		  ((= i 1) 2)
		     		  ((= (remainder (- i 2) 3) 0) 1)
		     		  ((= (remainder (- i 2) 3) 1) 1)
		     		  (else (+ (* (- i 1) (/ 2 3)) 2))
		     		)
)

(define e-2 
	(cont-frac-i (lambda (i) 1.0)
		     (lambda (i) 
		     	(cond ((= i 0) 1.0)
		     		  ((= i 1) 2.0)
		     		  ((= (remainder (- i 2) 3) 0) 1)
		     		  ((= (remainder (- i 2) 3) 1) 1)
		     		  (else (+ (* (- i 1) (/ 2 3)) 2))
		     	)
		     )
	10)
) ; 

(+ e-2 2)

; should be 2.718281828

; confirmed on http://web2.0calc.com/
; 1/(1/(1/(1/(1/(1/(1/(1/(1/(1/(1/(8)+1)+1)+6)+1)+1)+4)+1)+1)+2)+1)
