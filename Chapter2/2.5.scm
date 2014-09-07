#lang scheme

(define (2a3b a b)
	(* (expt 2 a) (expt 3 b))
)

(2a3b 1 2)
(2a3b 2 5)

(define (cons a b)
	(2a3b a b)
)

(remainder 11 2)

(define (car z)
	(if (= (remainder z 2) 0)
		(+ (car (/ z 2)) 1)
		0
	)
)

(define (cdr z)
	(if (= (remainder z 3) 0)
		(+ (cdr (/ z 3)) 1)
		0
	)
)

(car (2a3b 2 5))
(cdr (2a3b 2 5))
(car (2a3b 23 56))
(cdr (2a3b 23 56))

; [TODO] better idea
; try 2, 4, 8, ... may be one of optimizations for big number