#lang scheme

(define (count-pairs x)
	(if (not (pair? x))
		0
		(+  (count-pairs (car x))
			(count-pairs (cdr x))
			1)))

; three
(count-pairs (cons (cons 1 2) (cons 3 4)))

; four
(define p (cons 1 2))
(count-pairs (cons p (cons 3 p)))

; seven
(define q (cons p p))
(count-pairs (cons q q))

; -> with shared pairs