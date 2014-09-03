#lang scheme

(define (make-point x y)
	(cons x y)
)

(define (x-point point)
	(car point)
)

(define (y-point point)
	(cdr point)
)

(define (make-segment start end)
	(cons start end)
)

(define (start-segment segment)
	(car segment)
)

(define (end-segment segment)
	(cdr segment)
)

(define (average x1 x2)
	(/ (+ x1 x2) 2)
)

(define (midpoint-segment segment)
	(let ((start (start-segment segment))
		  (end (end-segment segment)))
		(make-point (average (x-point start) (x-point end)) (average (y-point start) (y-point end)))
	)
)

(define (print-point p)
	(newline)
	(display "(")
	(display (x-point p))
	(display ",")
	(display (y-point p))
	(display ")")
)

(print-point (make-point 1 10))
(print-point (make-point 2 (- 5)))

(print-point (midpoint-segment (make-segment (make-point 1 10) (make-point 2 (- 5)))))