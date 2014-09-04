; no lang declare needed in header

(define (make-point x y)
	(cons x y)
)

(define (x-point point)
	(car point)
)

(define (y-point point)
	(cdr point)
)

(define (add-point pointA pointB)
	(make-point
		(+ (x-point pointA) (x-point pointB))
		(+ (y-point pointA) (y-point pointB))
	)
)

(define (mul-point point s)
	(make-point
		(* (x-point point) s)
		(* (y-point point) s)
	)
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

(define (length-segment segment)
	(let 
		((start (start-segment segment))
		(end (end-segment segment)))
		(let 
			((diff-x (- (x-point start) (x-point end)))
			(diff-y (- (y-point start) (y-point end))))
			(sqrt (+ (* diff-x diff-x) (* diff-y diff-y)))
		)
	)
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