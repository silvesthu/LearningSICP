#lang scheme

(define (repeated f n)
	(lambda (x)
		(if (= n 1) 
			(f x) ; last one
			((repeated f (- n 1)) (f x))
		)
	)
)

(define (average x y) (/ (+ x y) 2))

(define (average-damp f) 
	(lambda (x) (average x (f x))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess)
		(let ((next (f guess)))
			(if (close-enough? guess next)
				next
				(try next))))
	(try first-guess)
)

; 1. damp * 1 -> x / (y^2) => y -> x / y => single average damping

(define (sqrt x)
	(fixed-point (average-damp (lambda (y) (/ x y)))
				 1.0))

(sqrt 4)

; 2. damp * 2 -> x / (y^3) => y -> x / y^2 => single average damping

(define (cbrt x)
	(fixed-point (average-damp (lambda (y) (/ x (* y y))))
				 1.0))

(cbrt 4)

; 3. damp * 3 -> x / (y^4) => y -> x / y^3 => single average damping not work => twice average damping

(define (qdrt x) ; fourth root
	(fixed-point (average-damp (average-damp (lambda (y) (/ x (* y y y)))))
				 1.0))

(qdrt 4)

; 4... => 5 root with twice damping

(define (fiveroot x) 
	(fixed-point (average-damp (average-damp (lambda (y) (/ x (* y y y y)))))
				 1.0))

(fiveroot 4)

; ?. damp * ? -> x / (y^(n-1))


(define (n-root-2 x n)
	(fixed-point (average-damp (average-damp 
		(lambda (y) 
			(/ x ((repeated (lambda (z) (* z y)) (- n 1)) 1))
		)))
	1.0)
)

(display "twice") (newline)
(n-root-2 4 2)
(n-root-2 4 3)
(n-root-2 4 4)
(n-root-2 4 5)
(n-root-2 4 6)
(n-root-2 4 7)

(display "three times") (newline)

(define (n-root-3 x n)
	(fixed-point (average-damp (average-damp (average-damp
		(lambda (y) 
			(/ x ((repeated (lambda (z) (* z y)) (- n 1)) 1))
		))))
	1.0)
)

(n-root-3 4 8)
(n-root-3 4 9)
(n-root-3 4 15) ; less than 2^3 (3-times damping)

; so n needs floor(log2(n)) times average damping

; from wikipedia 
; http://en.wikibooks.org/wiki/Scheme_Programming/Further_Maths

(define logB 
    (lambda (x B) 
      (/ (log x) (log B))))

(logB 4 2)
(floor (logB 5 2))
(truncate (logB 5 2)) ; truncate - no large than absolute value

(define (n-root x n)
	(fixed-point 
		(
			(repeated 
				average-damp 
				(truncate (logB n 2))
			)
			(lambda (y) 
				(/ x ((repeated (lambda (z) (* z y)) (- n 1)) 1)) ; just a pow.... anyway
			)
		)
	1.0)
)

(display "general") (newline)

(n-root 4 8)
(n-root 4 9)
(n-root 4 15)
(n-root 4 16)
(n-root 4 100)