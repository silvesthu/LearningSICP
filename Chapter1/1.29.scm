#lang scheme

(define (even? n)
	(= (remainder n 2) 0))

(define (sum term a next b)
	(if (> a b)
			0
			(+	(term a)
				 	(sum term (next a) next b))))

(define (yk! f a k h)
	(f (+ a (* k h))))

(define (h! b a n)
	(/ (- b a) n))

(define (meta-simpson-term f a h)
	(lambda (i) 
		(if (even? i) 
			(* 2 (yk! f a i h))
		  (* 4 (yk! f a i h))
		)
	)
)

(define (cube x) (* x x x))

(define (simpson f a b n)
	(let (
				(h (h! b a n))
			 )
		(* 
			h
			(+
				(yk! f a 0 h); first term
				(sum (meta-simpson-term f a h) 1 (lambda (x) (+ x 1)) n); sum
				(yk! f a n h); last term
			)
			(/ 1 3)
		)
	)
)

(simpson cube 0.0 1 100)
(simpson cube 0.0 1 1000)
(simpson cube 0.0 1 10000)