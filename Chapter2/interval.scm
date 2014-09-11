(define (make-interval a b) (cons a b))

(define (lower-bound n) (car n))
(define (upper-bound n) (cdr n))

(define (add-interval x y)
	(make-interval 	
			(+ (lower-bound x) (lower-bound y))
			(+ (upper-bound x) (upper-bound y))
	)
)

(define (sub-interval x y)
	(make-interval 	
			(- (lower-bound x) (upper-bound y))
			(- (upper-bound x) (lower-bound y))
	)
)

(define (mul-interval x y)
	(let 	((p1 (* (lower-bound x) (lower-bound y)))
			 (p2 (* (lower-bound x) (upper-bound y)))
			 (p3 (* (upper-bound x) (lower-bound y)))
			 (p4 (* (upper-bound x) (upper-bound y))))
	(make-interval 	(min p1 p2 p3 p4)
					(max p1 p2 p3 p4)))
)

(define (div-interval x y)
	(let ((uy (upper-bound y))
		  (ly (lower-bound y)))
		(if (or (= uy 0) (= ly 0))
			(error "error div-zero")
			(mul-interval x (make-interval 
				(/ 1.0 uy)
				(/ 1.0 ly)))
		)
	)
)