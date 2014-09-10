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