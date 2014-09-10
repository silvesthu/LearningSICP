 #lang scheme
 
; church numerals by Alonzo Church

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
	(lambda (f) (lambda (x) (f ((n f) x)))))

; (add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x))))

;(lambda (f) 
;	(lambda (x) 
;		(f 
;			(
;				(
;					(lambda (f) (lambda (x) x)) 
;					f
;				) 
;			x)
;		)
;	)
;)

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (g) (lambda (f) (lambda (x) (g (f x))))))

; ... higher rank function

(define (sub-1 n) ; naming sub-1 may not technically correct, it makes number larger than 0 finally decrease to zero, and form a cycle on zero
	(lambda (f) (n (lambda (g) g))))

((zero "dummy") "A")

((one (lambda (f) f)) "B")
(((sub-1 one) "dummy") "A")

(((two (lambda (f) f)) (lambda (g) g)) "C")

(define two-1 (two (lambda (f) f)))
((two-1 (lambda (f) f)) "B") ; as one

(define two-1-1 (lambda (t) two-1 (lambda (g) g)))
((two-1-1 "dummy") "A") ; not equal to zero

(define single (lambda (x) x))

; name "lambda (x) x" as single

; then 
;	zero = f(any) = single, zero(single) = single
; 	one = g(f(any)), one(single) = single
;	two = h(g(f(any))), two(single) = one

(one one) ; same as one

(one two) ; same as two
((lambda (f) (lambda (x) (f x))) two)
(lambda (x) (two x))
(lambda (x) ((lambda (g) (lambda (f) (lambda (x1) (g (f x1))))) x))
(lambda (x) (lambda (f) (lambda (x1) (x (f x1)))))
(lambda (g) (lambda (f) (lambda (x) (g (f x)))))

(((((two two) single) single) single) "End") ; same as three

(define (add x y) (lambda (g) (g (x y))))
; (((add one one) single) "EndAdd")
((((add one one) single) single) "EndAdd2")
(((((add one two) single) single) single) "EndAdd3")
((((((add two two) single) single) single) single) "EndAdd4")
(define three (add one two))
(define four (add two two))
(define five (add four one))
((((((five single) single) single) single) single) "EndAdd5")