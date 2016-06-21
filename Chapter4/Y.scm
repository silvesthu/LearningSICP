#lang scheme

; read these articles
; (Chinese) http://yaodanzhang.com/blog/2015/06/11/functional-programming-fundamental-y-combinator/
; (Chinese) http://liujiacai.net/blog/2014/10/12/lambda-calculus-introduction/

; named

(define (factorial n)
	(if (eq? n 1)
		1
		(* n (factorial (- n 1)))
	)
)
(factorial 5)

; try to remove name

#|
(lambda (n)
	(if (eq? n 1)
		1
		(* n (??? (- n 1)))
	)
)
|#

(define factorial-two-param
(lambda (f n)
	(if (eq? n 1)
		1
		(* n (f (- n 1)))
	)
)
)

; currying
(define factorial-curring
(lambda (f)
	(lambda (n) 
		(if (eq? n 1)
			1
			(* n ((f f) (- n 1)))
		)
	)
)
)
((factorial-curring factorial-curring) 5)

; inline self call
(define factorial-inline
	(
		(lambda (f) (f f))
		(lambda (f)
			(lambda (n) 
				(if (eq? n 1)
					1
					(* n ((f f) (- n 1)))
				)
			)
		)
	)
)
(factorial-inline 5) 

; extract f-wrapper
(define factorial-inline-2
	(
		(lambda (f) (f f))
		(lambda (f)
			; (define f-wrapper (f f))  ; this is no good cuz a direct call to f only gives infinity loop
			(define (f-wrapper x) ((f f) x))
			(lambda (n) 
				(if (eq? n 1)
					1
					(* n (f-wrapper (- n 1)))
				)
			)
		)
	)
)
(factorial-inline-2 5) 

; make f-wrapper a lambda -> now f-wrapper can be extract as a recursive core
(define factorial-inline-3
	(
		(lambda (f) (f f))
		(lambda (f)
			(
				(lambda (f-wrapper)
					(lambda (n) 
						(if (eq? n 1)
							1
							(* n (f-wrapper (- n 1)))
						)
					)
				)
				(lambda (x) ((f f) x))
			)
		)
	)
)
(factorial-inline-3 5) 

; now make f-wrapper a parameter of outer function
(define (factorial-inline-4 f-wrapper)
	(
		(lambda (f) (f f))
		(lambda (f)
			(
				f-wrapper
				(lambda (x) ((f f) x))
			)
		)
	)
)
((factorial-inline-4 	
	(lambda (f)
		(lambda (n)
			(if (eq? n 1)
				1
				(* n (f (- n 1)))
			)
		)
	)) 5)

; rewrite as Y. of course Y itself can be a lambda
(define (Y self)
	(
		(lambda (f) (f f))
		(lambda (f)
			(self (lambda (x) ((f f) x)))
		)
	)
)
((Y ; a factorial function take itself as parameter
	(lambda (f)
		(lambda (n)
			(if (eq? n 1)
				1
				(* n (f (- n 1)))
			)
		)
	)
) 5)















; Reference from original article

; (define (Y fake-rec)
; 	(
; 		(lambda (f) (f f))
; 		(lambda (f) (fake-rec (lambda (x) ((f f) x))))
; 	)
; )
; ((Y (lambda (f) (lambda (n)
; 	(if (eq? n 1)
; 		1
; 		(* n (f (- n 1)))
; 	)
; ))) 5)



