#lang scheme

(define (subsets s)
	(if (null? s)
		(list null)
		(let ((rest (subsets (cdr s))))
			(append rest 
				(map 
					(lambda (x) (cons (car s) x); (append (list (car s)) x)
				) rest)
			)
		)
	)
)

(append (list 1 2) (list 3)) ; so we have concat already...

(subsets (list 1 2 3))

; got the answer before i understand it...

; first (inner-most) iteration null? give the ()
; second iteration : s is (3) -> car s give the 3 -> x is null -> give the (3)
; third iteration : s is (2 3) -> car s give the 2 -> x map to () and (3) -> give the (2) and (2 3)
; ...

; the construction order is 
; () + 3
; () (3) + 2
; () (3) (2) (2 3) + 1
; ...

; every time "append" expand the rest set
; and lambda part add a head to the rest
; until there is not new head left

; In addition, we fetch a new head element everytime, so cons is enough inside the lambda expression.