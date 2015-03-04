#lang scheme

(define (element-of-set? x set) 
	(cond ((null? set) false) 
		((equal? x (car set)) true) 
		(else (element-of-set? x (cdr set))))) 
 
(define (adjoin-set x set)
	(cons x set))

(define (union-set a b)
	(if (pair? b)
		(union-set (adjoin-set (car b) a) (cdr b))
		(if (null? b)
			a
			(adjoin-set b a)
		)
	)
)

(define (intersection-set set1 set2)
	(cond ((or (null? set1) (null? set2)) '())
		((and (element-of-set? (car set1) set2) (not (element-of-set? (car set1) (cdr set1)))) ; modified
			(cons (car set1)
				(intersection-set (cdr set1) set2)))
		(else (intersection-set (cdr set1) set2))))

'(1 2 3)
(adjoin-set 0 '(1 2 3))
(union-set '(1 2 3) '(1 2 3))
(element-of-set? 3 (union-set '(1 2 3) '(1 2 3)))
(intersection-set '(1 2 3 4) '(1 2 3))
(intersection-set '(1 1 1) '(1 1)) ; 

; preference

; many adjoin or union, few intersection


