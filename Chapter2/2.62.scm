#lang scheme

; ordered implementation of set

(define (element-of-set? x set)
	(cond 
		((null? set) false)
		((= x (car set)) true)
		((< x (car set)) false)
		(else (element-of-set? x (cdr set)))
	)
)

(element-of-set? 1 '(1 2 3 4))

(define (intersection-set set1 set2)
	(if (or (null? set1) (null? set2))
		'()
		(let ((x1 (car set1)) (x2 (car set2)))
			(cond ((= x1 x2)
				(cons x1
					(intersection-set (cdr set1) (cdr set2))))
			((< x1 x2)
				(intersection-set (cdr set1) set2))
			((< x2 x1)
				(intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
	(define (adjoin-set-inner x left right)
		(cond 
			((null? right) (list x))
			((= x (car right)) right)
			((< x (car right)) (append left (list x) right))
			(else (adjoin-set-inner x (append left (list (car right))) (cdr right)))
		)
	)
	(adjoin-set-inner x '() set)
)

(include "../common/profile.scm")

(define! (union-set set1 set2)
	(cond 
		((and (null? set1) (null? set2)) '())
		((and (null? set1) (not (null? set2))) (cons (car set2) (union-set set1 (cdr set2))))
		((and (not (null? set1)) (null? set2)) (cons (car set1) (union-set (cdr set1) set2)))
		((= (car set1) (car set2)) (cons (car set2) (union-set (cdr set1) (cdr set2))))
		((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
		(else (cons (car set2) (union-set set1 (cdr set2))))
	)
)

(union-set '(1 3 7 10 11 20 55) '(2 4 6 20 33 55))
(profile 'allres)