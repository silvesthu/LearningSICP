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

(intersection-set '(1 2 3) '(3 4 5))

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

(adjoin-set 1 '())
(adjoin-set 1 '(1 2 3))
(adjoin-set 1 '(2 3))
(adjoin-set 2 '(1 3))

; same as in element-of-set?, average of steps required will be about n/2