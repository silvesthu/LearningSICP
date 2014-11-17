#lang scheme

(define (concat a b)
	(if (pair? a)
		(cons (car a) (concat (cdr a) b))
		(if (null? a)
			b
			(concat (list a) b)
		)		
	)
)

(define (fringe root)
	(if (pair? root)
		(concat (fringe (car root)) (fringe (cdr root)))
		root
	)
)

(define x (list (list 1 2) (list 3 4)))

(concat (list 1 2) (list 3 4))
(fringe x)
(fringe (list (list 1 (list 3 4 (list 5 6) 7) (list 8 9))))

