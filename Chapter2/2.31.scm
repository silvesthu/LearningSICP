#lang scheme

(define (tree-map action tree)
	(map ; map on each element -> skip null implicitly
		(lambda (sub-tree)
			(if (pair? sub-tree)
				(square-tree-map sub-tree)
				(action sub-tree)
			)
		)
		tree
	)
)

(define (square x) (* x x))
(define (square-tree tree) (tree-map square tree))

(square-tree
	(list 1
		(list 2 (list 3 4) 5)
		(list 6 7)
	)
)