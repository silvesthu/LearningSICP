#lang scheme

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

; above from https://github.com/jaroslawr/sicp/blob/master/2-64-partial-tree.scm

(define (tree->list-1 tree)
	(if (null? tree)
		'()
		(append (tree->list-1 (left-branch tree))
			(cons (entry tree)
				(tree->list-1 (right-branch tree))
			)
		)
	)
)

(define (tree->list-2 tree)
	(define (copy-to-list tree result-list)
		(if (null? tree)
			result-list
			(copy-to-list (left-branch tree)
				(cons (entry tree)
					(copy-to-list (right-branch tree)
						result-list
					)
				)
			)
		)
	)
	(copy-to-list tree '())
)

(define (tree-2.16-1)
	(make-tree 7 (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '())) (make-tree 9 '() (make-tree 11 '() '())))
)
(tree-2.16-1)
(tree->list-1 (tree-2.16-1))
(tree->list-2 (tree-2.16-1))

(define (tree-2.16-2)
	(make-tree 3 (make-tree 1 '() '()) (make-tree 7 (make-tree 5 '() '()) (make-tree 9 '() (make-tree 11 '() '()))))
)
(tree-2.16-2)
(tree->list-1 (tree-2.16-2))
(tree->list-2 (tree-2.16-2))

(define (tree-2.16-3)
	(make-tree 5 (make-tree 3 (make-tree 1 '() '()) '()) (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '())))
)
(tree-2.16-3)
(tree->list-1 (tree-2.16-3))
(tree->list-2 (tree-2.16-3))

(tree->list-1 (make-tree 1 '() '()))
(tree->list-2 (make-tree 1 '() '()))

; a. they produce same result (Pre-order-traversal)
; 
; and number of steps calling the main function are the same. -> O(n)
;
; However, according to http://www.billthelizard.com/2013/03/sicp-263-265-sets-as-binary-trees.html
; append implemented as adding element from first argument to second argument
; whose number of steps depends on the size of the first argument.
; 
; so for each node, those are lying in the left-branch of it are appended multiple time, about log(max-depth - depth)
; then for all nodes, the total number of step -> O(n) * O(log(n)) -> O(nlog(n))