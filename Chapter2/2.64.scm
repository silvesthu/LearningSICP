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

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (list->tree elements)
	(car (partial-tree elements (length elements)))
)

(list->tree '(1 3 5 7 9 11))
; 		5
;     1   9  
;      3 7 11

; a = b * quotient + remainder

; Example: '(1 3 5 7 9 11)

; partial-tree : (list length) -> (left-tree (this-entry elements-left))
; n =  0 -> '()
; n != 0
;	left-size = (6 - 1) / 2 = 2, right-size = (6 - 2 - 1) = 3 ; for the first iteration
;   left-result  <- recursive call to split convert left  half of sub-list to left-tree
;   right-result <- recursive call to split convert right half of sub-ist to left-tree
;   this-entry   <- center element (left one) in sub-list
;	remaining-elts <- elements left in list (those are not converted to tree yet)

; So processing sequence will be a pre-order traversal (construction) which consume the original list form left to right

; ----------------------------

; order of growth in the numer of steps
; every tree-node will be accessed once (creation) and no complex operation during the iteration
; so overall time complexity should be O(n)