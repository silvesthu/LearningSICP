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

(define (make-pair key value)
	(cons key value)
)

(define (key pair) (car pair))
(define (value pair) (cdr pair))

(define (lookup given-key set-of-records)
	(cond ((null? set-of-records) #f)
		((= given-key (key (entry set-of-records))) (value (entry set-of-records)))
		((< given-key (key (entry set-of-records)))
		 (lookup given-key (left-branch set-of-records)))
		((> given-key (key (entry set-of-records)))
		 (lookup given-key (right-branch set-of-records)))))

(define (test) (list->tree (list 
	(make-pair 1 "a")
	(make-pair 5 "e")
	(make-pair 3 "c")
	(make-pair 4 "d")
	(make-pair 2 "b")
)))

(test)
(lookup 3 (test))