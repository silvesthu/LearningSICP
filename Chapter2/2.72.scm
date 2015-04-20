#lang scheme

; for case as in 2.71

; average O(n^2) <- freq * step for all leaf
; most-freq : the last one will cost O(n) for each search-loop and O(n) for the worst-case => O(n^2)
; least-freq : O(1) <- always at the second layer 

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (encode-symbol symbol tree)
	(print "la") (newline)
	(cond 
		((null? tree) (error "null tree"))
		((leaf? tree) '())
		(else
			(cond 
				((not (not (member symbol (symbols (left-branch tree))))) 
					(append '(0) (encode-symbol symbol (left-branch tree)))
				)
				((not (not (member symbol (symbols (right-branch tree))))) 
					(append '(1) (encode-symbol symbol (right-branch tree)))
				)
				(else (error "symbol not in tree: " symbol))
			)
		)
	)
)

(require "2.69.scm")

(define tree (generate-huffman-tree (list 
'(A 		1) 
'(B 		2) 
'(C 		4) 
'(D			8)
'(E 		16) 
'(F 		32)
'(G 		64)
'(H 		128)
)))

(newline)
(newline)

(encode-symbol 'H tree)