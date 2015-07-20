#lang scheme

(require srfi/1)
(include "./algebra.scm")

(define (install-polynomial-multi-package)
  
  ;;; internal procedures
  
  ;; representation of poly
  
  (define (make-poly variables term-list)
    (cons variables term-list))
  
  (define (variables p) (car p))
  
  (define (term-list p) (cdr p))
  
  ;;[procedures same-variable? and variable? from section 2.3.2]
  
  (define (variable? x) 
	(any (lambda (x) (symbol? x)) x))
  
  ;; representation of terms and term lists
  
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
  
  (define (=zero? coeff)
    (equal? coeff 0))
    
  (define (the-empty-termlist) '())
  
  (define (first-term term-list) (car term-list))
  
  (define (rest-terms term-list) (cdr term-list))
  
  (define (empty-termlist? term-list) (null? term-list))
  
  (define (make-term order coeff) (list order coeff))
  
  (define (order term) (car term))
  
  (define (coeff term) (cadr term))

  ;; merge-var
  (define (merge-var v1 v2)
  	(cond 
  		((null? v1) v2)
  		((memq (car v1) v2) (merge-var (cdr v1) v2))
  		(else (merge-var (cdr v1) (cons (car v1) v2)))
  	)
  )

  (define (convert-var v-to p)
  	(let 
  		(
  			(v (variables p))
  			(t (term-list p))
  			(to-count (length v-to))
  			(from-count (length (variables p)))
  		)
  		(display "convert ") (display v) (display " to ") (display v-to) (display " on ") (display t) (newline)
  		(define (swizzle term)
  			(define (swizzle-order v-to-left)
  				;(display v-to-left) (newline)
  				;(display v) (newline)
  				(if (null? v-to-left) 
  					'()
  					(let 
  						(
  							(index (list-index (lambda (x) (eq? x (car v-to-left))) v))
  						)
  						(if index
  							(cons (list-ref (order term) index) (swizzle-order (cdr v-to-left)))
  							(cons 0 (swizzle-order (cdr v-to-left)))
  						)
  					)
  				)  				
  			)
  			(make-term (swizzle-order v-to) (coeff term))
  		)
  		(make-poly v-to (map swizzle t))
  	)
  )
  
  ;; add-poly
  
  (define (add-poly p1 p2)
    (let ((var (merge-var (variables p1) (variables p2))))
      ;(convert-var var p1)
      (make-poly var
                 (add-terms (term-list (convert-var var p1))
                            (term-list (convert-var var p2))))
  )
)
  
  ;; procedures used by add-poly

  (define (order-less o1 o2)
  	(cond 
  		((or (null? o1) (null? o2)) (error "invalid order"))
  		((< (car o1) (car o2)) #t)
  		((> (car o1) (car o2)) #f)
  		(else 
  			(if (or (null? (cdr o1)) (null? (cdr o2)))
  				#f ; equal
  				(order-less (cdr o1) (cdr o2))
  			)
  		)
  	)	
  )

  (define (order-equal o1 o2)
  	(if (or (null? o1) (null? o2)) 
  		(error "invalid order")
  		(equal? o1 o2)
  	)
  )

  (define (on-terms L1 L2 op opterm)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
           	 ;(print t1)(newline)(print t2)(newline)
           	 ;(print (order-less (order t1) (order t2)))(newline)
             (cond ((order-less (order t1) (order t2))
                    (adjoin-term
                      (opterm t2) (on-terms  L1 (rest-terms L2) op opterm)))
             	    
                   ((order-equal (order t1) (order t2))
                    (adjoin-term
                      (make-term (order t1)
                                 (op (coeff t1) (coeff t2)))
                      (on-terms  (rest-terms L1)
                                 (rest-terms L2) op opterm)))
  				   (else
                    (adjoin-term
                      t1 (on-terms  (rest-terms L1) L2 op opterm))))))))
  
  (define (add-terms L1 L2)
    (on-terms L1 L2 add (lambda (t) t))
  )

  
  ;; mul-poly
  
  ;(define (mul-poly p1 p2)
  ;  (if (same-variable? (variables p1) (variables p2))
  ;    (make-poly (variables p1)
  ;               (mul-terms (term-list p1)
  ;                          (term-list p2)))
  ;    (error "Polys not in same var -- MUL-POLY"
  ;           (list p1 p2))))
  
  ;; procedures used by mul-poly
  
  ;(define (mul-terms L1 L2)
  ;  (if (empty-termlist? L1)
  ;    (the-empty-termlist)
  ;    (add-terms (mul-term-by-all-terms (first-term L1) L2)
  ;               (mul-terms (rest-terms L1) L2))))
  
  ;(define (mul-term-by-all-terms t1 L)
  ;  (if (empty-termlist? L)
  ;    (the-empty-termlist)
  ;    (let ((t2 (first-term L)))
  ;      (adjoin-term
  ;        (make-term (+ (order t1) (order t2))
  ;                   (mul (coeff t1) (coeff t2)))
  ;        (mul-term-by-all-terms t1 (rest-terms L))))))

  ;; interface to rest of the system
  
  (define (tag p) (attach-tag 'polynomial-multi p))
  
  (put 'add '(polynomial-multi polynomial-multi) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))

  ;(put 'mul '(polynomial-multi polynomial-multi) 
  ;     (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'make 'polynomial-multi
       (lambda (var terms) (tag (make-poly var terms))))
  'done)


;; Constructor 
;; (which must be visible to the outside world)

(define (make-polynomial-multi var terms)
  ((get 'make 'polynomial-multi) var terms))

; ------------ http://cparrish.sewanee.edu/cs376/sicp/polynomial-package.ss
(install-polynomial-multi-package)

; x -> y -> z : input var must be this order

(define pxy1000 (make-polynomial-multi '(x y) '(((1 0) 2) ((0 0) 1))))
(add pxy1000 pxy1000)
(define pxy1001 (make-polynomial-multi '(x y) '(((1 0) 2) ((0 1) 1))))
(add pxy1000 pxy1001)

(define pyz0100 (make-polynomial-multi '(y z) '(((0 1) 2) ((0 0) 1))))
(add pxy1000 pyz0100)

(define pzyw012100 (make-polynomial-multi '(z y w) '(((0 1 2) 2) ((1 0 0) 1))))
(add pzyw012100 pyz0100)

;(list-index (lambda (x) (= x 0)) '(1 2 3))
; not found -> #f

; skip mul... should be ok after conversion

; how to sort 'x 'y ??
