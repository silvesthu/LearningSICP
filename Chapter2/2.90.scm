#lang scheme

(include "./algebra.scm")

; ---- TEST BEGIN -----

;(define A (make-complex-from-real-imag 1 2))
;(define B (make-complex-from-mag-ang 1 2))

;(add A B)

; ----  TEST END  -----

; convert dense into sparse internal then do calcucation...

; -------------- http://cparrish.sewanee.edu/cs376/sicp/polynomial-package.ss

(define (install-polynomial-sparse-package)
  
  ;;; internal procedures
  
  ;; representation of poly
  
  (define (make-poly variable term-list)
    (cons variable term-list))
  
  (define (variable p) (car p))
  
  (define (term-list p) (cdr p))
  
  ;;[procedures same-variable? and variable? from section 2.3.2]
  
  (define (variable? x) (symbol? x))
  
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  
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
  
  ;; add-poly
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (sub-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- SUB-POLY"
             (list p1 p2))))
  
  ;; procedures used by add-poly

  (define (on-terms L1 L2 op opterm)
    ;(display "on-terms ") (display L1) (newline)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                      t1 (on-terms  (rest-terms L1) L2 op opterm)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                      (opterm t2) (on-terms  L1 (rest-terms L2) op opterm)))
                   (else
                    (adjoin-term
                      (make-term (order t1)
                                 (op (coeff t1) (coeff t2)))
                      (on-terms  (rest-terms L1)
                                 (rest-terms L2) op opterm))))))))
  
  (define (add-terms L1 L2)
    (on-terms L1 L2 add (lambda (t) t))
  )

  (define (sub-terms L1 L2)
    (on-terms L1 L2 sub (lambda (t) (make-term (order t) (- (coeff t)))))
  )
  
  ;; mul-poly
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))
  
  ;; procedures used by mul-poly
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))
  
  ;; interface to rest of the system
  
  (define (tag p) (attach-tag 'polynomial-sparse p))
  
  (put 'add '(polynomial-sparse polynomial-sparse) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))

  (put 'sub '(polynomial-sparse polynomial-sparse) 
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  
  (put 'mul '(polynomial-sparse polynomial-sparse) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  
  (put 'make 'polynomial-sparse
       (lambda (var terms) (tag (make-poly var terms))))
  'done)


;; Constructor 
;; (which must be visible to the outside world)

(define (make-polynomial-sparse var terms)
  ((get 'make 'polynomial-sparse) var terms))

; ------------ http://cparrish.sewanee.edu/cs376/sicp/polynomial-package.ss

(install-polynomial-sparse-package)

; ------------------------------------- polynomial-sparse ---------------------------------------------


; -------------- http://cparrish.sewanee.edu/cs376/sicp/polynomial-package.ss

(define (install-polynomial-dense-package)
  
  ;;; internal procedures
  
  ;; representation of poly
  
  (define (make-poly variable term-list)
    (cons variable term-list))
  
  (define (variable p) (car p))
  
  (define (term-list p) (cdr p))
  
  ;;[procedures same-variable? and variable? from section 2.3.2]
  
  (define (variable? x) (symbol? x))
  
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  
  ;; representation of terms and term lists
    
  (define (the-empty-termlist) '())
  
  (define (first-term term-list) (car term-list))
  
  (define (rest-terms term-list) (cdr term-list))
  
  (define (empty-termlist? term-list) (null? term-list))
  
  ;; add-poly
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (sub-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- SUB-POLY"
             (list p1 p2))))
  
  ;; procedures used by add-poly

  (define (repeat-0 count)
    (if (<= count 0)
      '()
      (cons 0 (repeat-0 (- count 1)))
    )
  )

  (define (length-terms L)
    (if (pair? L)
      (length L)
      0
    )
  )

  (define (expand-order-0 diff-order L)
    (if (and (= diff-order 0) (= (length-terms L) 0))
      (append (repeat-0 1) L)
      (append (repeat-0 diff-order) L)
    )    
  )

  (define (on-terms L1 L2 op)
    (let 
      (
        (l1 (length-terms L1))
        (l2 (length-terms L2))
      )
      ;(print "add ") (print (expand-order-0 (- l2 l1) L1)) (print " and ") (print (expand-order-0 (- l1 l2) L2))
      (map op (expand-order-0 (- l2 l1) L1) (expand-order-0 (- l1 l2) L2))
    )    
  )
  
  (define (add-terms L1 L2)
    (on-terms L1 L2 +)
  )

  (define (sub-terms L1 L2)
    (on-terms L1 L2 -)
  )
  
  ;; mul-poly
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))
  
  ;; procedures used by mul-poly
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms 
        (mul-term-by-all-terms (first-term L1) (- (length-terms L1) 1) L2)
        (mul-terms (rest-terms L1) L2)
      )
    ) 
  )

  ; raise order then multiple one by one
  (define (mul-term-by-all-terms coeff order L)
    ;(display "order = ") (display order) (display " ")
    ;(display (map (lambda (x) (* x coeff))(append L (repeat-0 order)))) (newline)
    (map (lambda (x) (* x coeff))(append L (repeat-0 order)))
  )
  
  ;; interface to rest of the system
  
  (define (tag p) (attach-tag 'polynomial-dense p))
  
  (put 'add '(polynomial-dense polynomial-dense) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))

  (put 'sub '(polynomial-dense polynomial-dense) 
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  
  (put 'mul '(polynomial-dense polynomial-dense) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  
  (put 'make 'polynomial-dense
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(install-polynomial-dense-package)

;; Constructor 
;; (which must be visible to the outside world)

(define (make-polynomial-dense var terms)
  ((get 'make 'polynomial-dense) var terms))

; ------------------------------------- polynomial-dense ---------------------------------------------

(define (install-polynomial-generic-package)

  (define (dense-to-sparse L)
  	; not very effective though
  	; better to determine by how much storage has to be used for both
  	; and recheck in the end of calculation (mul 0 inside may give sparse polynomial)
  	;(print L)(newline)
  	(if (null? L) 
  		'()
  		(append (list (list (- (length L) 1) (car L))) (dense-to-sparse (cdr L)))
  	)  	
  )

  (define (tag-sparse p) (attach-tag 'polynomial-sparse p))
  (define (tag-dense p) (attach-tag 'polynomial-dense p))

  (put 'add '(polynomial-sparse polynomial-dense)
       (lambda (p1 p2) 
       (add (tag-sparse p1) (tag-sparse (cons (car p2) (dense-to-sparse (cdr p2)))))))

  (put 'add '(polynomial-dense polynomial-sparse) 
       (lambda (p1 p2) 
       (add (tag-sparse (cons (car p1) (dense-to-sparse (cdr p1)))) (tag-sparse p2))))

  (put 'sub '(polynomial-sparse polynomial-dense) 
  		(lambda (p1 p2) 
       (sub (tag-sparse p1) (tag-sparse (cons (car p2) (dense-to-sparse (cdr p2)))))))

  (put 'sub '(polynomial-dense polynomial-sparse) 
       (lambda (p1 p2) 
       (sub (tag-sparse (cons (car p1) (dense-to-sparse (cdr p1)))) (tag-sparse p2))))
  
  (put 'mul '(polynomial-sparse polynomial-dense) 
  		(lambda (p1 p2)
       (mul (tag-sparse p1) (tag-sparse (cons (car p2) (dense-to-sparse (cdr p2)))))))

  (put 'mul '(polynomial-dense polynomial-sparse) 
  		(lambda (p1 p2) 
       (mul (tag-sparse (cons (car p1) (dense-to-sparse (cdr p1)))) (tag-sparse p2))))
)

(install-polynomial-generic-package)

; sparse-sparse
(define s3002 (make-polynomial-sparse 'x '((3 3) (0 2))))
(define s12525 (make-polynomial-sparse 'x '((4 1) (3 2) (2 5) (1 2) (0 5))))
(add s3002 s3002)
(add s12525 s3002)

; dense-dense
(define d3002 (make-polynomial-dense 'x '(3 0 0 2)))
(define d12525 (make-polynomial-dense 'x '(1 2 5 2 5)))
(add d3002 d3002)
(add d12525 d3002)

(add s12525 d3002)
(mul d12525 s3002)