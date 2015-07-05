#lang scheme

(include "./algebra.scm")


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

;; Constructor 
;; (which must be visible to the outside world)

(define (make-polynomial-dense var terms)
  ((get 'make 'polynomial-dense) var terms))

; ------------ http://cparrish.sewanee.edu/cs376/sicp/polynomial-package.ss

(install-polynomial-dense-package)

; say, higher order comes first as in the book

(define p3002 (make-polynomial-dense 'x '(3 0 0 2)))
(add p3002 p3002)
(define p12525 (make-polynomial-dense 'x '(1 2 5 2 5)))
(add p12525 p3002)
(sub p12525 p3002)
(mul p12525 p3002) ; http://www.wolframalpha.com/input/?i=%28x%5E4%2B2*x%5E3%2B5*x%5E2%2B2*x%2B5%29%283*x%5E3%2B2%29