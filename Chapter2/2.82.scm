#lang scheme

(include "./algebra.scm")

(newline)(newline)

(define coercion-table 0)
(set! coercion-table '())

; with a simple list of list (not a table actually...)

(define (put-coercion from to item) (set! table (append table (list (list from to item)))))
(define (get-coercion from to) 
	(define (get-inner from to t)
		;(display t) (newline)
		(cond 
			((null? t) #f)
			((and (equal? from (caar t)) (equal? to (cadar t))) (caddar t))
			(else (get-inner from to (cdr t)))
		)
	)
	(get-inner from to table)
)

; --------------------------------

; a. 

; apply-generic call it self after conversion
; so a conversion both from and to same type will cause infinity loop

; b. 

; No, nothing has to be added. Function calls with parameters with same type 
; are handled as they installed in the first place.
; Except there is risk that it is possible that a. may occur.

; c.

(define (apply-generic-coericion op . args)
  (let ((type-tags (map type-tag args)))
       (let ((proc (get op (if (and (pair? type-tags) (eq? (cdr type-tags) null)) (car type-tags) type-tags))))
            (if proc
                (apply proc (map contents args))
                (if (= (length args) 2)
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags))
                          (a1 (car args))
                          (a2 (cadr args)))
                         (let ((t1->t2 (get-coercion type1 type2))
                               (t2->t1 (get-coercion type2 type1)))
                              (cond (t1->t2
                                      (apply-generic-coericion op (t1->t2 a1) a2))
                                    (t2->t1
                                      (apply-generic-coericion op a1 (t2->t1 a2)))
                                    (else (error "1. No method for these types" (list op type-tags))))))
                    (error "2. No method for these types" (list op type-tags)))))))

(define (apply-generic-coericion-vararg op . args)
  (let ((type-tags (map type-tag args)))
       (let ((proc (get op (if (and (pair? type-tags) (eq? (cdr type-tags) null)) (car type-tags) type-tags))))
       		(define (get-conversions types target-type)
       			(map 
       				(lambda (x) 
       					(if (eq? x target-type)
       						(lambda (x) x) ; convert to same type
       						(get-coercion x target-type)
       					)
       				) types
       			)
       		)
       		(define (apply-conversions conversions source-args)
       			;(display conversions) (newline)
       			;(display source-args) (newline)
       			(cond 
       				((null? conversions) '())
       				((null? source-args) '())
       				(else
       					;(display "else-append")
		       			(append 
		       				(if (car conversions) 
		       					(list ((car conversions) (car source-args)))
		       					'()
		       				)
		       				(apply-conversions (cdr conversions) (cdr source-args))
		       			)
		       		)
       			)
       		)
       		(define (check-all-same source-tags)
       			(display source-tags) (newline)
       			(cond 
       				((null? source-tags) #f)
       				((symbol? source-tags) #t)
       				((pair? source-tags)
       					(foldr (lambda (x result) (and result (eq? x (car source-tags)))) #t source-tags)
       				)
       				(else #f)
       			)
       		)
       		(define (try-apply op type-tags　args)
       			(define (try-apply-inner next-tags)
       				(display "next-tags = ") (display next-tags) (newline)
       				(if (null? next-tags) 
       					(error "No available conversion")
	                	(if (foldr (lambda (x result) (and x result)) #t (get-conversions type-tags (car next-tags)))
				            (apply apply-generic-coericion-vararg op (apply-conversions (get-conversions type-tags (car next-tags)) args))	                		
	                		(try-apply-inner (cdr next-tags))
	                	)
	       			)
	       		)
	       		(try-apply-inner type-tags)
       		)
            (if proc
                (apply proc (map contents args))
                (if (check-all-same type-tags)
                	(error "All args are already in same type, but no method corresponding installed" type-tags)
                	(try-apply op type-tags args)
                )
            )
       )
  )
)

(define (number->complex z) (make-complex-from-real-imag z 0))
(put-coercion 'number 'complex number->complex)

(define c12 (make-complex-from-real-imag 1 2))
(apply-generic-coericion-vararg 'add-3 1 2 c12)

; not a perfect impl though...

; -----------------------------------------------

; Say these conversions exists
; A -> X 
; C -> X

; exist method (M X X)
; however A can not be converted to C, neither C can be coverted to A

;( integer? '(a 3.1 b 2.7))