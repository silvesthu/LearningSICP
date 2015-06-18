#lang scheme

(define table 0)
(set! table '())

; with a simple list of list (not a table actually...)

(define (put op type item) (set! table (append table (list (list op type item)))))
(define (get op type) 
  (define (get-inner op type t)
    (cond 
      ((null? t) #f)
      ((and (equal? op (caar t)) (equal? type (cadar t))) (caddar t))
      (else (get-inner op type (cdr t)))
    )
  )
  (get-inner op type table)
)

; above from 2.73

(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
    (else (cons type-tag contents)))
  ) 

(define (type-tag datum)
  (cond
    ((number? datum) 'number) ; dummy
    ((pair? datum) (car datum))
    (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (cond 
    ((number? datum) datum)
    ((pair? datum) (cdr datum))
    (error "Bad tagged datum -- CONTENTS" datum)))

(put 'add '(number number) (lambda (x y) (+ x y))) ; still have to port....
(put 'equ? '(number number) (lambda (x y) (eq? x y))) ; still have to port....
;(put 'add-3 '(number number number) (lambda (x y z) (+ x y z))) ; still have to port....
(put '=zero? 'number (lambda (x) (eq? x 0))) ; still have to port....

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

;;有理数パッケージ
(define (install-rational-package)
  ;;内部手続き
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;;システムの他の部分へのインターフェース
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'equ? '(rational rational) 
       (lambda (x y) (and (eq? (numer x) (numer y)) (eq? (denom x) (denom y)))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))



;;複素数表現
(define (install-complex-package)
  ;;直交座標と極座標パッケージから取り入れた手続き
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;;内部手続き
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;;システムの他の部分へのインターフェース
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'add-3 '(complex complex complex)
       (lambda (z1 z2 z3) (tag (add-complex (add-complex z1 z2) z3))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'equ? '(complex complex) equ?)

  ; 2.77 begin (dispatch to detail type)
  (put 'real-part 'complex real-part)
  (put 'imag-part 'complex imag-part)
  (put 'magnitude 'complex magnitude)
  (put 'angle 'complex angle)
  ; 2.77 end

  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (square x)
  (* x x))

;; 直交座標表現
(define (install-rectangular-package)
  ;;内部手続き
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  
  ;;システムの他の部分とのインターフェース
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'real-part 'rectangular real-part)
  (put 'imag-part 'rectangular imag-part)
  (put 'magnitude 'rectangular magnitude)
  (put 'angle 'rectangular angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(rectangular rectangular) 
       (lambda (x y) (and (eq? (real-part x) (real-part y)) (eq? (imag-part x) (imag-part y)))))
  'done)
  

;;極座標表現
(define (install-polar-package)
  ;;内部手続き
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;;システムの他の部分とのインターフェイス
  (define (tag x)
    (attach-tag 'polar x))
  (put 'real-part 'polar real-part)
  (put 'imag-part 'polar imag-part)
  (put 'magnitude 'polar magnitude)
  (put 'angle 'polar angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(polar polar) 
       (lambda (x y) (and (eq? (magnitude x) (magnitude y)) (eq? (angle x) (angle y)))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; above copy from http://unlearned.hatenablog.com/entry/20100202/1265117310

(newline)(newline)

(install-rational-package)
(install-polar-package)
(install-rectangular-package)
(install-complex-package)

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

(define (apply-generic op . args)
  ;(display (quasiquote (apply-generic called for ,op on ,args)))
  ;(newline)
  (let ((type-tags (map type-tag args)))
    ;(display (quasiquote (type-tags = ,args))) (newline)
    ; unpack type list on single param
    (let ((proc (get op (if (and (pair? type-tags) (eq? (cdr type-tags) null)) (car type-tags) type-tags))))
      ;(display (quasiquote (apply ,proc as ,op of ,type-tags on ,(map contents args)))) (newline)
      (if proc        
        (apply proc (map contents args))
        (error
          "1. No method for these types -- APPLY-GENERIC"
          (list op type-tags))))))

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