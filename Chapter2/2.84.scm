#lang scheme

; raise in type tower

; number, rational, complex

(include "./algebra.scm")

(define tower '(number rational complex))

(define (convertable? x y)	
	(define (convertable?-inner left)
		(cond 
			((null? left) #f)
			((eq? (car left) y) #f)
			((eq? (car left) x) #t)
			(else (convertable?-inner (cdr left)))
		)
	)
	(if (and (memq x tower) (memq y tower))
		(if (eq? x y)
			#t
			(convertable?-inner tower)
		)		
		(error "Type not in tower")
	)	
)

(define (data-convertable? x to-type)	
	(convertable? (type-tag x) to-type)
)

;(convertable? 'number 'number)
;(convertable? 'number 'complex)
;(convertable? 'complex 'number)
;(convertable? 'complex 'rational)

; --------------------------------------------

; raise from 2.83

(define (install-real-package) ; e.g. use inexact to represent real
  (define (tag x) (attach-tag-force 'real x)) ; conflict with number.... use an alternative attach-tag...
  (put 'make 'real
       (lambda (n) (tag n)))
  'done)

(install-real-package)

(define (make-real n)
  ((get 'make 'real) n))

(define (install-raise-package)
	(put 'raise 'number
       (lambda (i) (make-rational i 1)))
	(put 'raise 'rational
       (lambda (i) (make-real (/ (car i) (cdr i)))))
	(put 'raise 'real
       (lambda (i) (make-complex-from-real-imag i 0)))
)

(install-raise-package)

; --------------------------------------------

(define (convert data to-type)
	(define (raise-until data to-type)
		(if (eq? (type-tag data) to-type)
			data
			(raise-until (apply-generic 'raise data) to-type)
		)
	)
	(if (convertable? (type-tag data) to-type)
		(raise-until data to-type)
		#f
	)
)

;(convert 7 'complex)

; --------------------------------------------

; apply-generic with convert support

(define (apply-generic-convert op . args)
	(define (convert-all args to-type)
		(let ((converted (map (lambda (x) (data-convertable? x to-type)) args)))
			(if (memq #f converted)
				#f
				(map (lambda (x) (convert x to-type)) args)
			)
		)
	)
	(define (apply-generic-convert-inner args next-args)
		(if (pair? next-args)
			(let ((result (convert-all args (type-tag (car next-args)))))
				(if result
					(let ((proc (get op (map type-tag result))))
						; (display "proc = ") (display proc) (display " on ") (display result) (newline)
						(if proc
							(apply proc (map contents result))
							(error "no suitable method to apply")
						)
					)
					(apply-generic-convert-inner args (cdr next-args))
				)
			)
			(error "no avaiable conversion")
		)
	)
	(let ((type-tags (map type-tag args)))
		(cond 
			((null? args) (error "no arg"))
			((and (pair? args) (null? (cdr args)))
				(let ((proc (get op (if (and (pair? type-tags) (eq? (cdr type-tags) null)) (car type-tags) type-tags))))
					(if proc
						(proc (car args))
						(error "no suitable method to apply")
					)
				)
			)
			(else (apply-generic-convert-inner args args))
		)
	)
)


(apply-generic-convert 'raise 1)
(apply-generic-convert 'add 1 2)
(apply-generic-convert 'add 1 (make-rational 1 2))
(apply-generic-convert 'add 1 (make-complex-from-real-imag 3 4))
(apply-generic-convert 'add (make-rational 1 3) (make-complex-from-real-imag 3 4))