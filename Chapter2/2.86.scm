#lang scheme

;(include "./algebra.scm")

(define table 0)
(set! table '())

; with a simple list of list (not a table actually...)

(define (put op type item) (set! table (append table (list (list op type item)))))
(define (get op type) 
  ;(display "Getting op = ") (display op) (display " , type = ") (display type) (newline)
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

(define (attach-tag-force type-tag contents)
    (cons type-tag contents)
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
(put 'sub '(number number) (lambda (x y) (- x y)))
(put 'mul '(number number) (lambda (x y) (* x y)))
(put 'div '(number number) (lambda (x y) (/ x y)))
(put 'sin 'number (lambda (x) (sin x)))
(put 'cos 'number (lambda (x) (cos x)))
(put 'atan 'number (lambda (x) (atan x)))
(put 'sqrt 'number (lambda (x) (sqrt x)))
(put 'square 'number (lambda (x) (* x x)))
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

  (put 'numer 'rational numer)
  (put 'denom 'rational denom)

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

;;; 直交座標表現
;(define (install-rectangular-package)
;  ;;内部手続き
;  (define (real-part z) (car z))
;  (define (imag-part z) (cdr z))
;  (define (make-from-real-imag x y) (cons x y))
;  (define (magnitude z)
;    (sqrt (+ (square (real-part z))
;             (square (imag-part z)))))
;  (define (angle z)
;    (atan (imag-part z) (real-part z)))
;  (define (make-from-mag-ang r a)
;    (cons (* r (cos a)) (* r (sin a))))
  
;  ;;システムの他の部分とのインターフェース
;  (define (tag x)
;    (attach-tag 'rectangular x))
;  (put 'real-part 'rectangular real-part)
;  (put 'imag-part 'rectangular imag-part)
;  (put 'magnitude 'rectangular magnitude)
;  (put 'angle 'rectangular angle)
;  (put 'make-from-real-imag 'rectangular 
;       (lambda (x y) (tag (make-from-real-imag x y))))
;  (put 'make-from-mag-ang 'rectangular
;       (lambda (r a) (tag (make-from-mag-ang r a))))
;  (put 'equ? '(rectangular rectangular) 
;       (lambda (x y) (and (eq? (real-part x) (real-part y)) (eq? (imag-part x) (imag-part y)))))
;  'done)
  

;;;極座標表現
;(define (install-polar-package)
;  ;;内部手続き
;  (define (magnitude z) (car z))
;  (define (angle z) (cdr z))
;  (define (make-from-mag-ang r a) (cons r a))
;  (define (real-part z)
;    (* (magnitude z) (cos (angle z))))
;  (define (imag-part z)
;    (* (magnitude z) (sin (angle z))))
;  (define (make-from-real-imag x y)
;    (cons (sqrt (+ (square x) (square y)))
;          (atan y x)))
;
;  ;;システムの他の部分とのインターフェイス
;  (define (tag x)
;    (attach-tag 'polar x))
;  (put 'real-part 'polar real-part)
;  (put 'imag-part 'polar imag-part)
;  (put 'magnitude 'polar magnitude)
;  (put 'angle 'polar angle)
;  (put 'make-from-real-imag 'polar
;       (lambda (x y) (tag (make-from-real-imag x y))))
;  (put 'make-from-mag-ang 'polar
;       (lambda (r a) (tag (make-from-mag-ang r a))))
;  (put 'equ? '(polar polar) 
;       (lambda (x y) (and (eq? (magnitude x) (magnitude y)) (eq? (angle x) (angle y)))))
;  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; above copy from http://unlearned.hatenablog.com/entry/20100202/1265117310

(display "Install packages...") (newline)

(install-rational-package)
;(install-polar-package)
;(install-rectangular-package)
;(install-complex-package)

(define (install-real-package) ; e.g. use inexact to represent real
  (define (tag x) (attach-tag-force 'real x)) ; conflict with number.... use an alternative attach-tag...
  (put 'make 'real
       (lambda (n) (tag n)))
  (put 'value 'real
       (lambda (n) (contents n)))
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

(define (apply-generic op . args)
  ;(display (quasiquote (apply-generic called for ,op on ,args)))
  ;(newline)
  (let ((type-tags (map type-tag args)))
    ; (display (quasiquote (type-tags = ,args))) (newline)
    ; unpack type list on single param
    (let ((proc (get op (if (and (pair? type-tags) (eq? (cdr type-tags) null)) (car type-tags) type-tags))))
      ;(display (quasiquote (apply ,proc as ,op of ,type-tags on ,(map contents args)))) (newline)
      (if proc        
        (apply proc (map contents args))
        (error
          "1. No method for these types -- APPLY-GENERIC"
          (list op type-tags))))))

(define (install-simplify-package)
  (put 'simplifiable? 'number
       (lambda (i) #f))
  (put 'simplifiable? 'rational
       (lambda (i) (= (apply-generic 'denom (attach-tag 'rational i)) 1)))
  (put 'simplifiable? 'real
       (lambda (i) (exact? (apply-generic 'value (attach-tag-force 'real i)))))
  (put 'simplifiable? 'complex
       (lambda (i) (= (apply-generic 'imag-part (attach-tag 'complex i)) 0)))

  (put 'simplify 'number
       (lambda (i) (error "can not simplify number")))
  (put 'simplify 'rational
       (lambda (i) (apply-generic 'numer (attach-tag 'rational i))))
  (put 'simplify 'real
       (lambda (i) (make-rational (numerator (apply-generic 'value (attach-tag-force 'real i))) (denominator (apply-generic 'value (attach-tag-force 'real i))))))
  (put 'simplify 'complex
       (lambda (i) (make-real (apply-generic 'real-part (attach-tag 'complex i)))))
)

(install-simplify-package)

;;複素数表現
(define (install-complex-package-compound)
  ;;直交座標と極座標パッケージから取り入れた手続き
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;;内部手続き
  (define (add-complex z1 z2)
    (make-from-real-imag (apply-generic 'add (real-part z1) (real-part z2))
                         (apply-generic 'add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (apply-generic 'sub (real-part z1) (real-part z2))
                         (apply-generic 'sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (apply-generic 'mul (magnitude z1) (magnitude z2))
                       (apply-generic 'add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (apply-generic 'div (magnitude z1) (magnitude z2))
                       (apply-generic 'sub (angle z1) (angle z2))))

  ;;システムの他の部分へのインターフェース
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
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

;; 直交座標表現
(define (install-rectangular-package-compound)
  ;;内部手続き
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (apply-generic 'sqrt (apply-generic 'add (apply-generic 'square (real-part z))
             (apply-generic 'square (imag-part z)))))
  (define (angle z)
    (apply-generic 'atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (apply-generic 'mul r (apply-generic 'cos a)) (apply-generic 'mul r (apply-generic 'sin a))))
  
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
       (lambda (x y) (and (apply-generic 'equ? (real-part x) (real-part y)) (apply-generic 'equ? (imag-part x) (imag-part y)))))
  'done)
  

;;極座標表現
(define (install-polar-package-compound)
  ;;内部手続き
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (apply-generic 'mul (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (apply-generic 'mul (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (apply-generic 'sqrt (apply-generic 'add (square x) (square y)))
          (apply-generic 'atan y x)))

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
       (lambda (x y) (and (apply-generic 'equ? (magnitude x) (magnitude y)) (apply-generic 'equ? (angle x) (angle y)))))
  'done)

; override packages (put impl will not overwrite, so comment out old ones manually)
(install-polar-package-compound)
(install-rectangular-package-compound)
(install-complex-package-compound)

; what to do ?
; 1. add generic sin, cos
; 2. replace all primitive arithmetic method with user-defined ones

(define c1 (make-complex-from-real-imag 1 2))
(define c2 (make-complex-from-real-imag (make-rational 1 2) (make-rational 2 3)))
(apply-generic 'add c2 c2)
(apply-generic 'sub c2 c2)

; still couple of missing methods in rational package, never mind...