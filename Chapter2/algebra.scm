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
(put 'gcd '(number number) (lambda (x y) 
  (display "gcd (n) ") (display x) (display ", ") (display y) (newline)
  (gcd x y)))
(put 'sin 'number (lambda (x) (sin x)))
(put 'cos 'number (lambda (x) (cos x)))
(put 'atan 'number (lambda (x) (atan x)))
(put 'sqrt 'number (lambda (x) (sqrt x)))
(put 'square 'number (lambda (x) (* x x)))
(put 'equ? '(number number) (lambda (x y) (eq? x y))) ; still have to port....
(put 'reduce '(number number) (lambda (x y) 
  (display "reduce (n) ") (display x) (display ", ") (display y) (newline)
  (let ((g (gcd x y)))
    (cons (/ x g) (/ y g))
  )
))
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
    (let ((g (apply-generic 'gcd n d)))
      ;(cons (div n g) (div d g)))
      ;(cons n d))
      (apply-generic 'reduce n d))
  )
  (define (add-rat x y)
    ;(display "add-rat (r) ") (display x) (display ", ") (display y) (newline)
    (make-rat (apply-generic 'add (apply-generic 'mul (numer x) (denom y))
                 (apply-generic 'mul (numer y) (denom x)))
              (apply-generic 'mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (apply-generic 'sub (apply-generic 'mul (numer x) (denom y))
                 (apply-generic 'mul (numer y) (denom x)))
              (apply-generic 'mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (apply-generic 'mul (numer x) (numer y))
              (apply-generic 'mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (apply-generic 'mul (numer x) (denom y))
              (apply-generic 'mul (denom x) (numer y))))

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

(display "Install packages...") (newline)

(install-rational-package)
(install-polar-package)
(install-rectangular-package)
(install-complex-package)

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
          "No method for these types -- APPLY-GENERIC"
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


; -------------- http://cparrish.sewanee.edu/cs376/sicp/polynomial-package.ss

(define (install-polynomial-package)
  
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
    (cond ((empty-termlist? L1) (map opterm L2))
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
    ;(display "L1 = ") (display L1) (newline)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
  
  (define (mul-term-by-all-terms t1 L)
    ;(display "t1 = ") (display t1) (newline)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (add (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (div-terms L1 L2)
    ;(newline)
    ;(display "div ") (display L1) (display " ~ ") (display L2) (newline)
    (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
          (list (the-empty-termlist) L1)
          (let ((new-c (div (coeff t1) (coeff t2)))
                (new-o (- (order t1) (order t2))))
            (let ((rest-of-result
              ;<compute rest of result recursively>
              (sub-terms L1 (mul-terms L2 (list (make-term new-o new-c))))))
              ;(display "sub ") (display rest-of-result) (display " ~ ") (display L1) (display " - ") (display (mul-terms L2 (list (make-term new-o new-c)))) (newline)
              (let ((inner (div-terms rest-of-result L2)))
                (list
                  (add-terms (list (make-term new-o new-c)) (car inner))
                  (cadr inner)
                )
              )
              ;<form complete result>
            )
          )
        )
      )
    )
  )

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      ;(make-poly (variable p1)
      (let ((result (div-terms (term-list p1) (term-list p2))))
        (cons 
          (make-poly (variable p1) (car result))
          (make-poly (variable p1) (cdr result))
        )
      )
      (error "Polys not in same var -- DIV-POLY"
             (list p1 p2))))

  (define (remainder-terms a b)
    ;(display "a = ") (display a) (newline)
    ;(display "b = ") (display b) (newline)
    (cadr (div-terms a b))
  )

  ; 2.96
  (define (pseudoremainder-terms a b)
    ;(display "pseudo-div ") (display a) (display ", ") (display b) (newline)
    (let ((e 
          (list 
            (list 
              0 
              (expt (coeff (first-term b)) (+ 1 (order (first-term a)) (- (order (first-term b)))))
            )
          )))
      
      (let ((d
        (div-terms 
          (mul-terms a e) 
          b
        )))
        ;(display "e = ") (display e) (newline)
        ;(display (car d)) (display "     ~    ") (display (cadr d)) (newline)
        ;(if (empty-termlist? (cadr d))
        (cadr d)
        ;  (the-empty-termlist)
        ;)
      )
    )
  )

  (define (gcd-terms a b)
    ;(display "gcd-terms") (display a) (display ", ") (display b) (newline)
    (if (empty-termlist? b)
      a
      ;(gcd-terms b (remainder-terms a b))
      (gcd-terms b (pseudoremainder-terms a b))
    )
  )

  ;(define (reduce-terms n d)
  ;  ;(display "reduce-terms ") (display n) (display ", ") (display d) (newline)
  ;  ;(display "result = ") 
  ;  ;(display     
  ;  ;  (let ((g (gcd-terms n d)))
  ;  ;    (list (car (div-terms n g)) (car (div-terms d g))))) (newline)
  ;  (let ((g (gcd-terms n d)))
  ;    (list (car (div-terms n g)) (car (div-terms d g)))
  ;  )    
  ;)

  ;(define (reduce-poly p1 p2)
  ;  ;(display "reduce-poly ") (display p1) (display ", ") (display p2) (newline)
  ;  (if (same-variable? (variable p1) (variable p2))
  ;    (list (make-poly (variable p1)
  ;               (car (reduce-terms (term-list p1)
  ;                             (term-list p2))))
  ;          (make-poly (variable p1)
  ;               (cadr (reduce-terms (term-list p1)
  ;                             (term-list p2))))
  ;    )
  ;    (error "Polys not in same var -- REDUCE-POLY"
  ;           (list p1 p2)))
  ;)

  (define (reduce-coeffs a) 
    ;(let ((g (map (lambda (x) (coeff)) a))))
    ;(display (map (lambda (x) (coeff x)) a))
    ;(display "reduce-coeffs ") (display a) (newline)
    (let ((g (apply gcd (map (lambda (x) (coeff x)) a))))
      (map (lambda (x) (make-term (order x) (/ (coeff x) g))) a)
    )
  )

  (define (gcd-poly p1 p2)
    ;(display "gcd terms = ") (display (gcd-terms (term-list p1) (term-list p2))) (newline)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (reduce-coeffs (gcd-terms (term-list p1)
                                            (term-list p2))))
      (error "Polys not in same var -- DIV-POLY"
             (list p1 p2))))  
  
  ;; interface to rest of the system
  
  (define (tag p) (attach-tag 'polynomial p))
  
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))

  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'div '(polynomial polynomial) 
       (lambda (p1 p2) 
       (list 
          (tag (car (div-poly p1 p2))) 
          (tag (cdr (div-poly p1 p2)))
       ))
  )

  (put 'gcd '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))

  (put 'reduce '(polynomial polynomial)
    (lambda (p1 p2) 
       (let ((g (gcd-poly p1 p2)))
        ;(display "g = ")(display g) (newline)
        (cons 
          (tag (car (div-poly p1 g))) 
          (tag (car (div-poly p2 g)))
       ))
    )
  )
  
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)


;; Constructor 
;; (which must be visible to the outside world)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

; ------------ http://cparrish.sewanee.edu/cs376/sicp/polynomial-package.ss

(install-polynomial-package)