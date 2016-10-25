(include "header.scm")

; from ch4.scm

(define (actual-value exp env)
  (force-it (eval-lazy exp env)))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define force-it-enable-mem #t)

(define (force-it-nonmem obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

(define (force-it-mem obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (force-it obj)
    (if force-it-enable-mem 
      (force-it-mem obj)
      (force-it-nonmem obj))
)

(define (apply-lazy procedure arguments env)
  ; (let 
  ;   ((value 
  ;     (if (eq? (cadr procedure) display)
  ;        (if (pair? (car arguments))
  ;         ; list -> print the list
  ;         (if (eq? (caar arguments) 'quote)
  ;           ; quote
  ;           (begin 
  ;             ;(display (cadar arguments)) 
  ;           #f)
  ;           ; other
  ;           (begin 
  ;             ;(display (car arguments)) 
  ;           #f) ; pass to default procedure
  ;         )
  ;         ; single value -> evaluate
  ;         (begin (display (lookup-variable-value (car arguments) env)) #t)
  ;        )
  ;        #f ; pass to default procedure
  ;     )))
  ;   (if value
  ;     value
  ;     (apply-lazy-inner procedure arguments env)
  ;   )
  ; )
  (apply-lazy-inner procedure arguments env)
)

(define (apply-lazy-inner procedure arguments env)
;  (display procedure) (newline)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (quote-to-list exp env)
  ; (newline)
  ; (display "quote-to-list ")(display exp)(display " --- ")(display (pair? exp))
  ; (newline)
  (if (and (pair? exp) (not (eq? (car exp) 'quote)))
    (list 'cons (list 'quote (car exp)) (quote-to-list (cdr exp) env))
    exp
  )
)

(define (text-of-quotation-lazy exp env)
  (if (pair? (cadr exp))
    ; list -> e.g. '(a b c)
    (begin
      ; (newline)
      ; (display "text-of-quotation-lazy ")(display exp);(display " --- ") (display (quote-to-list (cadr exp) env))
      ; (newline)
      (eval-lazy (quote-to-list (cadr exp) env) env)
    )
    ; single value -> e.g. 'a
    (begin 
      ; (newline)
      ; (display "text-of-quotation-lazy ")(display exp)
      ; (newline)
      (text-of-quotation exp)  
    )
  )
)

(define (eval-lazy exp env)
  ; (newline)
  ; (display "eval-lazy ") (display exp)
  ; (newline)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation-lazy exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval-lazy (cond->if exp) env))
        ;((eq? (car exp) 'car) (display (operands exp)))
        ((application? exp)		;**
         (apply-lazy (actual-value (operator exp) env)
         ;(apply-lazy (eval-lazy (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;(define (try a b)
;  (if (= a 0) 1 b))

;(try 0 (/ 1 0))

(set! apply@ apply-lazy)
(set! eval@ eval-lazy)
 
; simple test
;(eval@
;	; as a special form
;	'(begin 
;		(define (try a b)
;		  (if (= a 0) 1 b))
;		(try 0 (/ 1 0))
;	)
;	the-global-environment)