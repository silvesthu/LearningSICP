#lang scheme

;(define (eval exp env)
;  (cond ((self-evaluating? exp) exp)
;        ((variable? exp) (lookup-variable-value exp env))
;        ((quoted? exp) (text-of-quotation exp))
;        ((assignment? exp) (eval-assignment exp env))
;        ((definition? exp) (eval-definition exp env))
;        ((if? exp) (eval-if exp env))
;        ((lambda? exp)
;         (make-procedure (lambda-parameters exp)
;                         (lambda-body exp)
;                         env))
;        ((begin? exp) 
;         (eval-sequence (begin-actions exp) env))
;        ((cond? exp) (eval (cond->if exp) env))
;        ((application? exp)
;         (apply (eval (operator exp) env)
;                (list-of-values (operands exp) env)))
;        (else
;         (error "Unknown expression type -- EVAL" exp))))

; a. then everything will become procedure application cuz that has the most general form...
;    (define x 3) will become a call to procedure define which is not defined rather than a special form for assignment

; b. 

; (define (application? exp) (pair? exp))
; ↓
; (define (application? exp) (tagged-list? exp 'call)))