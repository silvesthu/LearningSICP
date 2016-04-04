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


; suppose we have put/get as in Chapter2

(define op-map ~) ; list of pred/eval-op pair
(define (put pred eval-op) ~)
(define (get pred) ~)

(define (eval-operator exp env) ; find op in list
	(define (eval-operator-inner exp env ops)
		(if (empty? ops) 
			(error "no registered operation for expression" exp)
			(if ((car (car ops)) exp)
				((cadr (car ops)) exp env) ; evaluation
				(eval-operator-inner exp env (cdr ops))
			)
		)		
	)
	(eval-operator-inner exp env op-map)
)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ;((variable? exp) (lookup-variable-value exp env))
        ;((quoted? exp) (text-of-quotation exp))
        ;((assignment? exp) (eval-assignment exp env))
        ;((definition? exp) (eval-definition exp env))
        ;((if? exp) (eval-if exp env))
        ;((lambda? exp)
        ; (make-procedure (lambda-parameters exp)
        ;                 (lambda-body exp)
        ;                 env))
        ;((begin? exp) 
        ; (eval-sequence (begin-actions exp) env))
        ;((cond? exp) (eval (cond->if exp) env))
        ((operator? exp) (eval-operator exp env))
        ;((application? exp)
        ; (apply (eval (operator exp) env)
        ;        (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

; put cases into list
; (put variable? (lookup-variable-value exp env))
; etc.