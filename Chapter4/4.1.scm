#lang scheme

; dummy
(define (no-operands? exps) (null? exps))
(define (first-operand exps) (car exps))
(define (rest-operands exps) (cdr exps))
(define (eval exp env) (display exp) (display " "))

(define (list-of-values-left exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values-left (rest-operands exps) env))))

(define dummy-left (list-of-values-left '(1 2 3 4 5) '()))
(newline)

(define (list-of-values-right exps env)
  (if (no-operands? exps)
      '()
      ; use let to ensure rest part is evaluated first
      ; recursive will make sure the right-most comes first
      (let ((rest (list-of-values-right (rest-operands exps) env)))
      (cons (eval (first-operand exps) env)
            rest))))

(define dummy-right (list-of-values-right '(1 2 3 4 5) '()))
(newline)
