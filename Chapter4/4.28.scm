#lang scheme

(include "lazy.scm")

(eval@ '(define (x) 1) the-global-environment)
(eval@ '(define (call c) (c)) the-global-environment)
(eval@ '(call x) the-global-environment)

; cuz eval-lazy do not handle trunk explicitly... 
; the statement above will not work unless a actual-value presents
