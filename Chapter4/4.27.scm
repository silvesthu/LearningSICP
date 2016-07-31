#lang scheme

(include "lazy.scm")

(eval@ '(define count 0) the-global-environment)
(eval@ '(define (id x)
			(set! count (+ count 1)) x) the-global-environment)
(eval@ '(define w (id (id 10))) the-global-environment)
(eval@ (actual-value 'count the-global-environment) the-global-environment)
(eval@ (actual-value 'w the-global-environment) the-global-environment)
(eval@ (actual-value 'count the-global-environment) the-global-environment)
(eval@ (actual-value 'w the-global-environment) the-global-environment)
(eval@ (actual-value 'count the-global-environment) the-global-environment)

; call actual-value directly as in interactive IO

; result is 

; ok
; ok
; ok
; 1  -> (id 10) is delayed -> the outer id increment count
; 10 -> evaluate (id 10) -> gives 10
; 2 -> count incremented
; 10
; 2 -> result is memorized, not id performed