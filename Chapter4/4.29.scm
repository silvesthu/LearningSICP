#lang scheme

(include "lazy.scm")

(eval@ '(define count 0) the-global-environment)
(eval@ '(define (id x)
			(set! count (+ count 1)) x) the-global-environment)

(eval@ '(define (square x) (* x x)) the-global-environment)
(eval@ '(define (test)
			(define (some-complicated-calculation) (id 0))
			(define (some-recursive input count)
				(if (> count 0) (+ input (some-recursive input (- count 1))) 0)
			)
			(some-recursive (some-complicated-calculation) 1000)
			) the-global-environment)

(eval@ '(square (id 10)) the-global-environment)
; 100
(eval@ (actual-value 'count the-global-environment) the-global-environment)
; 1

; reset count
(eval@ '(set! count 0) the-global-environment)

(eval@ '(test) the-global-environment)
(eval@ (actual-value 'count the-global-environment) the-global-environment)
; 1

; disable memorize and reset count
(eval@ '(set! count 0) the-global-environment)
(set! force-it-enable-mem #f)

(eval@ '(square (id 10)) the-global-environment)
; 100
(eval@ (actual-value 'count the-global-environment) the-global-environment)
; 2

; reset count
(eval@ '(set! count 0) the-global-environment)

(eval@ '(test) the-global-environment)
(eval@ (actual-value 'count the-global-environment) the-global-environment)
; 1000

