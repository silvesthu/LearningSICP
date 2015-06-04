#lang scheme

; Explicit dispatch

	; Add type

		; Add implementation
		; Add branch in generic interface

	; Add op

		; Add implementation
		; Add new generic interface

; Data-directed style

	; Add type

		; Add package of functions and a installer

	; Add op

		; Add generic interface, and implementation in each package

; Message-passing-style

	; Add type

		; Write a new constructor

	; Add op

		; Add new branch of cond in exist constructor

; -------------

; Best for adding type often : Message-passing-style

; Best for adding op often : Data-directed style ? (or Message-passing-style...)


; -------------

; Some misunderstanding has been clarified !

; 1. Function closure as object (and set! works on local names !)

(define (Counter init) ; only constructor
	(define value init) ; declaration is necessary (otherwise unbound identifier access)
	(define (call method . arg) ; function table
		(cond ; dispatch
			((eq? method 'inc) (set! value (+ value 1)))
			((eq? method 'get) value)
			((eq? method 'set) (set! value (car arg)))
			((eq? method 'expand))
			; ... more
			(else (error "invalid method call" method))
		)
	)
	call
)

(define counter (Counter 0))
;(counter 'get)
;(counter 'inc)
;(counter 'get)
;(counter 'inc)
;(counter 'get)
;(counter 'set 1024)
;(counter 'get)

(define (Step2Counter init)
	(define counter (Counter init))
	(define (call method . arg) ; function table
		(define (call-inner method args)
			(cond ; dispatch
				((eq? method 'inc) (counter 'set (+ (counter 'get) 2)))
				; (else (counter method arg)) ; how to pass down vararg ?
				; => apply works as opposite to vararg
				
				; equivalent forms
				(else (apply counter method arg))
				;(else (apply counter (cons method arg)))

				; apply itself also accept varargs in form of
				; (define (apply func arg0 arg1 ... args)
				;	(apply func (append (list arg0 arg1 ...) args))
				; )

				; final form
				; (define (apply func args)
				; 	<Internal implementation>
				; )
			)
		)
		(call-inner method arg)
	)
	call
)

(define counter-2 (Step2Counter 0))
(counter-2 'get)
(counter-2 'inc)
(counter-2 'get)
(counter-2 'set 1024)
(counter-2 'get)