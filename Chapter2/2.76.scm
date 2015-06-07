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

(newline)

; quote

; 1. quasiquote
(quasiquote (1 2 (unquote (+ 1 2)) (unquote (- 5 1))))
; => (1 2 3 4)
; => quasiquote can be unquoted inside (partial eval)

; 2. quote
(quote (1 2 (unquote (+ 1 2)) (unquote (- 5 1))))
; => quote can't be unquoted (partial evaluated)

; 3. unquote-slicing
`(1 2 ,@(list (+ 1 2) (- 5 1)))
; => (1 2 3 4)

`(1 2 ,(list (+ 1 2) (- 5 1)))
; => (1 2 (3 4))

`(1 2 ,(+ 1 2) ,(- 5 1))
; => (1 2 3 4)

; quote : program as data

; 4. eval : data as program

(define ns (make-base-namespace)) ; must specify some namespace in Racket
(eval '(cons 1 2) ns) ; works

(define (call-binary op left right)
	(eval (list op left right) ns)
)

(call-binary '+ 1 2)
(call-binary '* 2 3)
