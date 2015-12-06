#lang scheme

; oh reversable calculation !

; -- copy from https://github.com/ten0s/sicp/blob/master/ch03/constraint.scm --

;
; Connector
;

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
	(define (set-my-value! newval setter)
	  (cond ((not (has-value? me))
			 (set! value newval)
			 (set! informant setter)
			 (for-each-except setter
							  inform-about-value
							  constraints))
			((not (= value newval))
			 (error "Contradiction" (list value newval)))
			(else 'ignored)))
	(define (forget-my-value! retractor)
	  (if (eq? retractor informant)
		  (begin (set! informant false)
				 (for-each-except retractor
								  inform-about-no-value
								  constraints))
		  'ignored))
	(define (connect new-constraint)
	  (if (not (memq new-constraint constraints))
		  (set! constraints
				(cons new-constraint constraints))
		  '())
	  (if (has-value? me)
		  (inform-about-value new-constraint)
		  '())
	  'done)
	(define (me request)
	  (cond ((eq? request 'has-value?)
			 (if informant true false))
			((eq? request 'get-value) value)
			((eq? request 'set-value!) set-my-value!)
			((eq? request 'forget-value!) forget-my-value!)
			((eq? request 'connect) connect)
			(else
			 (error "Unknown request -- MAKE-CONNECTOR" request))))
	me))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'get-value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget-value!) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (for-each-except exception procedure list)
  (define (loop items)
	(cond ((null? items) 'done)
		  ((eq? (car items) exception) (loop (cdr items)))
		  (else (procedure (car items))
				(loop (cdr items)))))
  (loop list))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

;
; Adder
;

(define (adder a1 a2 sum)
  (define (process-new-value)
	(cond ((and (has-value? a1) (has-value? a2))
		   (set-value! sum
					   (+ (get-value a1) (get-value a2))
					   me))
		  ((and (has-value? a1) (has-value? sum))
		   (set-value! a2
					   (- (get-value sum) (get-value a1))
					   me))
		  ((and (has-value? a2) (has-value? sum))
		   (set-value! a1
					   (- (get-value sum) (get-value a2))
					   me))))
  (define (process-forget-value)
	(forget-value! a1 me)
	(forget-value! a2 me)
	(forget-value! sum me)
	(process-new-value))
  (define (me request)
	(cond ((eq? request 'I-have-a-value)
		   (process-new-value))
		  ((eq? request 'I-lost-my-value)
		   (process-forget-value))
		  (else
		   (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

;
; Multiplier
;

(define (multiplier m1 m2 product)
  (define (process-new-value)
	(cond ((or (and (has-value? m1) (= (get-value m1) 0))
			   (and (has-value? m2) (= (get-value m2) 0)))
		   (set-value! product 0 me))
		  ((and (has-value? m1) (has-value? m2))
		   (set-value! product
					   (* (get-value m1) (get-value m2))
					   me))
		  ((and (has-value? m1) (has-value? product))
		   (set-value! m2
					   (/ (get-value product) (get-value m1))
					   me))
		  ((and (has-value? m2) (has-value? product))
		   (set-value! m1
					   (/ (get-value product) (get-value m2))
					   me))))
  (define (process-forget-value)
	(forget-value! m1 me)
	(forget-value! m2 me)
	(forget-value! product me))
  (define (me request)
	(cond ((eq? request 'I-have-a-value)
		   (process-new-value))
		  ((eq? request 'I-lost-my-value)
		   (process-forget-value))
		  (else
		   (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

;
; Constant
;

(define (constant value connector)
  (define (me request)
	(error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

;
; Probe
;

(define (probe name connector)
  (define (print-probe value)
	(newline)
	(display "Probe: ")
	(display name)
	(display " = ")
	(display value))
  (define (process-new-value)
	(print-probe (get-value connector)))
  (define (process-forget-value)
	(print-probe "?"))
  (define (me request)
	(cond ((eq? request 'I-have-a-value)
		   (process-new-value))
		  ((eq? request 'I-lost-my-value)
		   (process-forget-value))
		  (else
		   (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

; -- 3.33 -------------------------

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))

(define (average A B C)
	(let 
		(
			(two (make-connector))
			(equal (make-connector))
		)
		(adder A B equal)
		(constant 2 two)
		(multiplier C two equal)
		'ok
	)
)

(average A B C)

(probe "A" A)
(probe "B" B)
(probe "C" C)

(set-value! A 8 'user)
(set-value! B 12 'user)
; (set-value! C 5 'user) ; contradiction

(forget-value! A 'user)
(set-value! C 12 'user)