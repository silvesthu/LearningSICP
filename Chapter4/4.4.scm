#lang scheme

; based on 4.3.scm

(define (eval-and exp env)
    (cond 
        ((null? exp) #t)
        ((pair? exp) 
            (if (car exp)
                (eval-and (cdr exp) env)
                #f
            )
        )
        (else exp)
    )
)

; as derived expressions
; (and a b c) -> (if a #t (if b #t c))

(define (and->if exp) (expand-and (cdr exp)))
(define (make-if predicate consequent alternative) (list 'if predicate consequent alternative))
(define (expand-and clause)
    ;(display clause) (newline)
    (if (null? clause) 
        #t
        (if (null? (cdr clause))
            (car clause) ; last element
            (make-if
                (car clause)
                #t
                (expand-and (cdr clause))
            )
        )
    )
)

(eval-and '(#t #t #t) '())
(eval-and '(#t #f #t) '())

(and->if (list 'and #t #f #t))

(define (eval-or exp env)
    (cond 
        ((null? exp) #f)
        ((pair? exp) 
            (if (car exp)
                #t
                (eval-or (cdr exp) env)
            )
        )
        (else exp)
    )
)

; eval-or as derived. should be similar as eval-and. omit

(eval-or '(#f #f #f) '())
(eval-or '(#f #t #f) '())

; (define (and? exp) (tagged-list? exp 'and))
; (define (or? exp) (tagged-list? exp 'or))

; register

;(put and? (eval-and exp env))
;(put or? (eval-or exp env))
