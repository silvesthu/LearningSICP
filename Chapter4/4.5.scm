#lang scheme

(cond ((assoc 'b '((a 1) (b 2))) => cadr)
    (else #f))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-actions-special clause) (cddr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses)
            )
            (make-if (cond-predicate first)
                 (if (eq? (car rest) '=>)
                    (list (cond-actions-special first) (cond-predicate first))) ; special form (make a procedure call)
                    (sequence->exp (cond-actions first)) ; ordinary form                    
                 (expand-clauses rest))))))
            