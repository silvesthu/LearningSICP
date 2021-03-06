; from http://www-inst.eecs.berkeley.edu/~cs61a/sp09/library/code/ch4.scm

(require r5rs)

(define ENABLE-DEBUG-DISPLAY #f)

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval@ (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval@ (if-predicate exp) env))
      (eval@ (if-consequent exp) env)
      (eval@ (if-alternative exp) env)))

(define (eval-sequence exps env)
  ;(set! counter-eval (+ counter-eval 1))
  (if ENABLE-DEBUG-DISPLAY (begin (display exps) (newline)))
  (cond ((last-exp? exps) (eval@ (first-exp exps) env))
        (else (eval@ (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; 4.30 Cy
;(define (eval-sequence exps env)
;  (if ENABLE-DEBUG-DISPLAY (begin (display exps) (newline)))
;  (cond ((last-exp? exps) (eval@ (first-exp exps) env))
;        (else (actual-value (first-exp exps) env)
;              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval@ (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval@ (definition-value exp) env)
                    env)
  'ok)

;;;SECTION 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))


(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))


(define (make-if predicate consequent alternative)
  ; (display "<make-if>") (newline)
  ; (display predicate) (newline)
  ; (display consequent) (newline)
  ; (display alternative) (newline)
  ; (display "</make-if>") (newline)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

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
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (filter pred x)
      (cond 
        ((null? x) x)
        ((pred (car x)) (cons (car x) (filter pred (cdr x))))
        (else (filter pred (cdr x)))))

(define (scan-out-defines proc-exp)
 (define keyword (car proc-exp)) ; 'procedure
 (define parameters (cadr proc-exp))
 (define expressions (cddr proc-exp))
 (define var-defines 
  (filter (lambda (e) (and (pair? e) (eq? (car e) 'define) (not (pair? (cadr e))))) (caddr proc-exp)))
 (define proc-defines 
  (filter (lambda (e) (and (pair? e) (eq? (car e) 'define) (pair? (cadr e)))) (caddr proc-exp)))
 (define non-defines (filter (lambda (e) (not (and (pair? e) (eq? (car e) 'define)))) (caddr proc-exp)))
 (define (name n) (if (pair? n) (car n) n))
 (list keyword parameters 
    (list (append (list 'let)
      (list (map (lambda (d) (list (name (cadr d)) ''*unassigned*)) (append var-defines proc-defines)))
      (map (lambda (d) (list 'set! (cadr d) (list (caddr d)))) 
        var-defines)
      (map (lambda (d) (list 'set! (caadr d) (make-lambda (cdadr d) (list (caddr d))))) proc-defines)
        non-defines)))
)

(define (make-procedure parameters body env)
  ;(display (list 'procedure parameters body env)) (newline) (newline)
  ;(display (append (scan-out-defines (list 'procedure parameters body)) (list env))) (newline) (newline)
  ;(append (scan-out-defines (list 'procedure parameters body)) (list env)))
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
              (error "Unassigned variable" var)
              (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'caddr caddr)
        (list 'caadr caadr)        
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'list list)
        (list 'cons cons)
        (list 'display display)
        (list 'newline newline)
        (list '< <)
        (list '> >)
        (list '= =)
        (list '<= <=)
        (list '>= >=)
        (list 'not not)
;;      more primitives
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))
(define keyword-table '())
(define (put-keyword op proc) (set! keyword-table (append keyword-table (list (list op proc)))))
(define (get-keyword exp) 
  (define (get-inner t)
    (cond 
      ((null? t) #f)
      ((tagged-list? exp (caar t)) (cadar t))
      (else (get-inner (cdr t)))
    )
  )
  (get-inner keyword-table)
)

(define (eval@ exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval@ (cond->if exp) env))
        ((get-keyword exp) ((get-keyword exp) exp env))
        ((application? exp)
         (apply@ (eval@ (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply@ procedure arguments)
  ;(set! counter-execute (+ counter-execute 1))
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

;(put-keyword 'TEST-IN (lambda (exp env) 'TEST-OUT))
;(eval@ '(TEST-IN 1 2 3) the-global-environment)

; ------------------------------------------

; port from 4.4
(define (eval-or exp-oprands env)
  (define core 
    (cond 
      ((null? exp-oprands) #f)
      ((empty? exp-oprands) #f)
      ((pair? exp-oprands)
          (if (eval@ (car exp-oprands) env)
              #t
              (eval-or (cdr exp-oprands) env)
          )
      )
      (else exp-oprands)
    )
  )
  ;(display "<eval-or>") (newline)
  ;(display exp-oprands) (newline)
  ;(display core) (newline)
  ;(display "</eval-or>") (newline)
  core
)

(put-keyword 'or (lambda (exp env) (eval-or (cdr exp) env)))
;(eval@ '(or (< 1 0)) the-global-environment)

; -------------------------------------------

; port from 4.6
(define (let->lambda exp)
  (define clauses (cdr exp))
  (if (null? clauses)
      'false
      ;(error "let->lambda" (car clauses)))
      (let (
          (raw-defines (car clauses)) 
          (expressions (cdr clauses))
        )
        (define defines (map (lambda (d) (cons 'define d)) raw-defines))
        ;(error "defines" defines)
        (sequence->exp (append defines expressions))
      ))
)

(put-keyword 'let (lambda (exp env) (eval@ (let->lambda exp) env)))


;(define (let->lambda-parameters lets)
;  (map car lets)
;)

;(define (let->arguments lets)
;  (map cdr lets)
;)

;(define (let->combination exp)
;  (let 
;    (
;      (let-keyword (car exp))
;      (lets (cadr exp))
;      (body (caddr))
;    )
;    (apply (make-lambda (let->lambda-parameters lets) body) (sequence->exp (let->arguments lets)))
;  )
;)

;;; Support for Let (as noted in footnote 56, p.428)

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-var binding) (car binding))
(define (let-val binding) (cadr binding))

(define (make-combination operator operands) (cons operator operands))

(define (let->combination exp)
  ;;make-combination defined in earlier exercise
  (let ((bindings (let-bindings exp)))
    (make-combination (make-lambda (map let-var bindings)
                                   (let-body exp))
                      (map let-val bindings))))

;;;Code from SECTION 4.3.3, modified as needed to run it

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

;; analyze from 4.1.6, with clause from 4.3.3 added
;; and also support for Let
(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

;;;Simple expressions

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

;;;Conditionals and sequences

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

;;;Definitions and assignments

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env                        
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
               (let ((old-value
                      (lookup-variable-value var env))) 
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

;;;Procedure applications

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    ;; success continuation for this aproc
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                ;; success continuation for recursive
                                ;; call to get-args
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;;;amb expressions

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

