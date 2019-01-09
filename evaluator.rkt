#lang racket

(require racket/file)
(require racket/mpair)
(require compatibility/mlist)

(define (mlistify xs)
  (cond ((null? xs) (mlist))
        ((pair? xs) (mcons (mlistify (car xs)) (mlistify (cdr xs))))
        (else xs)))

(define (cadr x) (mcar (mcdr x)))
(define (cddr x) (mcdr (mcdr x)))
(define (caddr x) (mcar (mcdr (mcdr x))))
(define (caadr x) (mcar (mcar (mcdr x))))
(define (cdadr x) (mcdr (mcar (mcdr x))))
(define (cdddr x) (mcdr (mcdr (mcdr x))))
(define (cadddr x) (mcar (mcdr (mcdr (mcdr x)))))

;; evaluator core

; eval
(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((macro-definition? exp) (eval-macro-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (my-eval (cond->if exp) env))
        ((application? exp)
         (let ((op (my-eval (operator exp) env)))
           (if (macro? op)               
               (my-eval (my-apply (make-procedure (macro-parameters op)
                                                  (macro-body op)
                                                  the-global-environment)
                                  (operands exp)) ; a macro doesn't eval its operands
                        env)
               (my-apply op
                         (list-of-values (operands exp) env)))))
        (else
         (error "Unknown expression type -- EVAL" exp))))

; apply
(define (my-apply procedure arguments)
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
         (error "Unknown procedure type -- APPLY" procedure))))

; procedure arguments
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (mcons (my-eval (first-operand exps) env)
             (list-of-values (rest-operands exps) env))))

; conditionals
(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))

; sequences
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (my-eval (first-exp exps) env))
        (else (my-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; assignments and definitions
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (my-eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (my-eval (definition-value exp) env)
                    env)
  'ok)

(define (eval-macro-definition exp env)
  (define-variable! (macro-definition-variable exp)
                    (macro-definition-value exp)
                    env)
  'ok)

;; representing expressions

; self evaluating
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; variables
(define (variable? exp) (symbol? exp))

; quotations
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (mpair? exp)
      (eq? (mcar exp) tag)
      false))

; assignments
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; definitions
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

; macro definitions
(define (macro-definition? exp)
  (tagged-list? exp 'defmacro))
(define (macro-definition-variable exp)
  (caadr exp))
(define (macro-definition-value exp)
  (make-macro (cdadr exp)   ; formal parameters
              (cddr exp)))  ; body

; lambdas

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (mcons 'lambda (mcons parameters body)))

; macros

(define (macro? exp) (tagged-list? exp 'macro))
(define (make-macro parameters body)
  (mcons 'macro (mcons parameters body)))
(define (macro-parameters exp) (cadr exp))
(define (macro-body exp) (cddr exp))

; conditionals

(define (if? exp) (tagged-list? exp  'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (mlist 'if predicate consequent alternative))

; begin

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (mcdr exp))
(define (last-exp? seq) (null? (mcdr seq)))
(define (first-exp seq) (mcar seq))
(define (rest-exps seq) (mcdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (mcons 'begin seq))

; procedure applications

(define (application? exp) (mpair? exp))
(define (operator exp) (mcar exp))
(define (operands exp) (mcdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (mcar ops))
(define (rest-operands ops) (mcdr ops))

; derived expressions

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (mcdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (mcar clause))
(define (cond-actions clause) (mcdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (mcar clauses))
            (rest (mcdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;; evaluator data structures

; testing of predicates

(define (true? x)
  (not (eq? x 'false)))

(define (false? x)
  (eq? x 'false))

; representing procedures

(define (make-procedure parameters body env)
  (mlist 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

; operations on environments

(define (enclosing-environment env) (mcdr env))
(define (first-frame env) (mcar env))
(define the-empty-environment (mlist))

(define (make-frame variables values)
  (mcons variables values))
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

(define (scan-vars vars vals callback)
  (define (loop acc-vars acc-vals vars vals)
    (if (null? vars)
        (callback acc-vars acc-vals)
        (if (symbol? vars)
            (loop (mcons vars acc-vars)
                  (mcons vals acc-vals)
                  '()
                  '())
            (loop (mcons (mcar vars) acc-vars)
                  (mcons (mcar vals) acc-vals)
                  (mcdr vars)
                  (mcdr vals)))))
  (loop '() '() vars vals))

(define (extend-environment vars vals base-env)
  (if (and (pair? vals) (< (mlength vals) (mlength vars)))
      (error "Too few arguments supplied" vars vals)
      (scan-vars vars vals (lambda (vars vals) (mcons (make-frame vars vals) base-env))))) 

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (mcar vals))
            (else (scan (mcdr vars) (mcdr vals)))))
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
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (display var)
  (newline)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;; running the evaluator as a program

(define (boolify fn) (lambda args (if (apply fn args) 'true 'false)))

(define (my-read-string s)
  (my-eval (mlistify (read (open-input-string (string-append "(begin " s ")")))) the-global-environment))

(define (slurp path)
  (file->string path))

(define (user-print object)
  (if (compound-procedure? object)
      (display (mlist 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (if (macro? object)
          (display (mlist 'macro
                     (procedure-parameters object)
                     (procedure-body object)))      
          (display object))))

(define primitive-procedures
  (mlist (mlist 'car mcar)
         (mlist 'cdr mcdr)
         (mlist 'cons mcons)
         (mlist 'null? (boolify null?))
         (mlist 'eq? (boolify eq?))
         (mlist 'pair? (boolify mpair?))
         (mlist '+ +)
         (mlist '- -)
         (mlist '* *)
         (mlist '/ /)
         (mlist '< (boolify <))
         (mlist 'read-string my-read-string)
         (mlist 'slurp slurp)
         (mlist 'eval my-eval)
         (mlist 'apply my-apply)
         (mlist 'display user-print)
         ))

(define (primitive-procedure-names)
  (mmap mcar primitive-procedures))

(define (primitive-procedure-objects)
  (mmap (lambda (proc) (mlist 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true 'true initial-env)
    (define-variable! 'false 'false initial-env)
    initial-env))

(define the-global-environment (setup-environment))
(define-variable! 'the-global-environment the-global-environment the-global-environment)

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) (mlist->list args)))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (my-eval (mlistify input) the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(my-read-string (slurp "core.scm"))

(driver-loop)
