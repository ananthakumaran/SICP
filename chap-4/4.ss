;; *Exercise 4.4:* Recall the definitions of the special forms `and'
;; and `or' from *Note Chapter 1:::

;; * `and': The expressions are evaluated from left to right.  If
;; any expression evaluates to false, false is returned; any
;; remaining expressions are not evaluated.  If all the
;; expressions evaluate to true values, the value of the last
;; expression is returned.  If there are no expressions then
;; true is returned.

;; * `or': The expressions are evaluated from left to right.  If
;; any expression evaluates to a true value, that value is
;; returned; any remaining expressions are not evaluated.  If
;; all expressions evaluate to false, or if there are no
;; expressions, then false is returned.


;; Install `and' and `or' as new special forms for the evaluator by
;; defining appropriate syntax procedures and evaluation procedures
;; `eval-and' and `eval-or'.  Alternatively, show how to implement
;; `and' and `or' as derived expressions.

(define (first-cond exp) (car exp))
(define (rest-cond exp) (cdr exp))
(define (empty? exp) (null? exp)
(define (conditions exp) (cdr exp))

(define (and? exp)
  (tagged-list? exp 'and))

(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-and exp env)
  (if (empty? (first-cond exp))
      'true
      (if (empty? (rest-cond exp))
	  (eval (first-cond exp) env) ;; last expression
	  (if (true? (eval (first-cond exp) env))
	      (eval-and (rest-cond exp) env)
	      'false))))

(define (eval-or exp env)
  (if (empty? (first-cond exp))
      'false
      (let ((val (eval (first-cond exp) env)))
	(if val
	    val
	    (eval-or (rest-cond exp) env)))))

;; Derived and and or
(define (or->if exp)
  (if (empty? (first-cond exp))
      'false
      (make-if (first-cond exp)
	       (first-cond exp)
	       (or->if (rest-cond exp)))))

(define (and->if exp)
  (if (empty? (first-cond exp))
      'true
      (make-if (empty? (rest-cond exp))
	       (first-cond exp) ;; last expression
	       (make-if (first-cond exp)
			(and->if (rest-cond exp))
			'false))))

(define (eval exp env)
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
	((cond? exp) (eval (cond->if exp) env))
	((and? exp) (eval-if (conditions exp) env))
	((or? exp) (eval-or (conditions exp) env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))



