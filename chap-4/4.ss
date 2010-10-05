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

(define (eval-and exp env)
  (if (empty? (first-cond exp))
      'true
      (if (empty? (rest-cond exp))
	  (eval (first-cond exp) env) ;; last expression
	  (if (true? (eval (first-cond exp) env))
	      (eval-and (rest-cond exp) env)
	      'false))))

(define (and? exp)
  (tagged-list? exp 'and))

(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-or exp env)
  (if (empty? (first-cond exp))
      'false
      (let ((val (eval (first-cond exp) env)))
	(if val
	    val
	    (eval-or (rest-cond exp) env)))))
