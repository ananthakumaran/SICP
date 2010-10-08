;; *Exercise 4.7:* `Let*' is similar to `let', except that the
;; bindings of the `let' variables are performed sequentially from
;; left to right, and each binding is made in an environment in which
;; all of the preceding bindings are visible.  For example

;; (let* ((x 3)
;;        (y (+ x 2))
;;        (z (+ x y 5)))
;;   (* x z))


;; returns 39.  Explain how a `let*' expression can be rewritten as a
;; set of nested `let' expressions, and write a procedure
;; `let*->nested-lets' that performs this transformation.  If we have
;; already implemented `let' (*Note Exercise 4-6::) and we want to
;; extend the evaluator to handle `let*', is it sufficient to add a
;; clause to `eval' whose action is

;; (eval (let*->nested-lets exp) env)

;; or must we explicitly expand `let*' in terms of non-derived
;; expressions?

(define (make-let params body)
  (list 'let params body))

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-params exp) (cadr exp))
(define (let*-body exp) (caddr exp))

(define (let*->nested-lets exp)
  (define (loop params body)
    (if (null? params)
	body
	(make-let (list (car params)) (loop (cdr params) body))))
  (loop (let*-params exp) (let*-body exp)))

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
	((let? exp) (eval (let->combination exp) env))
	((let*? exp) (eval (let*->nested-lets exp) env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

;; test
(let*->nested-lets '(let* ((x 3)(y (+ x 2))(z (+ x y 5)))(* x z)))
;; (let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) (* x z))))

