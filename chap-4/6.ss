;; *Exercise 4.6:* `Let' expressions are derived expressions, because

;; (let ((<VAR_1> <EXP_1>) ... (<VAR_N> <EXP_N>))
;;   <BODY>)

;; is equivalent to

;; ((lambda (<VAR_1> ... <VAR_N>)
;;    <BODY>)
;;  <EXP_1>
;;  ...
;;  <EXP_N>)

;; Implement a syntactic transformation `let->combination' that
;; reduces evaluating `let' expressions to evaluating combinations of
;; the type shown above, and add the appropriate clause to `eval' to
;; handle `let' expressions.

(define (let? exp) (tagged-list? exp 'let))
(define (let-params exp) (cadr exp))
(define (let-body exp) (caddr exp))

(define (let->combination exp)
  (let ((params (let-params exp))
	(body (let-body exp)))
    (let ((var (map (lambda (x) (car x)) params))
	  (exp (map (lambda (x) (cadr x)) params)))
      (append (list (make-let var body)) exp))))

(define (make-let var body)
  (list 'lambda var body))

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
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

;; test
(let->combination '(let ((a 4) (b (+ 4 5))) (+ a b)))
