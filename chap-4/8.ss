;; *Exercise 4.8:* "Named `let'" is a variant of `let' that has the
;; form

;; (let <VAR> <BINDINGS> <BODY>)

;; The <BINDINGS> and <BODY> are just as in ordinary `let', except
;; that <VAR> is bound within <BODY> to a procedure whose body is
;; <BODY> and whose parameters are the variables in the <BINDINGS>.
;; Thus, one can repeatedly execute the <BODY> by invoking the
;; procedure named <VAR>.  For example, the iterative Fibonacci
;; procedure (section *Note 1-2-2::) can be rewritten using named
;; `let' as follows:

;; (define (fib n)
;;   (let fib-iter ((a 1)
;; 		 (b 0)
;; 		 (count n))
;;     (if (= count 0)
;; 	b
;; 	(fib-iter (+ a b) a (- count 1)))))

;; Modify `let->combination' of *Note Exercise 4-6:: to also support
;; named `let'.

(define (named-let? exp) (= (length exp) 4))
(define (named-let-params exp) (caddr exp))
(define (named-let-body exp) (cadddr exp))
(define (named-let-var exp) (cadr exp))

(define (let->combination exp)
  (if (named-let? exp)
      (let ((params (named-let-params exp))
	    (body (named-let-body exp))
	    (name (named-let-var exp)))
	(let ((var (map (lambda (x) (car x)) params))
	      (exp (map (lambda (x) (cadr x)) params)))
	  (list (list 'lambda
		      '()
		      (list 'define
			    (append (list name) var)
			    body)
		      (append (list name) exp)))))
      (let ((params (let-params exp))
	    (body (let-body exp)))
	(let ((var (map (lambda (x) (car x)) params))
	      (exp (map (lambda (x) (cadr x)) params)))
	  (append (list (make-lambda var body)) exp)))))


;; test
(let->combination '(let fib-iter ((a 1)
				  (b 0)
				  (count n))
		     (if (= count 0)
			 b
			 (fib-iter (+ a b) a (- count 1)))))

;; ((lambda () (define (fib-iter a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))) (fib-iter 1 0 n)))



