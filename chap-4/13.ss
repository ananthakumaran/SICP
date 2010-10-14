;; *Exercise 4.13:* Scheme allows us to create new bindings for
;; variables by means of `define', but provides no way to get rid of
;; bindings.  Implement for the evaluator a special form
;; `make-unbound!' that removes the binding of a given symbol from the
;; environment in which the `make-unbound!' expression is evaluated.
;; This problem is not completely specified.  For example, should we
;; remove only the binding in the first frame of the environment?
;; Complete the specification and justify any choices you make.


(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals pre-var pre-val head)
      (cond ((null? vars)
	     (error " Var is not defined " var))
	    ((eq? var (car vars))
	     (begin
	       (if head
		   (set-car! pre-var (cdr vars))
		   (set-cdr! pre-var (cdr vars)))
	       (set-cdr! pre-val (cdr vals))))
	    (else (scan (cdr vars) (cdr vals) vars  vals false))))
    (scan (frame-variables frame)
	  (frame-values frame)
	  frame
	  frame
	  true)))

;; We should remove only the binding in the first frame
;; of ther environment. Otherwise we will mess up with the
;; variables declared by others in outer namespace
