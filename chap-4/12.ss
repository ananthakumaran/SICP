;; *Exercise 4.12:* The procedures `set-variable-value!',
;; `define-variable!', and `lookup-variable-value' can be expressed
;; in terms of more abstract procedures for traversing the
;; environment structure.  Define abstractions that capture the
;; common patterns and redefine the three procedures in terms of these
;; abstractions.

(define (scan-frame frame success failure)
    (define (scan vars vals)
      (cond ((null? vars)
	     (failure))
	    ((eq? var (car vars))
	     (success vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame)))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan-frame frame
		      (lambda (vals) (car vals))
		      (lambda () (env-loop (enclosing-environment env)))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan-frame frame
		      (lambda (vals) (set-car! vals val))
		      (lambda () (env-loop (enclosing-environment env)))))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (scan-frame frame
		(lambda (vals) (set-car! vals val))
		(lambda () (add-binding-to-frame! var val frame)))))
