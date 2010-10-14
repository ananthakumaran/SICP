;; *Exercise 4.11:* Instead of representing a frame as a pair of
;; lists, we can represent a frame as a list of bindings, where each
;; binding is a name-value pair.  Rewrite the environment operations
;; to use this alternative representation.

(define (make-frame variables values)
  (map (lambda (x y) (cons x y)) variables values))

(define x (make-frame '(x y) '(1 2)))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (cons var val)))

(define (scan-frame frame var success failure)
    (define (scan params)
      (cond ((null? params)
	     (failure))
	    ((eq? var (caar params))
	     (success (car params)))
	    (else (scan (cdr params)))))
    (scan frame))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan-frame frame
		      var
		      (lambda (pair) (cdr pair))
		      (lambda () (env-loop (enclosing-environment env)))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan-frame frame
		      var
		      (lambda (pair) (set-cdr! pair val))
		      (lambda () (env-loop (enclosing-environment env)))))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (scan-frame frame
		var
		(lambda (pair) (set-cdr! pair val))
		(lambda () (add-binding-to-frame! var val frame)))))
