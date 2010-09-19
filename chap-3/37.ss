;; *Exercise 3.37:* The `celsius-fahrenheit-converter' procedure is
;; cumbersome when compared with a more expression-oriented style of
;; definition, such as

;; (define (celsius-fahrenheit-converter x)
;;   (c+ (c* (c/ (cv 9) (cv 5))
;; 	  x)
;;       (cv 32)))

;; (define C (make-connector))
;; (define F (celsius-fahrenheit-converter C))

;; Here `c+', `c*', etc. are the "constraint" versions of the
;; arithmetic operations.  For example, `c+' takes two connectors as
;; arguments and returns a connector that is related to these by an
;; adder constraint:

;; (define (c+ x y)
;;   (let ((z (make-connector)))
;;     (adder x y z)
;;     z))

;; Define analogous procedures `c-', `c*', `c/', and `cv' (constant
;; value) that enable us to define compound constraints as in the
;; converter example above.(3)


(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (cv c)
  (let ((z (make-connector)))
    (constant c z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c- x y)
  (c+ x (c* (cv -1) y))) ;; lazy

(define (divider m1 m2 c)
  (define (process-new-value)
    (cond ((and (has-value? m1) (has-value? m2))
	   (set-value! c
		       (/ (get-value m1) (get-value m2))
		       me))
	  ((and (has-value? c) (has-value? m1))
	   (set-value! m2
		       (/ (get-value m1) (get-value c))
		       me))
	  ((and (has-value? c) (has-value? m2))
	   (set-value! m1
		       (* (get-value c) (get-value m2))
		       me))))
  (define (process-forget-value)
    (forget-value! c me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else
	   (error "Unknown request -- DIVIDER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect c me)
  me)

(define (c/ x y)
  (let ((z (make-connector)))
    (divider x y z)
    z))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
	  x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(probe "Celcius" C)
(probe "Fahrenheit" F)
(set-value! C 25 'user)
(forget-value! C 'user)
(set-value! F 77 'user)
