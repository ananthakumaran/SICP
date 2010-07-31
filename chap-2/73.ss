;; Exercise 2.73. Section 2.3.2 described a program that performs
;; symbolic differentiation:

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	<more rules can be added here>
	(else (error "unknown expression type -- DERIV" exp))))

;; We can regard this program as performing a dispatch on the type of the
;; expression to be differentiated. In this situation the ``type tag'' of
;; the datum is the algebraic operator symbol (such as +) and the
;; operation being performed is deriv. We can transform this program into
;; data-directed style by rewriting the basic derivative procedure as

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp)) (operands exp)
	       var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; a. Explain what was done above.

;; The exp and the var are checked and if the exp is a combination(not a 
;; number and not a var),the procedure for the operator is taken from the table
;; and applied upon the operands. 

;; Why can't we assimilate the predicates number? and same- variable?
;; into the data-directed dispatch?

;; Numbers and variable are checking whether the expression is a atom or 
;; combination.

;; b. Write the procedures for derivatives of sums and products, and the
;; auxiliary code required to install them in the table used by the
;; program above.

(define (install-add-mul-deriv-package)
  ;; internal procedures
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (addend e)
    (cadr e))
  (define (augend e)
    (caddr e))
  (define (make-sum a b)
    (cond 
     ((and (=number? a 0) b))
     ((and (=number? b 0) a))
     ((and (number? a) (number? b)) (+ a b))
     (else
      (list '+ a b))))
  (define (multiplier e)
    (cadr e))
  (define (multiplicand e)
    (caddr e))
  (define (make-product m1 m2)
    (cond
     ((or (=number? m1 0) (=number? m2 0)) 0)
     ((=number? m1 1) m2)
     ((=number? m2 1) m1)
     ((and (number? m1) (number? m2)) (* m1 m2))
     (else
      (list '* m1 m2))))
  ;; interface
  (put 'deriv '+ 
       (lambda (exp var) (make-sum (deriv (addend exp))
			      (deriv (augend exp)))))
    (let ((u (multiplier exp))
	  (v (multiplicand exp)))
      (make-sum
       (make-product u (deriv v var))
       (make-product (deriv u var) v))))

  (put 'deriv '*
       (lambda (exp var)
	 (let ((u (multiplier exp))
	       (v (multiplicand exp)))
	   (make-sum
	    (make-product u (deriv v var))
	    (make-product (deriv u var) v)))))
  'done)

;; c. Choose any additional differentiation rule that you like, such as
;; the one for exponents (exercise 2.56), and install it in this
;; data-directed system.

(define (install-exp-deriv-package)
  ;; internal
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (base e)
    (cadr e))
  (define (exponent e)
    (caddr e))
  (define (make-exponentiation base exponent)
    (cond
     ((=number? exponent 0) 1)
     ((=number? exponent 1) base)
     (else
      (list '** base exponent))))
  (define (make-product m1 m2)
    (cond
     ((or (=number? m1 0) (=number? m2 0)) 0)
     ((=number? m1 1) m2)
     ((=number? m2 1) m1)
     ((and (number? m1) (number? m2)) (* m1 m2))
     (else
      (list '* m1 m2))))
  ;; interface
  (put 'deriv '** 
       (lambda (exp var)
	 (let ((u (base exp))
	       (n (exponent exp)))
	   (make-product
	    (make-product n (make-exponentiation u (- n 1)))
	    (deriv u var)))))
  'done)

;; d. In this simple algebraic manipulator the type of an expression is
;; the algebraic operator that binds it together. Suppose, however, we
;; indexed the procedures in the opposite way, so that the dispatch line
;; in deriv looked like

((get (operator exp) 'deriv) (operands exp) var)

;; What corresponding changes to the derivative system are required? 
;; We can handle this in two ways
;; 1. while inserting in the table pass the operator as first arg and 'deriv
;;    as second arg
;; 2. change the get procedure or change the put procedure
