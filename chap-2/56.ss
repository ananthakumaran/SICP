;; Exercise 2.56. Show how to extend the basic differentiator to handle
;; more kinds of expressions. For instance, implement the differentiation
;; rule by adding a new clause to the deriv program and defining
;; appropriate procedures exponentiation?, base, exponent, and
;; make-exponentiation. (You may use the symbol ** to denote
;; exponentiation.) Build in the rules that anything raised to the power
;; 0 is 1 and anything raised to the power 1 is the thing itself.

(define (variable? x)
  (symbol? x))

(define (same-variable? a b)
  (and (variable? a) (variable? b) (eq? a b)))

(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))

(define (addend e)
  (cadr e))

(define (augend e)
  (caddr e))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a b)
  (cond 
   ((and (=number? a 0) b))
   ((and (=number? b 0) a))
   ((and (number? a) (number? b)) (+ a b))
   (else
    (list '+ a b))))

(define (product? e)
  (and (pair? e) (eq? (car e) '*)))

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



(define (exponentiantion? e)
  (and (pair? e) (eq? (car e) '**)))

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

(define (deriv exp var)
  (cond
   ((number? exp) 0)
   ((variable? exp) 
    (if (same-variable? exp var)
	1
	0))
   ((sum? exp)
    (make-sum (deriv (addend exp) var)
	      (deriv (augend exp) var)))
   ((product? exp)
    (let ((u (multiplier exp))
	  (v (multiplicand exp)))
      (make-sum
       (make-product u (deriv v var))
       (make-product (deriv u var) v))))
   ((exponentiantion? exp)
    (let ((u (base exp))
	  (n (exponent exp)))
      (make-product
       (make-product n (make-exponentiation u (- n 1)))
       (deriv u var))))
   (else
    (error "unknown expression type -- DERIV" exp))))
    

;; test   
(deriv '(** x 2) 'x)
(deriv '(** x 1) 'x)
(deriv '(** x 3) 'x)
(deriv '(** x 0) 'x)
