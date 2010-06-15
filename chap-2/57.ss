;; Exercise 2.57. Extend the differentiation program to handle sums and
;; products of arbitrary numbers of (two or more) terms. Then the last
;; example above could be expressed as

;; (deriv '(* x y (+ x 3)) 'x)

;; Try to do this by changing only the representation for sums and
;; products, without changing the deriv procedure at all. For example,
;; the addend of a sum would bne the first term, and the augend would be
;; the sum of the rest of the terms.

(define (variable? x)
  (symbol? x))

(define (same-variable? a b)
  (and (variable? a) (variable? b) (eq? a b)))

(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))

(define (addend e)
  (cadr e))

(define (augend e)
  (if (> (length e) 3)
      (cons '+ (cddr e))
      (caddr e)))

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
  (if (> (length e) 3)
      (cons '* (cddr e))
      (caddr e)))

(define (make-product m1 m2)
  (cond
   ((or (=number? m1 0) (=number? m2 0)) 0)
   ((=number? m1 1) m2)
   ((=number? m2 1) m1)
   ((and (number? m1) (number? m2)) (* m1 m2))
   (else
    (list '* m1 m2))))

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
   (else
    (error "unknown expression type -- DERIV" exp))))


;; test
(deriv '(* x y (+ x 3) (* 4 x)) 'x)
(deriv '(* x (* y (* (+ x 3) (* 4 x)))) 'x)
