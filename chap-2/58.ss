;; Exercise 2.58. Suppose we want to modify the differentiation program
;; so that it works with ordinary mathematical notation, in which + and *
;; are infix rather than prefix operators. Since the differentiation
;; program is defined in terms of abstract data, we can modify it to work
;; with different representations of expressions solely by changing the
;; predicates, selectors, and constructors that define the representation
;; of the algebraic expressions on which the differentiator is to
;; operate.

;; a. Show how to do this in order to differentiate algebraic expressions
;; presented in infix form, such as (x + (3 * (x + (y + 2)))). To
;; simplify the task, assume that + and * always take two arguments and
;; that expressions are fully parenthesized.

;; b. The problem becomes substantially harder if we allow standard
;; algebraic notation, such as (x + 3 * (x + y + 2)), which drops
;; unnecessary parentheses and assumes that multiplication is done before
;; addition. Can you design appropriate predicates, selectors, and
;; constructors for this notation such that our derivative program still
;; works?

;; a

(define (variable? x)
  (symbol? x))

(define (same-variable? a b)
  (and (variable? a) (variable? b) (eq? a b)))

(define (sum? e)
  (and (pair? e) (eq? (cadr e) '+)))

(define (addend e)
  (car e))

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
    (list a '+ b))))

(define (product? e)
  (and (pair? e) (eq? (cadr e) '*)))

(define (multiplier e)
  (car e))

(define (multiplicand e)
  (caddr e))

(define (make-product m1 m2)
  (cond
   ((or (=number? m1 0) (=number? m2 0)) 0)
   ((=number? m1 1) m2)
   ((=number? m2 1) m1)
   ((and (number? m1) (number? m2)) (* m1 m2))
   (else
    (list m1 '* m2))))

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
(deriv '(x * (y * ((x + 3) * (4 * x)))) 'x)
(deriv '(x + (3 * (x + (y + 2)))) 'x)
(deriv '(x * x) 'x)




(define (infix-to-postfix exp stack result)
  (if (null? exp)
      (append (reverse result) stack)
      (let ((i (car exp)))
	(cond
	 ((eq? i '+) (infix-to-postfix (cdr exp) (cons i stack) result))
	 ((eq? i '*) 
	  (if (not (null? stack))
	      (infix-to-postfix exp (cdr stack) (cons (car stack) result))
	      (infix-to-postfix (cdr exp) (cons '* stack) result)))
	 ((variable? i) (infix-to-postfix (cdr exp) stack (cons i result)))
	 (else
	  (error " error " i))))))

(infix-to-postfix '(a + b * c) '() '())


(define (reverse lat)
  (define (loop l result)
    (if (null? l)
	result
	(loop (cdr l) (cons (car l) result))))
  (loop lat '()))
  

(define (add-parens exp)
  
  

;; b TODO