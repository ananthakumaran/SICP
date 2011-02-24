;; *Exercise 5.3:* Design a machine to compute square roots using
;; Newton's method, as described in section *Note 1-1-7:::

;; (define (sqrt x)
;;   (define (good-enough? guess)
;;     (< (abs (- (square guess) x)) 0.001))
;;   (define (improve guess)
;;     (average guess (/ x guess)))
;;   (define (sqrt-iter guess)
;;     (if (good-enough? guess)
;; 	guess
;; 	(sqrt-iter (improve guess))))
;;   (sqrt-iter 1.0))

;; Begin by assuming that `good-enough?' and `improve' operations are
;; available as primitives.  Then show how to expand these in terms
;; of arithmetic operations.  Describe each version of the `sqrt'
;; machine design by drawing a data-path diagram and writing a
;; controller definition in the register-machine language.

(controller
 (assign a (const 1.0))
 sqrt-iter
 (test (op good-enough?) (reg g))
 (branch (label sqrt-done))
 (assign g (op improve) (reg g))
 (goto (label sqrt-iter))
 sqrt-done)

;; expand good-enough?
(controller
 (assign a (const 1.0))
 sqrt-iter
 (assign t (op *) (reg g) (reg g))
 (assign t (op -) (reg t) (reg x))
 (test (op <) (reg t) (const 0))
 (assign t (op *) (reg t) (const -1))
 (test (op <) (reg t) (const 0.001))
 (branch (label sqrt-done))
 (assign g (op improve) (reg g))
 (goto (label sqrt-iter))
 sqrt-done)

;; expand improve
(controller
 (assign a (const 1.0))
 sqrt-iter
 (assign t (op *) (reg g) (reg g))
 (assign t (op -) (reg t) (reg x))
 (test (op <) (reg t) (const 0))
 (assign t (op *) (reg t) (const -1))
 (test (op <) (reg t) (const 0.001))
 (branch (label sqrt-done))
 (assign t (op /) (reg x) (reg g))
 (assign t (op +) (reg t) (reg g))
 (assign g (op /) (reg t) (const 2))
 (goto (label sqrt-iter))
 sqrt-done)
