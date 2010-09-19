;; *Exercise 3.34:* Louis Reasoner wants to build a squarer, a
;; constraint device with two terminals such that the value of
;; connector `b' on the second terminal will always be the square of
;; the value `a' on the first terminal.  He proposes the following
;; simple device made from a multiplier:

;; (define (squarer a b)
;;   (multiplier a a b))

;; There is a serious flaw in this idea.  Explain.

(define (squarer a b)
  (multiplier a a b))

(define A (make-connector))
(define B (make-connector))

(probe "A" A)
(probe "SQUARE" B)

(squarer A B)

(forget-value! B 'user)
(set-value! A 10 'user)
;; Probe: SQUARE = 100
;; Probe: A = 10
;; ;Value: done

;; good so far

(forget-value! A 'user)
;; Probe: SQUARE = ?
;; Probe: A = ?
;; ;Value: done

(set-value! B 10 'user)
;; Probe: SQUARE = 10
;; ;Value: done

;; Now the multiplier check the value of m1 and m2
;; and if any one has a value then it will caluclate other
;; value based on it. But in this case both m1 and m2 is
;; A. So both m1 and m2 will be empty
