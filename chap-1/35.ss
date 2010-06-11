;; Exercise 1.35. Show that the golden ratio x (section 1.2.2) is a fixed point of the transformation
;;  x -> 1 + 1/x, and use this fact to compute by means of the fixed-point procedure.
;;
;; golden ratio
;; x^2 = x + 1
;; x   = (x + 1) / x
;; x   = 1 + 1/x
;;
;;

(define (fixed-point f first-guess)
  (define tolerance 0.001)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try-next guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try-next next))))
  (try-next first-guess))


;; golden ratio
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)


