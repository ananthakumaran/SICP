;; Exercise 2.35. Redefine count-leaves from section 2.2.2 as an
;; accumulation:

;; (define (count-leaves t)
;;   (accumulate <??> <??> (map <??> <??>)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))


(define (count-leaves t)
  (accumulate +
	      0 
	      (map (lambda (x)
		     (if (not (pair? x))
			 1
			 (count-leaves x))) t)))

(count-leaves (list 1 (list 2 (list 3 4))))
					