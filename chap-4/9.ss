;; *Exercise 4.9:* Many languages support a variety of iteration
;; constructs, such as `do', `for', `while', and `until'.  In Scheme,
;; iterative processes can be expressed in terms of ordinary
;; procedure calls, so special iteration constructs provide no
;; essential gain in computational power.  On the other hand, such
;; constructs are often convenient.  Design some iteration
;; constructs, give examples of their use, and show how to implement
;; them as derived expressions.


;; (for (init x) (check x) (inc x)
;;      (body))

((lambda ()
   (define (loop)
     (if (check x)
	 (begin
	   (body)
	   (inc x)
	   (loop))))
   (init x)
   (loop))

;; (while (condition x)
;;   (body))

((lambda ()
   (define (loop)
     (if (condition x)
	 (begin
	   (body)
	   (loop))))
   (loop))

;; (until (condition x)
;;        (body))
((lambda ()
   (define (loop)
     (if (not (condition x))
	 (begin
	   (body)
	   (loop))))
   (loop))
