;; Exercise 3.4. Modify the make-account procedure of exercise 3.3 by
;; adding another local state variable so that, if an account is accessed
;; more than seven consecutive times with an incorrect password, it
;; invokes the procedure call-the-cops.

(define (make-account balance password)
  (let ((wrong-tries 0))
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount)) 
      balance)
    (define (call-the-cops)
      (display "Calling the Cops"))
    (define (match-password secret-password)
      (if (eq? password secret-password)
	  (begin 
	    (set! wrong-tries 0)
	    #t)
	  (begin
	    (set! wrong-tries (1+ wrong-tries))
	    (if (> wrong-tries 7)
		(call-the-cops))
	    #f)))
    (define (dispatch secret-password m)
      (if (match-password secret-password)
	  (cond ((eq? m 'withdraw) withdraw)
		((eq? m 'deposit) deposit)
		(else (error "Unknown request -- MAKE-ACCOUNT"
			     m)))
	  "Incorrect Password"))
    dispatch))

;; test
(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
