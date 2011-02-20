;; *Exercise 4.68:* Define rules to implement the `reverse' operation
;; of *Note Exercise 2-18::, which returns a list containing the same
;; elements as a given list in reverse order.  (Hint: Use
;; `append-to-form'.)  Can your rules answer both `(reverse (1 2 3)
;; ?x)' and `(reverse ?x (1 2 3))' ?


(assert! (rule (append-to-form () ?y ?y)))

(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
	       (append-to-form ?v ?y ?z)))


;; ans
(assert! (rule (reverse (?x) (?x))))

(assert! (rule (reverse (?u . ?v) ?z)
	       (and (reverse ?v ?y)
		    (append-to-form ?y (?u) ?z))))

(reverse (1 2 3) ?x)
;;(reverse ?x (1 2 3))
(reverse ?x (1 2 3))
;;infinite loop

;; changing the order of and condition will solve the (reverse ?x (1 2 3))
;; but will not work for (reverse (1 2 3) ?x)
