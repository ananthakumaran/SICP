;; *Exercise 4.58:* Define a rule that says that a person is a "big
;; shot" in a division if the person works in the division but does
;; not have a supervisor who works in the division.

(assert! (rule (bigshot ?x)
	       (and (job ?x (?division . ?rest-1))
		    (supervisor ?x ?y)
		    (not (job ?y (?division . ?rest-2))))))

(bigshot ?x)
