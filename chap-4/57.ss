;; *Exercise 4.57:* Define a rule that says that person 1 can replace
;; person 2 if either person 1 does the same job as person 2 or
;; someone who does person 1's job can also do person 2's job, and if
;; person 1 and person 2 are not the same person. Using your rule,
;; give queries that find the following:

(rule (can-do ?job-1 ?job-2)
      (or (same ?job-1 ?job-2)
	  (can-do-job ?job-1 ?job-2)))

(rule (replace ?person-1 ?person-2)
      (and (job ?person-1-job ?person-1)
	   (job ?person-2-job ?person-2)
	   (can-do ?person-1-job ?person-2-job)

;; a. all people who can replace Cy D. Fect;

(replace ?x (Fect Cy D))

;; b. all people who can replace someone who is being paid more
;; than they are, together with the two salaries.

(and (salary ?x ?x-salary)
     (salary ?y ?y-salary)
     (replace ?x ?y)
     (lisp-value < ?x-salary ?y-salary))

