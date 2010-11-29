;; *Exercise 4.56:* Formulate compound queries that retrieve the
;; following information:

;; a. the names of all people who are supervised by Ben Bitdiddle,
;; together with their addresses;

(and (supervisor ?x (Bitdiddle Ben))
     (address ?x ?address))

;; b. all people whose salary is less than Ben Bitdiddle's,
;; together with their salary and Ben Bitdiddle's salary;

(and (salary (Bitdiddle Ben) ?ben-salary)
     (salary ?name ?salary)
     (lisp-value < ?salary ?ben-salary))

;; c. all people who are supervised by someone who is not in the
;; computer division, together with the supervisor's name and
;; job.

(and (supervisor ?x ?y)
     (not (job ?y (computer . ?z))))
