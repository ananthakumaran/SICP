;; *Exercise 4.62:* Define rules to implement the `last-pair'
;; operation of *Note Exercise 2-17::, which returns a list
;; containing the last element of a nonempty list.  Check your rules
;; on queries such as `(last-pair (3) ?x)', `(last-pair (1 2 3) ?x)',
;; and `(last-pair (2 ?x) (3))'.  Do your rules work correctly on
;; queries such as `(last-pair ?x (3))' ?

(assert! (rule (last-pair (?u . ?r) ?l)
	       (last-pair ?r ?l)))

(assert! (rule (last-pair (?l) ?l)))


(last-pair (3) ?x)
;;(last-pair (3) 3)
(last-pair (1 2 3) ?x)
;;(last-pair (1 2 3) 3)
(last-pair (2 ?x) (3))
;;(last-pair (2 (3)) (3))
(last-pair ?x (3))
;; (last-pair ((3)) (3))
;; (last-pair (?u-20 (3)) (3))
;; (last-pair (?u-20 ?u-22 (3)) (3))
;; .................................
