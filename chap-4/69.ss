;; *Exercise 4.69:* Beginning with the data base and the rules you
;; formulated in *Note Exercise 4-63::, devise a rule for adding
;; "greats" to a grandson relationship. This should enable the system
;; to deduce that Irad is the great-grandson of Adam, or that Jabal
;; and Jubal are the great-great-great-great-great-grandsons of Adam.
;; (Hint: Represent the fact about Irad, for example, as `((great
;; grandson) Adam Irad)'.  Write rules that determine if a list ends
;; in the word `grandson'.  Use this to express a rule that allows
;; one to derive the relationship `((great .  ?rel) ?x ?y)', where
;; `?rel' is a list ending in `grandson'.)  Check your rules on
;; queries such as `((great grandson) ?g ?ggs)' and `(?relationship
;; Adam Irad)'.

;; (son Adam Cain)
;; (son Cain Enoch)
;; (son Enoch Irad)
;; (son Irad Mehujael)
;; (son Mehujael Methushael)
;; (son Methushael Lamech)
;; (wife Lamech Ada)
;; (son Ada Jabal)
;; (son Ada Jubal)

(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert! (rule (grandson ?g ?s)
	       (and (son ?g ?f)
		    (sons ?f ?s))))

(assert! (rule (sons ?f ?s)
	       (or (son ?f ?s)
		   (and (wife ?f ?w)
			(son ?w ?s)))))

(assert! (rule (relate (grandson) ?x ?y)
	       (grandson ?x ?y)))

(assert! (rule (relate (great . ?r) ?y ?z)
	       (and (son ?y ?s)
		    (relate ?r ?s ?z))))


(relate (great grandson) ?g ?ggs)
;; (relate (great grandson) mehujael jubal)
;; (relate (great grandson) irad lamech)
;; (relate (great grandson) mehujael jabal)
;; (relate (great grandson) enoch methushael)
;; (relate (great grandson) cain mehujael)
;; (relate (great grandson) adam irad)

(relate ?relationship Adam Irad)
;; (relate (great grandson) adam irad)

(relate (great great great great great grandson) ?g ?ggs)
;; (relate (great great great great great grandson) adam jubal)
;; (relate (great great great great great grandson) adam jabal)
