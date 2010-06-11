;; Exercise 2.29. A binary mobile consists of two branches, a left branch
;; and a right branch. Each branch is a rod of a certain length, from
;; which hangs either a weight or another binary mobile. We can represent
;; a binary mobile using compound data by constructing it from two
;; branches (for example, using list):

;; (define (make-mobile left right)
;;     (list left right))

;; A branch is constructed from a length (which must be a number)
;; together with a structure, which may be either a number (representing
;; a simple weight) or another mobile:

;; (define (make-branch length structure)
;;     (list length structure))

;; a. Write the corresponding selectors left-branch and right-branch,
;; which return the branches of a mobile, and branch-length and
;; branch-structure, which return the components of a branch.

;; b. Using your selectors, define a procedure total-weight that returns
;; the total weight of a mobile.

;; c. A mobile is said to be balanced if the torque applied by its
;; top-left branch is equal to that applied by its top-right branch (that
;; is, if the length of the left rod multiplied by the weight hanging
;; from that rod is equal to the corresponding product for the right
;; side) and if each of the submobiles hanging off its branches is
;; balanced. Design a predicate that tests whether a binary mobile is
;; balanced.

;; d. Suppose we change the representation of mobiles so that the
;; constructors are

;; (define (make-mobile left right)
;;   (cons left right))
;; (define (make-branch length structure)
;;   (cons length structure))

;; How much do you need to change your programs to convert to the new
;; representation?



(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (left-branch structure)
  (car structure))
(define (right-branch structure)
  (cadr structure))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (leaf? branch)
  (number? (branch-structure branch)))
(define (weight branch)
  (branch-structure branch))


(define (total-weight mobile)
  (define (total branch)
    (if (leaf? branch)
	(weight branch)
	(total-weight (branch-structure branch))))
  (+ (total (left-branch mobile))
     (total (right-branch mobile))))

(define (balanced-tree? left right)
  (= (* (branch-length left)
	(if (leaf?  left)
	    (weight left)
	    (total-weight (branch-structure left))))
     (* (branch-length right)
	(if (leaf?  right)
	    (weight right)
	    (total-weight (branch-structure right))))))


(define (balanced? mobile)
  (let ((left (left-branch mobile))
	(right (right-branch mobile)))
    (cond
     ((and (leaf? left) (leaf? right))
      (balanced-tree? left right))
     (else
      (and (balanced-tree? left right)
	   (balanced? (branch-structure left))
	   (balanced? (branch-structure right)))))))


;;  test

(define c-right (make-branch 6 5))
(define c-left (make-branch 6 5))
(define c (make-mobile c-left c-right))
(define b-right (make-branch 6 5))
(define b-left (make-branch 6 5))
(define b (make-mobile b-left b-right))
(define a-left (make-branch 1 b))
(define a-right (make-branch 2 c))
(define a (make-mobile a-left a-right))

(left-branch a)
(right-branch a)
(left-branch b)
(right-branch b)
(left-branch c)
(right-branch c)
(branch-structure (right-branch a))
(branch-structure (left-branch a))
(total-weight a)
(total-weight c)
(total-weight b)
(balanced? c)
(balanced? b)
(branch-structure (left-branch a))
(balanced? a)
(balanced-tree? a-left a-right)




(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

(define (left-branch structure)
  (car structure))
(define (right-branch structure)
  (cdr structure))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))

(define (leaf? branch)
  (number? (branch-structure branch)))
(define (weight branch)
  (branch-structure branch))


(define (total-weight mobile)
  (define (total branch)
    (if (leaf? branch)
	(weight branch)
	(total-weight (branch-structure branch))))
  (+ (total (left-branch mobile))
     (total (right-branch mobile))))

(define (balanced-tree? left right)
  (= (* (branch-length left)
	(if (leaf?  left)
	    (weight left)
	    (total-weight (branch-structure left))))
     (* (branch-length right)
	(if (leaf?  right)
	    (weight right)
	    (total-weight (branch-structure right))))))

(define (balanced? mobile)
  (let ((left (left-branch mobile))
	(right (right-branch mobile)))
    (cond
     ((and (leaf? left) (leaf? right))
      (balanced-tree? left right))
     (else
      (and (balanced-tree? left right)
	   (balanced? (branch-structure left))
	   (balanced? (branch-structure right)))))))


;;  test

(define c-right (make-branch 6 5))
(define c-left (make-branch 6 5))
(define c (make-mobile c-left c-right))
(define b-right (make-branch 6 5))
(define b-left (make-branch 6 5))
(define b (make-mobile b-left b-right))
(define a-left (make-branch 1 b))
(define a-right (make-branch 2 c))
(define a (make-mobile a-left a-right))

(left-branch a)
(right-branch a)
(left-branch b)
(right-branch b)
(left-branch c)
(right-branch c)
(branch-structure (right-branch a))
(branch-structure (left-branch a))
(total-weight a)
(total-weight c)
(total-weight b)
(balanced? c)
(balanced? b)
(branch-structure (left-branch a))
(balanced? a)
(balanced-tree? a-left a-right)
