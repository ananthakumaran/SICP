;; Exercise 2.66. Implement the lookup procedure for the case where the
;; set of records is structured as a binary tree, ordered by the
;; numerical values of the keys.

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      false
      (let ((entry-key (key (entry (set-of-records)))))
	(cond ((= given-key entry-key)
	       (entry (set-of-records)))
	      ((< given-key entry-key)
	       (lookup given-key (left-branch set-of-records)))
	      ((> given-key entry-key)
	       (lookup given-key (right-branch set-of-records)))))))
