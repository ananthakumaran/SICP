;; Exercise 2.31. Abstract your answer to exercise 2.30 to produce a
;; procedure tree-map with the property that square-tree could be defined
;; as

;; (define (square-tree tree) (tree-map square tree))

;; using map
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (tree-map proc tree)
  (define (iter sub-tree)
    (if (pair? sub-tree)
	(tree-map proc sub-tree)
	(proc sub-tree)))
  (map iter tree))


(define (tree-map proc tree)
  (cond
   ((null? tree) '())
   ((not (pair? tree)) (proc tree))
   (else
    (cons (tree-map proc (car tree))
	  (tree-map proc (cdr tree))))))
    

(define (square-tree tree) (tree-map square tree))

(square-tree
(list 1
      (list 2 (list 3 4) 5)
      (list 6 7)))

