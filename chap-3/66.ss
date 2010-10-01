;; *Exercise 3.66:* Examine the stream `(pairs integers integers)'.
;; Can you make any general comments about the order in which the
;; pairs are placed into the stream? For example, about how many
;; pairs precede the pair (1,100)?  the pair (99,100)? the pair
;; (100,100)? (If you can make precise mathematical statements here,
;; all the better. But feel free to give more qualitative answers if
;; you find yourself getting bogged down.)


(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (from n)
  (cons-stream n
	       (from (+ n 1))))

(define integers (from 1))


(define ex (pairs integers integers))

(define (loop from to p)
  (if (> from to)
      'done
      (begin
	(p from)
	(loop (+ from 1) to p))))

(loop 0 100 (lambda (x)
	     (display (stream-ref ex x))
	     (display " ")))

;; (1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (1 5) (2 4) (1 6) (3 4) (1 7)
;; (2 5) (1 8) (4 4) (1 9) (2 6) (1 10) (3 5) (1 11) (2 7) (1 12) (4 5)
;; (1 13) (2 8) (1 14) (3 6) (1 15) (2 9) (1 16) (5 5) (1 17) (2 10)
;; (1 18) (3 7) (1 19) (2 11) (1 20) (4 6) (1 21) (2 12) (1 22) (3 8) (1 23)
;; (2 13) (1 24) (5 6) (1 25) (2 14) (1 26) (3 9) (1 27) (2 15) (1 28)
;; (4 7) (1 29) (2 16) (1 30) (3 10) (1 31) (2 17) (1 32) (6 6) (1 33)
;; (2 18) (1 34) (3 11) (1 35) (2 19) (1 36) (4 8) (1 37) (2 20) (1 38)
;; (3 12) (1 39) (2 21) (1 40) (5 7) (1 41) (2 22) (1 42) (3 13) (1 43)
;; (2 23) (1 44) (4 9) (1 45) (2 24) (1 46) (3 14) (1 47) (2 25) (1 48)
;; (6 7) (1 49) (2 26) (1 50) (3 15) (1 51) (2 27)


;; (1 1) (1 2) (1 3) (1 4) (1 5) (1 6)
;;       (2 2) (2 3) (2 4) (2 5) (2 6)
;;             (3 3) (3 4) (3 5) (3 6)
;;                   (4 4) (4 5) (4 6)
;;                         (5 5) (5 6)
;;                               (6 6)


;; select one from the first row and select one from the rest of the
;; rows and do it again and again. The selection in the rest of the row
;; also recurs.


