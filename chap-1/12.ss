;;A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n -3) 
;;if n> 3. Write a procedure that computes f by means of a recursive process. Write a procedure that
;;computes f by means of an iterative process.

(define (f n)
  (cond
   ((< n 3) 0)
   (else
    (iter-fun n 0 1 2))))

(define (recur-fun n)
  (cond
   ((< n 3) n)
   (else
    (+ (recur-fun (- n 1)) (* 2 (recur-fun (- n 2))) 
       (* 3 (recur-fun (- n 3)))))))

(define (iter-fun n f1 f2 f3)
  (cond
   ((< n 3) f3)
   (else (iter-fun (- n 1) f2 f3 (+ f3 (* 2 f2) (* 3 f1))))))
  
