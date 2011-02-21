;; *Exercise 5.2:* Use the register-machine language to describe the
;; iterative factorial machine of *Note Exercise 5-1::.

(controller
 (assign b (const 1))
 (assign c (const 1))
 iter
 (test (op >) (reg c) (reg a))
 (branch (label factorial-done))
 (assign b (op *) (reg b) (reg c))
 (assign c (op +) (reg c) (const 1))
 (goto (label iter))
 factorial-done)

