;; Exercise 2.76. As a large system with generic operations evolves, new
;; types of data objects or new operations may be needed. For each of the
;; three strategies -- generic operations with explicit dispatch, data-
;; directed style, and message-passing-style -- describe the changes that
;; must be made to a system in order to add new types or new
;; operations. Which organization would be most appropriate for a system
;; in which new types must often be added? 

;;; generic operation with explicit dispatch

1. Need to attach a type tag so that the data can be distinguished
from other types of implementation.
2. Need to change the generic procedures every time we add
a new type to the system.
3. The implementer should know about the other implementation so that
he can avoid name collisions.

;;; data directed style

1. Need to attach a type tag so that the data can be distinguished
from other types of implementation.

;;; message passing style

1. New type can be added without affecting the other parts of the system


;; Which would be most appropriate for a system in which new operations
;; must often be added?

Message passing style is the most appropriate for a system in which new 
operations must often be added. Instead of using procedures which check the 
data type and calling the approprite function, we can use intelligent data
type which can respond to a message.
