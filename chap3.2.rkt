#lang planet neil/sicp



;;; Exercise 3.9
;;
;; The recursive procedure activates six environments:
;;
;; (factorial 6) (factorial 5) (factorial 4)
;;   n:6          n:5           n:4
;; (factorial 3) (factorial 2) (factorial 1)
;;  n:3           n:2           n:1
;;
;; The iterative procedure activates two environments:
;;
;; (factorial 6)
;;  n:6
;;  (fact-iter 1 1 6)
;;   product    : 1
;;   counter    : 1
;;   max-count  : 6
;;
;; The bottom environment is reused when fact-iter tail-calls itself.



;;; Exercise 3.10

;; On paper.

;; Fomr the perspective of the lambda returned by either version of
;; make-withdraw, the only variable they access (balance) is in an
;; environment that only it has a pointer to. An enclosing environment
;; with initial-balance is present there too, but is not relevant to
;; the action.



;;; Exercise 3.11

;; Local states for two accounts are kept separate in two
;; environments.  The "deposit" and "widhtdraw" procedures are
;; distinct for each account as well. Thus when the procedure referred
;; to by 'acc2' looks for 'deposit,' it finds a procedure that is also
;; attached to the same environment.crow



;;; Exercise 3.12

;; More paper.
