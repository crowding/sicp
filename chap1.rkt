#lang planet/neil sicp

;;Exercise 1.1

10                              ;10
(+ 5 3 4)                       ;11
(- 9 1)                         ;8
(/ 6 2)                         ;3
(+ (* 2 4) (- 4 6))             ;-16
(define a 3)                    ;;3
(define b (+ a 1))              ;;4
(+ a b (* a b))                 ;19
(= a b)                         ;#f
(if (and (> b a) (< b (* a b)))
    b a)                        ;4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))                ;16
(+ 2 (if (> b a) b a))          ;6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))                     ;16

;;Exercise 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
 (* 3 (- 6 2) (- 2 7)))

;;Exercise 1.3
(define (sum-two-larger x y z)
  (cond (((and (< x z) (< x y)) (+ (* y y) (* z z)))
         ((< x y) (+ (* x x) (* y y)))
         (else (+ (* x x) (* y y))))))

;;Exercise 1.4
;if b is greater than 0, a+b, else a-b.

;;Exercise 1.5
;;In applicative-order evaluation, (p) expands to (p) before entering 'test',
;;thus entering an infinite loop. In normal order evaluation, 
;;the 'if' special form expands first, selects the '0' and skips
;;expanding the argument.