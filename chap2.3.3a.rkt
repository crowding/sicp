#lang planet neil/sicp

;;; exercise 2.59

(define (union-uset set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else (union-uset (adjoin-set (car set2) set1) (cdr set2)))))

;;; Exercise 2.60

(define (element-of-multiset? x set) (element-of-uset? x set))
(define adjoin-multiset car)
(define union-multiset append)
(define (intersection-multiset set1 set2)
  (intersection-uset set1 set2))

;Would be more efficient of no intersections are computed,
;and likelihood of assembling duplicate elements is low.

;;; Exercise 2.61

(define (adjoin-set x set) 
  (cond ((null? set) (list x))
        ((= (car set) x) set)
        ((> (car set) x) (cons x set))
        ((< (car set) x) (cons (car set) 
                               (adjoin-set x (cdr set))))))

;;; exercise 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))
         (cons (car set2) (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        ((> (car set1) (car set2))
         (cons (car set2) (union-set set1 (cdr set2))))))

;;; pasted from book

(define (element-of-uset? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-uset? x (cdr set)))))

(define (adjoin-uset x set)
  (if (element-of-uset? x set)
      set
      (cons x set)))

(define (intersection-uset set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-uset? (car set1) set2)
         (cons (car set1)
               (intersection-uset (cdr set1) set2)))
        (else (intersection-uset (cdr set1) set2))))

;;;ordered
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))
