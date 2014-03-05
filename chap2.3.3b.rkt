#lang planet neil/sicp

;;; Exercise 2.63

(define tree-a '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree-b '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree-c '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

;; A. Both procedures produce the same results, '(1 3 5 7 9 11)
;; for each of the trees.

;; B. procedure 1 involves many calls to append -- on average,
;; O(log N) calls to append for each element appended.
;; So the complexity is O(n log n) for a balanced tree,
;; but O(n^2) in the worst case (all left tree.)
;; Procedure 2 operates in O(n) time.

;;; Exercise 2.64

;; The procedure measures the length of the list once, then 
;; divides that number (- 1 for the entry) in half. It then
;; converts the head of the list into a subtree consuming the 
;; specified number of items from the head of the list.
;; Then it is left with the entry followed by the
;; tail half of the list, which again makes a subtree.
;; Constructing subtrees is done by the same method, 
;; by recursion. 

;; B.       5 
;          / \
;         /   \
;        /     \
;       1       9
;        \     / \
;         3   7  11

;; B. The procedure should operate in O(n) time, as there is one call of
;; partial-tree per element, and all operations used are O(1)
;; (i.e. no appends).

;;; Exercise 2.66

(define key car)
(define value cdr)
(define test-tree '((5 . a) ((1 . b) () ((3 . c) () ()))
                            ((9 . d) ((7 . e) () ())
                                     ((11 . f) () ()))))

(define (lookup given-key record-tree)
  (cond ((null? record-tree) false)
        ((= given-key (key (entry record-tree)))
         (entry record-tree))
        ((< given-key (key (entry record-tree)))
         (lookup given-key (left-branch record-tree)))
        ((> given-key (key (entry record-tree)))
         (lookup given-key (right-branch record-tree)))))

;;; pasted from book

(define (list->tree elements)
         (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
             (left-result
              (partial-tree elts left-size))
             (left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (right-size (- n (+ left-size 1)))
             (this-entry (car non-left-elts))
             (right-result
              (partial-tree
               (cdr non-left-elts)
               right-size))
             (right-tree (car right-result))
             (remaining-elts
              (cdr right-result)))
        (cons (make-tree this-entry
                         left-tree
                         right-tree)
              remaining-elts))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))