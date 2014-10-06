#lang planet neil/sicp

;;; Exercise 2.17 
(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))

;;; Exercise 2.18
;(define (reverse list)
;  (define (iter list accum)
;    (if (null? list)
;        accum
;        (iter (cdr list) (cons (car list) accum))))
;  (iter list nil))

;; Exercise 2.19

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

;; Exercise 2.20

(define (same-parity p . rest)
  (define (iter list accum)
    (if (null? list)
        accum
        (if (= (remainder (+ p (car list)) 2) 0)
            (iter (cdr list) (cons (car list) accum))
            (iter (cdr list) accum))))
  (reverse (iter rest (list p))))

;; Exercise 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))
  
;(define (square-list items)
;  (map square items))

;;; Exercise 2.22

; The first attempt reverses because building a list iteratively 
; with cons only works back-to-front. The second version doesn't 
; produce a list but a structure like ((((1) 4) 9) 16) -- which 
; again puts the last element near the "top" of the data structure.

;;; Exercise 2.23

(define (for-each f l)
  (if (null? l) nil
      (begin
       (f (car l))
       (for-each (cdr l)))))

;;; Exercise 2.24
; (1 (2 (3 4)))

; [1|.] > [.|nil]
;          v
;         [2|.] > [.|nil]
;                  v
;                 [3|.] > [4|nil]

;  *
; / \
;1   *
;   / \
;  2   *
;     / \
;    3   4

;;; exercise 2.25

;> (car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
;7
;> (car (car '((7))))
;7
;> (cadr (cadr (cadr (cadr (cadr (cadr '(1 (2 (3 (4 (5 (6 7))))))))))))
;7

;;; Exercuse 2.26

;> (append x y)
;(1 2 3 4 5 6)
;> (cons x y)
;((1 2 3) 4 5 6)
;> (list x y)
;((1 2 3) (4 5 6))

;;; Exercise 2.27

(define (deep-reverse l)
  (define (iter list accum)
    (if (null? list) accum
        (iter (cdr list) (cons (deep-reverse (car list)) accum))))
  (if (pair? l) 
      (iter l nil)
      l))

;;; Exercise 2.28

(define (fringe l)
  (define (iter accum list)
    (if (null? list) accum
        (let ((head (car list)) 
              (tail (cdr list)))
          (if (pair? head)
              (if (null? (cdr head))
                  (iter accum (cons (car head) tail))
                  (iter accum (cons (car head) (cons (cdr head) tail))))
              (iter (cons head accum) tail)))))
  (reverse (iter nil l)))

;;; Exercise 2.29

;; a
(define left-branch car)
;(define (right-branch x) (list-ref x 1))
(define branch-length car)
;(define (branch-structure x) (list-ref x 1))

;; b
(define (total-weight mobile)
  (if (pair? mobile)
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))
      mobile))

;; c
(define (branch-torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

(define (torque mobile)
  (if (pair? mobile)
      (- (branch-torque (left-branch mobile))
         (branch-torque (right-branch mobile)))
      0))

(define (balanced? mobile)
  (if (pair? mobile)
      (and (= 0 (torque mobile))
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))
      #t))

;; d
(define (make-mobile left right) (cons left right))
(define (make-branch length structure)
  (cons length structure))

(define right-branch cdr)
(define branch-structure cdr)

;> (define mob (make-mobile 
;               (make-branch 10 100)
;               (make-branch 5 (make-mobile
;                               (make-branch 5 150)
;                               (make-branch 15 50)))))
;> (balanced? mob)
;#t

;;; Exercise 2.30

;directly
;(define (square-tree tree)
;  (cond
;    ((pair? tree)
;     (cons (square-tree (car tree)) (square-tree (cdr tree))))
;    ((null? tree) nil)
;    ((square tree))))

;using map
;(define (square-tree tree)
;  (map (lambda (sub-tree)
;         (if (pair? sub-tree)
;             (square-tree sub-tree)
;             (square sub-tree)))
;       tree))         


;;; Exercise 2.31

(define (tree-map f tree)
  (cond
    ((pair? tree)
     (cons (tree-map f (car tree)) (tree-map f (cdr tree))))
    ((null? tree) nil)
    ((f tree))))  

(define (square-tree tree) (tree-map square tree))

;;; Exercise 2.32

;For some arbitrary item X in S (say, the first), each subset U
;of S either contains X or not. Each subset U which contains X
;corresponds to a subset U-{X} that does not. Then all subsets
;of S is equal to all subsets of S-{X} and all subsets of S-{X}
;with X.
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x))
                          rest)))))

;;; Exercise 2.33
;(define (map p sequence)
;  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
;(define (append seq1 seq2) (accumulate cons seq2 seq1))
;(define (length sequence) (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;;; Exercise 2.34

(define (horner-eval x coefficient-sequence) 
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;;; exercise 2.35
(define (count-leaves t)
  (accumulate 
   + 0
   (map (lambda (x) 
          (if (pair? x) 
              (count-leaves x)
              1))
        t)))

;;; exercise 2.36
(define (accumulate-n op init seqs)
         (if (null? (car seqs))
             nil
             (cons (accumulate op init (map car seqs))
                   (accumulate-n op init (map cdr seqs)))))

;;; exercise 2.37
(define (vector-*-vector m n) (accumulate + 0 (map * m n)))
(define (matrix-*-vector m v) (map (lambda (r) (vector-*-vector r v)) m))
(define (transpose mat) (accumulate-n cons nil mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;;; exercise 2.38
;(fold-right / 1 (list 1 2 3)) => 3/2
;(fold-left / 1 (list 1 2 3)) => 1/6
;(fold-right list nil (list 1 2 3)) => (1 (2 (3 ())))
;(fold-left list nil (list 1 2 3)) => (((() 1) 2) 3)

;Fold-left and fold-right would be identical if the 
;operation is associative, i.e.
;(op (op a b) c) == (op a (op b c))

;;; Exercise 2.39

;(define (reverse sequence)
;  (fold-right (lambda (x y) (append y (list x))) nil sequence))

;(define (. sequence)
;  (fold-left (lambda (x y) (cons y x)) nil sequence))

;;; Exercise 2.40 

(define (unique-pairs n)
  (flatmap (lambda (i) 
             (map (lambda (j) (list j i)) (enumerate-interval 1 (dec i))))
           (enumerate-interval 2 n)))

(define (prime-sum-pairs n)
       (map make-pair-sum
            (filter prime-sum?
                    (unique-pairs n))))

;;; Exercise 2.41

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (pair) 
                        (append pair (list i))) 
                      (unique-pairs (dec i))))
           (enumerate-interval 3 n)))

;;; Exercise 2.42

;Since the algorithm fills in from left to right,
;the representation is just a list of row numbers
;running from rightmost queen to left; the last 
;element in the list is the first row.
;e.g. the solution in Figure 2.8 is (6 4 1 5 8 2 7 3) 
(define empty-board nil)

(define (adjoin-position row col board)
  (if (= col (+ 1 (length board)))
      (cons row board)
      (error "must adjoin queens in column order")))

(define (safe? k board)
  ; check if rightmost queen is safe
  (if (not (= (length board) k)) (error "can only check newest queen"))
  (define (iter board up across down)
    ; check each queen safe from attack by rightmost queen
    (if (null? board) #t
        (let ((queen (car board)))
          (cond ((or (= queen up)
                     (= queen down)
                     (= queen across)) #f)
                (else (iter (cdr board)
                            (inc up)
                            across
                            (dec down)))))))
  (if (null? board) #t
      (let ((queen (car board)))
        (iter (cdr board) (inc queen) queen (dec queen)))))

;;; 2.43

;; Louis's code generates ALL positions at once before filtering
;; -- so a n assignment that is ruled out on the third queen 
;; generates a bunch of irrelevant child assignments. 


;; pasted from book

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row
                                    k
                                    rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime-sum? pair)
      (prime? (+ (car pair) (cadr pair))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (make-pair-sum pair)
      (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (flatmap proc seq)
      (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define fold-right accumulate)

(define (square x) (* x x))
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
     (cond ((= amount 0) 1)
           ((or (< amount 0) (no-more? coin-values)) 0)
           (else
            (+ (cc amount
                   (except-first-denomination
                    coin-values))
               (cc (- amount
                      (first-denomination
                       coin-values))
                   coin-values)))))

