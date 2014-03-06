#lang planet neil/sicp

;;; Exercise 2.67

; > (decode sample-message sample-tree)
; {a d a b b c a}

;;; Exercise 2.68

(define (encode-symbol symbol tree)
  (cond ((null? tree) (error "unexpected null"))
        ((leaf? tree) 
         (if (element-of-set? symbol (symbols tree))
             nil
             (error "symbol not in tree:" symbol)))
        ((element-of-set? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        ; assume in left branch, verify later
        (else (cons 0 (encode-symbol symbol (left-branch tree))))))

;;; Exercise 2.69

(define sample-pairs '((a 4) (b 2) (d 1) (c 1)))

(define (successive-merge leaves)
  ;the first two are the smallest weight
  (if (< (length leaves) 2) (car leaves)
      (let ((branch (make-code-tree (car leaves) (cadr leaves)))
            (rest (cddr leaves)))
        (successive-merge (adjoin-set branch rest)))))

;;;Exercise 2.70

(define lyric-alphabet
  '((a 2) (get 2) (sha 3) (wah 1) 
          (boom 1) (job 2) (na 16) (yip 9)))

(define song '(Get a job
                   Sha na na na na na na na na
                   Get a job
                   Sha na na na na na na na na
                   Wah yip yip yip yip yip yip yip yip yip
                   Sha boom))

; > (length (encode song (generate-huffman-tree lyric-alphabet)))
; 84

; whereas encoding with a straight 3-bits-per-symbol would use 
; 108 bits.

;;; Exercise 2.71

; The encoding results in a symbol set that looks like
; 00000, 00001, 0001, 001, 01, 1.

; The trees are such that all right branches lead directly to leaves.
;
;         *
;        / \
;       *   e
;      / \
;     *   d
;    / \
;   *   c
;  / \
; a   b
;
; and similarly for any n.
; The most frequent symbol uses 1 bit and the least N bits.

;;; Exercise 2.72

; To encode a symbol in an alphabet of N takes O(N) steps at worst,
; because of scanning the lists at each level. This is the case for
; an alphabet and a message of uniform frequency distribution; on 
; average you must search through 3N/4 symbols to find the right
; leaf node.

; In the case of exercise 2.71, for an alphabet of size N
; encoding takes 1 step half of the time, because
; branch symbol set is searched before the left. The search 
; takes 2 steps if the second symbol, 3 steps if the third,
; and so on; so averaged over the frequency of occurrence
; we get 2 steps, or O(1). (note that the implementation
; above does not bother searching the left symbol sets, instead 
; opting to error out of it reaches a leaf node that does not
; match the symbol. This is necessary to achieve O(1) in this case.)

;;;pasted from book

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (element-of-set? x set)
     (cond ((null? set) false)
           ((equal? x (car set)) true)
           (else (element-of-set? x (cdr set)))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left)
           (weight right))))

(define (left-branch  tree) (car  tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch
                (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)   ;symbol
                               (cadr pair)) ;frequency                   
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))