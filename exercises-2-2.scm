; 2.17
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

; 2.18
(define (reverse l)
  (if (= 1 (length l))
      l
      (append (reverse (cdr l))
              (list (car l)))))

; or better still, handle length 0 lists
; (needed for count-change):
(define (reverse l)
  (if (= 0 (length l))
      '()
      (append (reverse (cdr l))
              (list (car l)))))

; 2nd go: iterative way is much better. Literally picking elements of the head
; of the list and consing them onto the result. Sometimes recursive thinking
; gives you a worse result
(define (reverse list)
  (define (reverse-iter list result)
    (if (null? list)
        result
        (reverse-iter (cdr list) (cons (car list) result))
        ))
  (reverse-iter list null))

; 2.19
(define (first-denomination coin-values)
  (car (last-pair coin-values)))

(define (except-first-denomination coin-values)
  (reverse (cdr (reverse coin-values))))

(define (no-more? coin-values)
  (= 0 (length coin-values)))

; Order shouldn't affect it, no
; though might affect efficiency

; 2.20
(define (same-parity x . y)
  (define (same-parity-helper x . y)
  (if (= 0 (length y))
      null
  (if (= (modulo x 2)
         (modulo (car y) 2))
      (cons (car y)
            (apply same-parity-helper x (cdr y)))
      (apply same-parity-helper x (cdr y)))))
  (cons x (apply same-parity-helper x y)))

; will's suggestion: signature of same-parity-helper
; can just be (x y), so you don't have to use apply
;
; 2nd go, using filter. Much nicer :)
(define (same-parity original . others)
  (filter (lambda (x) (= (modulo x 2) (modulo original 2)))
          others)

; 2.21
(define (square-list items)
  (if (null? items)
      null
      (cons (* (car items) (car items))
            (square-list (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

; 2.22
; It takes things off the head of items list
; and attaches them to the head of answer list
; [1 2 3,] -> [2 3, 1] -> [3, 2 1] -> [,3 2 1]
; The other way
; produces a left-folded rather than a right-folded
; list.
; i.e. ((((), 1), 2), 3), 4) rather than (1, (2, (3, (4, ()))))

; 2.23
(define (for-each proc l)
  (if (null? l)
      null
      (begin
        (proc (car l))
        (for-each proc (cdr l)))))

; 2.24
;   /\
;  1 /\
;   /\ o
;  2 /\
;   /\ o
;  3 /\
;   4  o

; 2.25
; (note: apply left to right, reverse of composition convention)
; d d a d a
; a a
; d a d a d a d a d a d a

; 2.26
(append x y) ;=>
'(1 2 3 4 5 6)
(cons x y) ;=>
'((1 2 3) 4 5 6)
(list x y) ;=>
'((1 2 3) (4 5 6))

; 2.27
(define (deep-reverse l)
  (if (pair? l)
      (append (deep-reverse (cdr l))
              (list (deep-reverse (car l))))
      l))

; 2nd go (without append):
(define (deep-reverse list)
  (define (reverse-iter l result)
    (if (null? l)
        result
        (reverse-iter (cdr l) (cons (deep-reverse (car l))
                                    result))
        ))
  (if (pair? list)
      (reverse-iter list null)
      list))

; Will's solution:
(define (deep-reverse l)
  (define (rev l) (if (list? l) (deep-reverse l) l))
  (reverse (map rev l)))

; 2.28
; :'(
; this just returns the thing you put it!
(define (fringewrong l)
  (define (fringe-iter l result)
    (cond ((null? l) result)
           ((pair? l) (fringe-iter (cdr l) (cons (fringe-iter (car l) result) result))
          (fringe-iter null (cons l result)))))
  (fringe-iter l null))

; Note: stop trying to do clever thigns and just use append....
(define (fringe original-list)
  (define (fringe-helper list result)
    (cond ((null? list) result)
          ((not (pair? list)) (cons list result))
          (else (append (fringe (car list)) (fringe (cdr list))))))
  (fringe-helper original-list null))
; Actually the helper in the above is unnecessary. See enumerate-tree
; definition from later on:
; (define (enumerate-tree tree)
;  (cond ((null? tree) nil)
;        ((not (pair? tree)) (list tree))
;        (else (append
;               (enumerate-tree (car tree))
;               (enumerate-tree (cdr tree)))))

; 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define left-branch car)
(define right-branch cadr)
(define branch-length car)
(define branch-structure cadr)
(define has-submobiles? list?)

(define example-mobile
  (make-mobile (make-branch 5 2)
               (make-branch 3 (make-mobile (make-branch 2 6)
                                           (make-branch 1 (make-mobile (make-branch 2 1)
                                                                       (make-branch 4 3)))))))

(define (total-branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (has-submobiles? structure)
        (total-weight structure)
        structure)
    ))

(define (total-weight mobile)
  (+  (total-branch-weight (left-branch mobile))
      (total-branch-weight (right-branch mobile))))

(define (torque branch)
  (* (branch-length branch)
     (total-branch-weight branch)))

(define (balanced? mobile)
  (and (equal? (torque (left-branch mobile))
               (torque (right-branch mobile)))
       (balanced-submobiles? mobile)))

(define (balanced-submobiles? mobile)
  (and (balanced-branch? (left-branch mobile))
       (balanced-branch? (right-branch mobile))))

(define (balanced-branch? branch)
  (let ((structure (branch-structure branch)))
    (if (has-submobiles? structure)
       (balanced? structure)
       true)))

; part 4: need to change little, only s/cadr/cdr in
(define right-branch cadr)
(define branch-structure cadr)

; 2.30
(define (square-tree tree)
  (cond ((null? tree) null)
        ((list? tree)
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))
        (else (* tree tree))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (list? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

; 2.31
; Note: map takes a function of 1 argument
; but that's ok as the lambda closes over the
; definition of proc
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (list? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

(define (square x) (* x x))

; 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset) (cons (car s) subset))
                          rest)))))
; subsets of (x U S) = {S U U_(T ⊂ S) {{x U T}}}
; the {x} comes out of the null element of (T ⊂ S)

; 2.33
(define (map p sequence)
  (accumulate (lambda (item acc) (cons (p item) acc))
              null sequence))

; note the order swapped! accumulate is cons-ing things
; car'd from seq1 onto the accumulator
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (item acc) (+ 1 acc)) 0 sequence))

; 2.34
; Note: this works because accumulate is a right-fold,
; so last element in the seq gets combined with the accumulator first.
; If was a left-fold, would have to reverse the sequence first
(define
  (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff acc)
     (+ this-coeff (* x acc)))
   0
   coefficient-sequence))

; 2.35
(define (count-leaves tree)
  (accumulate (lambda (item acc)
                (if (list? item)
                    (+ acc (count-leaves (car item))
                           (count-leaves (cdr item)))
                    (+ acc 1)))
              0
              tree))
; works, but not actually using accumulate in any kind
; of elegant way, it's basically just doing the same as
; the original definition. Also not using map.
(define (count-leaves2 tree)
  (accumulate (lambda (item acc)
                    (+ acc 1))
              0
              (enumerate-tree tree)))
; Still not using map though. What're they getting at
; in this question? This maybe?
(define (count-leaves tree)
  (accumulate +
              0
              (map (lambda (leaf) 1)
                   (enumerate-tree tree))))

; Second go: maybe going for something like
(define (count-leaves t)
  (accumulate + 0 (map (lambda (tree)
                         (if (pair? tree)
                             (count-leaves tree)
                             1)) t))

; 2.36
; eeeasy!
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; 2.37
(define (matrix-*-vector m v)
  (map (lambda (m_i)
         (dot-product m_i v))
       m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m_i)
           (matrix-*-vector cols m_i))
         m)))

; 2.38
; see notes about argument combination order!
(fold-right / 1 (list 1 2 3)) => 3/2
(fold-left  / 1 (list 1 2 3)) => 1/6
(fold-right list nil (list 1 2 3)) => '(1 (2 (3 null)'))
(fold-left  list nil (list 1 2 3)) => '(((null 1) 2) 3)

; The property for fold-left and fold-right to be the same is associativity.
; BUT -- unless the initial value is the identity element for the operation, you also need commutitivity, don't you, as the initial is in a different place....?
; E.g. consider some operation like string concatenation that's associative but not commutitive
> (accumulate string-append "init" (list "a" "b"))
"abinit"
> (fold-left string-append "init" (list "a" "b"))
"initab"
; Is commutativity strong enough to guarantee that they'll produce the same values?
; does commutativity imply associativity for binary operations? no! need both.
; e.g.
(foldl (lambda (item acc)
         (/ (+ item acc) 2))
       0
       (list 1 2))

1 1/4
(foldr (lambda (item acc)
         (/ (+ item acc) 2))
       0
       (list 1 2))
1

; 2.39
(define (reverse sequence)
  (foldr
   (lambda (item acc)
     (append acc (list item)))
   null sequence))

(define (reverse sequence)
  (fold-left
   (lambda (acc item) ; sodding reversed argument order!
     (cons item acc))
   null sequence))

; or with a sensible language:
(define (reverse1 list)
  (foldl cons null list))

; 2.40
; this is just method extraction...
(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j)
             (list i j))
           (enumerate-interval
             1
             (- i 1))))
    (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
        prime-sum?
        (unique-pairs n))))

; 2.41
; unique-pairs but with the inside 'list i j'
; replaced by the j
; can we abstract this to a higher order function,
; a (unique-k-tuples n k)?
(define (unique-triplets n)
  (flatmap
    (lambda (i)
      (flatmap (lambda (j)
             (map (lambda (k)
                    (list i j k))
                  (enumerate-interval
                    1
                    (- j 1))))
           (enumerate-interval
             1
             (- i 1))))
    (enumerate-interval 1 n)))

(define (triplets-that-sum-to s n)
  (define (sum-to-s? triplet)
    (= s (foldr + 0 triplet)))
  (filter
    sum-to-s?
    (unique-triplets n)))

; 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) ; each solution is a list of positions
           (safe? k positions))
         (flatmap ; maps each solution at k-1 to a list of board-size proposed solutions
                  ; then flattens
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position ; adds the kth queen to the list of positions
                    new-row
                    k
                    rest-of-queens))
                 (enumerate-interval
                  1
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define row car)
(define col cadr)

(define empty-board null)

(define (adjoin-position row-num col-num positions)
  (cons (list row-num col-num) positions))

(define (safe? k positions)
  (define proposed-queen (car positions))
  (define other-queens (cdr positions))
  (not (or (append
   (map (lambda (other)
         (= (row other) (row proposed-queen)))
       other-queens)
   (map (lambda (other)
         (= (+ (col other) (row other))
            (+ (col proposed-queen) (row proposed-queen))))
        other-queens)
   (map (lambda (other)
         (= (- (col other) (row other))
            (- (col proposed-queen) (row proposed-queen))))
        other-queens)
   ))))

; 2.43
; queen-cols is called k times, rather than once. So will run ~board-size times slower (with the dubious but simplifying assumption that vast majority of time is spent in the last iteration, k = board-size; actually even slower than that)
