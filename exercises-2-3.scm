; 2.53
(list 'a 'b 'c) => '(a b c)
(list (list 'george)) => '((george))
(cdr '((x1 x2) (y1 y2))) => '((y1 y2))
(cadr '((x1 x2) (y1 y2))) => '(y1 y2)
(pair? (car '(a short list))) => #f
(memq 'red '((red shoes) (blue socks))) => #f
(memq 'red '(red shoes blue socks)) => '(red shoes blue socks)

; 2.54
(define (my-equal? a b)
  (cond
    ((and (number? a) (number? b)) (= a b))
    ((and (symbol? a) (symbol? b)) (eq? a b))
    ; need or rather than and for null else if one is
    ; null and the other isn't it'll be caught by the
    ; list branch and try to call car on it
    ((or (null? a) (null? b)) (eq? a b))
    ((and (list? a) (list? b))
     (and (my-equal? (car a) (car b))
          (my-equal? (cdr a) (cdr b))))
    (else #f)
    ))

; Or using an all? helper I just wrote:
(define (all? proc list)
  (if
    (null? list)
    #t
    (and (proc (car list))
         (all? proc (cdr list)))
    ))

(define (any? proc list)
  (if
    (null? list)
    #f
    (or (proc (car list))
        (all? proc (cdr list)))
    ))

(define (my-equal? a b)
  (cond
    ((all? number? (list a b)) (= a b))
    ((all? symbol? (list a b)) (eq? a b))
    ; need any? rather than all? for null else if one is
    ; null and the other isn't it'll be caught by the
    ; list branch and try to call car on it
    ((any? null? (list a b)) (eq? a b))
    ((all? list? (list a b))
     (and (my-equal? (car a) (car b))
          (my-equal? (cdr a) (cdr b))))
    (else #f)
    ))

; 2.55
; ''abracadabra evaluates to '(quote abracadabra) = (list 'quote 'abracadabra)

; 2.56
((exponential? exp)
 (make-product (exponent exp)
               (make-exponential
                 (base exp)
                 (- (exponent exp) 1))))

(define (exponential? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))

(define (make-exponential base exponent)
  (cond
    ((not (number? exponent))
     (error "Can't do non-numerical exponents"))
    ((= exponent 0) 1)
    ((= exponent 1) base)
    (else (list '** base exponent))))

; 2.57

(define (make-sum . addends)
  (let ((sum-numeric-addends (foldr + 0 (filter number? addends)))
        (non-numeric-addends (filter (lambda (x) (not (number? x))) addends)))
    (define net-addends (if (= 0 sum-numeric-addends)
                          non-numeric-addends
                          (cons sum-numeric-addends non-numeric-addends)))
    (if (null? (cdr net-addends)) ; only one item
      (car net-addends)
      (cons '+ net-addends))))

(define (make-product . multiplicands)
  (let ((prod-numeric-multiplicands (foldr * 1 (filter number? multiplicands)))
        (non-numeric-multiplicands (filter (lambda (x) (not (number? x))) multiplicands)))
    (define net-multiplicands (cond
                                ((= 1 prod-numeric-multiplicands)
                                 non-numeric-multiplicands)
                                ((= 0 prod-numeric-multiplicands)
                                 '(0))
                                (else (cons prod-numeric-multiplicands non-numeric-multiplicands))))
    (if (null? (cdr net-multiplicands)) ; only one item
      (car net-multiplicands)
      (cons '* net-multiplicands))))

(define (addend s) (cadr s))
(define (augend s) (if (null? (cdddr s)) ; only 2 args
                       (caddr s)
                       (cons '+ (cddr s))))

(define (multiplier p) (cadr p))
(define (multiplicand p) (if (null? (cdddr p)) ; only 2 args
                             (caddr p)
                             (cons '* (cddr p))))

; 2.58 - infixness!
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (addend s) (car s))
(define (augend s) (caddr s))

(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

; start with old make-sum & make-product from q, as can assume only 2 args
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))


; 2.59
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((element-of-set? (car s1) s2)
         (union-set (cdr s1) s2))
        (else (cons (car s1) (union-set (cdr s1) s2)))))

; 2.60
; element-of-set is unchanged
; intersection-set is also unchanged

(define adjoin-set cons)

(define union-set append)

; would use this if you want O(1) adjoin-set and O(n) union-set
; at the expense of space. eg if adding things to a set is very common

; 2.61
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set))
         (cons x set))
        ((= x (car set))
         set)
        (else
          (cons (car set) (adjoin-set x (cdr set))))))
; only needs to go as far as the right insertion point

; 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1 (union-set
                              (cdr set1)
                              (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set
                              (cdr set1)
                              set2)))
                  ((< x2 x1)
                   (cons x2 (union-set
                              set1
                              (cdr set2)))))))))

; 2.63
; They produce the same results.
;
; But they don't have the same complexity, because the first one does an appen
; at each non-null vertix which is O(size of left side). So it's O(n^2) rather
; than O(n). (will it be n^2? might be better than that. not sure). So right
; one is more efficient even though it's less intuitive

; 2.64

(define (list->tree elements)
  (car (partial-tree
        elements (length elements))))

; I rewrote it to be easier for me to understand
(define (partial-tree elts n)
  (cond ((= n 0) (cons '() elts))
    (else
      (define left-size
        (quotient (- n 1) 2)) ; integer division
      (define left-tree-pair
        (partial-tree elts left-size))
      (define left-tree
        (car left-tree-pair))
      (define non-left-elements
        (cdr left-tree-pair))
      (define centre
        (car non-left-elements))
      (define right-tree-pair
        (partial-tree (cdr non-left-elements) (- n left-size 1))) ; -1 for the centre one
      (define right-tree
        (car right-tree-pair))
      (define remaining-els
        (cdr right-tree-pair))
      (cons (make-tree centre left-tree right-tree) remaining-els)
      )))

; recursively takes the first n elements, splits it into three (left half,
; centre right half), and calls partial-tree on the left and right halves. (not
; quite since the remaining-els trick saves calculating the right half elements
; up front, but that's the basic idea)
;
; Since integer division rounds down, it prefers things being on the right side:
;   5
;  / \
; 1   9
; |  / \
; 3 7  11
;
; Order of growth: partial-tree is called once per node, so O(n)

; 2.65
; can't we just do this?
(define (union-set s1 s2)
  (foldl adjoin-set s1 (tree->list s2)))
; except this is O(n.log(n)) since adjoin-set is O(log(n)). hmm
;
; Could always just convert to ordered lists and back, but not sure that's in
; the spirit of the question either :/
(define (union-set s1 s2)
  (list->tree (union-set-ordered-list (tree->list s1) (tree->list s2))))
(define (intersection-set s1 s2)
  (list->tree (intersection-set-ordered-list (tree->list s1) (tree->list s2))))
; :(
; but it is technically O(n), since all four ops are O(n), and each done once
; TODO: think of other solutinos

; 2.66
; just element-of-set with (key ...) wrapped around every entry retreival
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records))) true)
        ((< given-key (key (entry set-of-records)))
         (lookup
           given-key
           (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup
           given-key
           (right-branch set-of-records)))))

; 2.67
'(A D A B B C A)

; 2.68
(define (encode-symbol symbol tree)
  (cond ((leaf? tree)
         '())
        ((memq symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((memq symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol not in tree: " symbol ", tree: " (symbols tree)))))

; Addendum re 2.72: would probably be better if searching the right branch
; first? since when making the tree, weight(left-branch <=
; weight(right-branch), so more efficient to search the branch with higher
; weight first

; 2.69
(define (successive-merge leaf-set)
  (if (= 1 (length leaf-set))
    (car leaf-set)
    (successive-merge (adjoin-set (make-code-tree (car leaf-set)
                          (cadr leaf-set))
          (cddr leaf-set)))))

; 2.70
(define 1950s (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))))
; > 1950s
'(((leaf NA 16)
   ((leaf YIP 9)
    (((leaf A 2) ((leaf WAH 1) (leaf BOOM 1) (WAH BOOM) 2) (A WAH BOOM) 4)
     ((leaf SHA 3) ((leaf JOB 2) (leaf GET 2) (JOB GET) 4) (SHA JOB GET) 7)
     (A WAH BOOM SHA JOB GET)
     11)
    (YIP A WAH BOOM SHA JOB GET)
    20)
   (NA YIP A WAH BOOM SHA JOB GET)
   36))


(encode '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM) 1950s)
'(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)
; 84 bits
; A fixed-length code would take 3 bits per word for 36 words = 108 bits. So we save 20 bits

; 2.71
; Tree will have one leaf and one node on every level
;    /\
;   /\
;  /\
; /\
; etc
;
; Most frequent symbol: 1 bit
; Least frequent symbol: height of the tree = n-1 bits


; 2.72
; order of growth
; = O(number of nodes to search * length of symbol list at each node)
; = O(height of tree * length of symbol list)
; For worst case (eg 2.71), each of those is n, so O(n^2)
; For best case (balanced tree), height is log(n), so \Omega(n.log(n))
;
; for 2.71-worst-case, order of encoding for most frequent symbol depends on
; whether it's on the left or the right branch. If the left, it's O(1). If the
; right, O(n). For the least-frequent symbol, it's O(n^2) per previous para.
;
; could optimise by making encode-symbol search right branch first, as that has
; weight >= left branch -- see addendum to 2.68
