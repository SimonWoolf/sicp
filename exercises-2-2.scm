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

; 2.28
; :'(
; this just returns the thing you put it!
(define (fringewrong l)
  (define (fringe-iter l result)
    (cond ((null? l) result)
           ((pair? l) (fringe-iter (cdr l) (cons (fringe-iter (car l) result) result))
          (fringe-iter null (cons l result)))))
  (fringe-iter l null))
