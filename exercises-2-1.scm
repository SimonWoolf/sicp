; 2.1
(define (make-rat n d)
  (define (same-sign x y)
    (or (and (< n 0)
             (< d 0))
        (and (> n 0)
             (> d 0)))
    )
  (let ((g (gcd n d))
        (signed-n (if (same-sign n d)
                      (abs n)
                      (- (abs n))
                      )))
    (cons (/ signed-n g) 
          (/ (abs d) g))))

; 2.2
(define [make-point x y]
  [cons x y])

[define [x-point p]
  [car p]]

[define [y-point p]
  [cdr p]]

(define [make-segment p q]
  [cons p q])

[define [start-point s]
  [car s]]

[define [end-point s]
  [cdr s]]
 
[define [midpoint-segment s]
  [average-points [start-point s]
                  [end-point s]]]

[define [average-points p q]
  [make-point [/ [+ [x-point p] [x-point q]] 2]
              [/ [+ [y-point p] [y-point q]] 2]]]

; 2.3
[define [make-rect s1 s2 s3 s4]
  [cons s1 [cons s2 [ cons s3 s4]]]]

[define [side1 rect]
  [car rect]]
[define [side2 rect]
  [car [cdr rect]]]
[define [side3 rect]
  [car [cdr [cdr rect]]]]
[define [side4 rect]
  [car [cdr [cdr [cdr rect]]]]]


[define [perim rect]
  [+ [length [side1 rect]]
     [length [side2 rect]]
     [length [side3 rect]]
     [length [side4 rect]]]]

[define [area rect]
  [sqrt [* [length [side1 rect]]
           [length [side2 rect]]
           [length [side3 rect]]
           [length [side4 rect]]]]]

[define [length segment]
  [let [[p [start-point segment]]
        [q [end-point segment]]]
    [sqrt [+ [square [- [y-point p]
                        [y-point q]]]
             [square [- [x-point p]
                        [x-point q]]]]]
  ]]

; 2.4

;So:
(car (cons x y)) => ((lambda (m) (m x y)) (lambda (p q) p))
=> ((lambda (p q) p) x y)
=> x

; Corresponding cdr:
(define (car z)
  (z (lambda (p q) q)))

; 2.5
; 2 and 3 are coprime. So result true by uniqueness
; of prime factorisation.

[define [cons x y]
  [* [expt 2 x] [expt 3 y]]]

[define [num-divisions x a]
  [define [numdiv-recur x a count]
    [if [not [= 0 [modulo x a]]]
        count
        [numdiv-recur [/ x a] a [+ count 1]]
    ]]
  [numdiv-recur x a 0]]

[define [car l]
  [num-divisions l 2]]

[define [cdr l]
  [num-divisions l 3]]

; 2.6

[add-1 zero]
=> (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
=> (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
=> (lambda (f) (lambda (x) (f x)))

[add-1 [add-one zero]]
=> (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
=> (lambda (f) (lambda (x) (f (lambda (x) (f x) x))))
=> (lambda (f) (lambda (x) (f (f x))))

; pattern: identified!
; useful helper:
[define [proc-to-int p]
[[p [lambda [x] [+ x 1]]] 0]]

; function composition give you *multiplcation*
; very neatly. but not addition!
[define [multiply a b]
  [lambda [f] [a [b f]]]]

; Addition.. hmm. This works, but it's **messy**.
; There must be a church-idiomatic way!
(define (plus a b)
  [let [[n [+ [proc-to-int a]
              [proc-to-int b]]]]
  [lambda [f] [lambda [x] [[repeated f n] x]]]])
