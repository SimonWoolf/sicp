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

; alternative (ignoring gcd) (probs less efficient, does unnecessary
; multiplication):
(define (make-rat n d)
  (if (< (* n d) 0)
      (cons (- (abs n)) (abs d))
      (cons (abs n) (abs d))
      ))

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

; Altermative:
; - define (lms p q) = (length (make-segment p q))
; - given 4 points, store as ((a, c), (b, d))
; where a and c are opposites, defined as (lms a c) > (lms a b), (lms a d)
; - perimeter: (* 2 (+ (lms a b) (lms a d))
; - area: (* (lms a b) (lms a d))

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

; Edit: we did addition!
[define [add a b]
  [lambda [f] [lambda [x] [[a f] [[b f] x]]]]]

;And power a, b is
[lambda [f] [[b a] f]]]
; i.e.
[b a]

; i.e. for three squared, in both places there was an f in
; two there's now a 'three', i.e. a threeifier. so [three [three x]]
; i.e. three squared


; 2.7 car, cdr

; 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

; 2.9
; width(x + y) = width(x) + width(y)
; width(x - y) = width(x) + width(y)
;
; width(x * y) can't be a fn of width(x) and width(y):
; x = (-1,1), y = (-1,1) both have width 2
; product has width 2
; x = (0,2), y = (0,2) both have width 2
; product has width 4

; 2.10
(if (and (< x 0) (> y 0))
  (...))

; 2.11
(define (mul-interval-better x y)
  (define (classify interval)
    (define a (lower-bound interval))
    (define b (upper-bound interval))
    (cond ((and (> a 0) (> b 0)) 1)
          ((and (< a 0) (< b 0)) -1)
          (else 0)))

  (define x1 (lower-bound x))
  (define x2 (upper-bound x))
  (define y1 (lower-bound y))
  (define y2 (upper-bound y))

  ; phew!
  (case (cons (classify x) (classify y))
    ('(1 . 1) (make-interval (* x1 x2) (* y1 y2)))
    ('(-1 . -1) (make-interval (* y1 y2) (* x1 x2)))
    ('(1 . -1) (make-interval (* x2 y1) (* y2 x1)))
    ('(-1 . 1) (make-interval (* x1 y2) (* y1 x2)))
    ('(0 . 1) (make-interval (* x1 y2) (* x2 y2)))
    ('(1 . 0) (make-interval (* y1 x2) (* y2 x2)))
    ('(0 . -1) (make-interval (* x2 y1) (* x1 y1)))
    ('(-1 . 0) (make-interval (* y2 x1) (* y1 x1)))
    ('(0 . 0) (make-interval (min ((* x1 y2) (* x2 y1)))
                             (max ((* x1 y1) (* x2 y2)))))
    ))

; 2.12
(define (make-center-percent c p)
  (make-center-width c (* c p 0.001)))

(define (percent i)
  (* 100 (/ (width i) (center i))))

; 2.13
; x(1 + d)y(1 + e)
; = xy(1 + de + d + e)
; ~= xy(1 + d + e) for small d, e
; so: sum the percentage tolerances

; 2.14 - 2.16
; Yes.
; Our interval arithmetic has no concept of intervals being
; 'the same', only equal. So e.g. if I do R1/R1, that should
; be 1 no matter what the value of R1, but our arithmetic treats
; the errors in R1 and R1 as being independent, even when
; they're not. So ELA is correct.
;
; What we need is an interval arithmetic that has the concept
; of how independent intervals are from each other.
; I.e. 'covariance as well as variance'.
; Not going to attempt this now.
