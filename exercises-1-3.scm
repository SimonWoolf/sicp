; 1.29

(define (sigma f a next b)
  (if (> a b)
      0
      (+ (f a)
         (sigma f (next a) next b))))

(define (integral f a b n)
  (define dx (/ (- b a) n))
  (define (add-dx x) (+ x dx))
  (* (sigma f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpsons-int f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (next-all y) (+ y h))
  (define (next-odds-only y) (+ y (* 2 h)))
  (* (/ h 3)
     (+ (y 0)
        (* 2 (sigma f a next-all b))
        (* 2 (sigma f a next-odds-only b))
        (y n)))
  )

(define (cube x) (* x x x))

; n = 100: integral gives 0.24998750000000042
; simpsons gives 0.25
; n = 1000: integral gives 0.249999875000001
; simpsons gives 0.25

; Even at n=10, error in simpsons is 0 for cube 0 to 1!
; odd..

; 1.30

(define (sigma-iter f a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (+ result (f x)))))
  (iter a 0))

; NB excercise wanted to make you shadow a! is that really a good idea?

; 1.31

(define (product f a next b)
  (if (> a b)
      1
      (* (f a)
         (product f (next a) next b))))

(define (fac n)
  (product (lambda (x) x) 1 (lambda (x) (+ x 1)) n)
  )

(define (pi precision)
  (define last-even (* precision 2))
  (define (square x) (* x x))
  (define (plus2 x) (+ 2 x))
  (* 4
     (/ (* 2 last-even (product square 4 plus2 (- last-even 1)))
        (product square 3 plus2 last-even))
  ))

(exact->inexact (pi 80)) ; => 3.161288580501705

(define (product-iter f a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (* result (f x)))))
  (iter a 1))

; 1.32
;
(define (accumulate combiner identity f a next b)
  (if (> a b)
      identity
      (combiner (f a)
               (accumulate combiner identity f (next a) next b))
      ))

; Started using labda early, it's too useful, sorry :)
(define (product f a next b)
  (accumulate (lambda (x y) (* x y)) 1 f a next b))

(define (sum f a next b)
  (accumulate (lambda (x y) (+ x y)) 0 f a next b))

(define (accumulate-iter combiner identity f a next b)
  (define (iter x result)
    (if (> x b)
      result
      (iter (next x) (combiner result (f x)))))
   (iter a identity)
  )

; 1.33

(define (filtered-accumulate combiner filter identity f a next b)
  (if (> a b)
      identity
      (combiner (if (filter a)
                    (f a)
                    identity)
                (filtered-accumulate combiner filter identity f (next a) next b))
      ))

; sum of primes from a to b, assuming a prime? fn:
(filtered-accumulate (lambda (x y) (+ x y))
                     (lambda (x) (prime? x))
                     0
                     (lambda (x) (* x x))
                     a
                     (lambda (x) (+ x 1))
                     b
                     )

; product of +ve integers x < n that have GCD(x,n) = 1:
(filtered-accumulate (lambda (x y) (* x y))
                     (lambda (x) (= 1 (gcd x n)))
                     1
                     (lambda (x) (* x x))
                     1
                     (lambda (x) (+ x 1))
                     n
                     )

; 1.34
; I expect:
(f f) => (f 2)
=> (2 2)
=> "error: expected procedure got integer"

; 1.35
; 1 + 1/((1 + sqrt(5))/2)
;   = 1 + 2/(1 + sqrt(5))
;   = 1 + 2(1 - sqrt(5))/(1 + sqrt(5))(1 - sqrt(5))
;   = 1 + 2(1 - sqrt(5))/(-4)
;   = 1 + (sqrt(5) - 1)/2
;   = (1 + sqrt(5))/2

; 1.36
; x^x = 1000: x = 4.555532270803653
; Starting with 2.0: 34 steps
; With averaging: 9 steps

; 1.37
(define (cont-frac n d k)
  (define (cont-frac-helper n d k i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i)
              (cont-frac-helper n d k (+ i 1)))
           )
        )
    )
  (cont-frac-helper n d k 1)
  )

; need k=10 before I get 4d.p. accuracy -
; 0.61797 = 0.6180 to 4 d.p.

; iterative:
(define (cont-frac n d k)
  (define (cont-frac-iter n d k i result)
    (if (= i 0)
        result
        (cont-frac-iter n d k (- i 1)
                        (/ (n i) (+ (d i) result)))
        )
    )
  (cont-frac-iter n d k k 0)
  )

; 1.38
[define [euler-d i]
  [cond [[not [= 2 [modulo i 3]]] 1]
        [else [* 2 [/ [+ i 1] 3]]]
        ]]

[exact->inexact
   [cont-frac [λ [i] 1]
             euler-d
             30]]

; 1.39
[define [tan-cf x k]
  [define [tan-n i]
    [if [= i 1]
        x
        [- [* x x]]]]
  [define [tan-d i]
    [- [* i 2] 1]]
  [exact->inexact
    [cont-frac tan-n
               tan-d
               k]]
  ]

; 1.40
[define [cubic a b c]
  [lambda [x]
    [+ [* x x x]
       [* a x x]
       [* b x]
       c]
  ]]

; 1.41
[define [double f]
  [lambda [x] [f [f x]]]
  ]

; Predict: 13
; Actual: 21! Oh, of course, it's applying [double double]
; twice, for four doubles.

; 1.42
[define [compose f g]
  [lambda [x] [f [g x]]]
  ]

1.43
[define [repeated f n]
  [if [= n 1]
      f
      [compose f [repeated f [- n 1]]]
  ]]
; NB all of these higher order fns you need to know the number of arguments, as to
; define a lambda you need to list its formal params. Ways around that..?

; 1.44
[define [smoothed f]
  [lambda [x]
    [/ [+ [f [- x dx]]
          [f x]
          [f [+ x dx]]]
       3]]
  ]

[define [nfold-smoothed f n]
  [[repeated smoothed n] f]
  ]

; 1.46
#lang racket
[define [iterative-improve good-enough improve]
  [define [improver guess]
    [if [good-enough guess]
      guess
      [improver [improve guess]]
      ]
    ]
  improver
  ]

[exact->inexact [[iterative-improve
   [lambda [x] [< [abs [- [* x x] 5]] 0.00001]]
   [lambda [x] [average x [/ 5 x]]]]
   10]]
; => 2.2360701085328496

 [define [fixed-point-finder f initial-guess]
   [exact->inexact [[iterative-improve
                     [lambda [x] [close-enough? x [f x]]]
                     f]
                     initial-guess]
  ]]
