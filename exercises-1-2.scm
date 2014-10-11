; 1.9
; first one:
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc inc ( (+ 0 5)))))
(inc (inc (inc inc ( 5 ))))
(inc (inc (inc 6))))
(inc (inc 7)))
(inc 8))
9

; second one:
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

; first is recursive, second iterative

; 1.10
(= (A 1 10)
   (A 0 (A 1 9))
   ...
   (^ 2 10)
   1024)

(= (A 2 4)
   (A 1 (A 2 3))
   (A 1 (A 1 (A 2 2)))
   (A 1 (A 1 (A 1 (A 2 1))))
   (A 1 (A 1 (A 1 2)))
   (A 1 (A 1 (A 0 (A 1 1))))
   (A 1 (A 1 (A 0 2)))
   (A 1 (A 1 4))
   ...
   65536
   )

(= (A 3 3) 65536)

(define (f n) (* 2 n))
(define (g n) (^ 2 n))
(define (h n) ##TODO##) -- tetration..?

; 1.11
; recursive:

(define (f n)
  (if (< n 3)
    n
    (+
      (f (- n 1))
      (* 2 (f (- n 2)))
      (* 3 (f (- n 3)))
      )))

; iterative:

(define (f n)
  (f-iter 2 1 0 n))

(define (f-iter a b c count)
  (if (= count 0)
      c
      (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))

; 1.12

(define (pascal row col)
  (cond ((or (= col 0) (= col row)) 1)
        ((or (< col 0) (> col row)) 0)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))

; 1.13
;See paper

; 1.14
;Tree: see paper
;Space-complexity: definitely \Theta(n*m) or less. Maybe \Theta(n)?
;Time-complexity: \Theta(n^m) ???

; 1.15
; 1. applicative order. So things are evaluated eagerly. So angle is divided by 3 five times (to get to 0.05). So, 5.
; 2. Space complexity and time-complexity both number of times it takes to divide a by 3 to get below a constant (As linearly recursive the way it's implemented here). Which is better than linear. Logarithmic to base 3..? So theta(log a)

; 1.16
(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b n result)
  (cond ((= n 0) result)
        ((even? n) (expt-iter (squared b) (/ n 2) result))
        (else (expt-iter b (- n 1) (* b result)))
        )
  )

(define (squared x) (* x x))

; 1.17
(define (* a b)
  (cond ((= b 1) a)
        ((even? b) (* (double a) (halve b))
        (else (+ a (* a (- b 1))))
        )

; 1.18
; follows by analogy

; 1.19
; {T_{p,q}}^2 = T_{p^2 + q^2, q^2 + 2pq} - see paper for proof
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (squared p) (squared q))
                   (+ (squared q) (* 2 q p))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (squared x) (* x x))

; 1.20
; normal order: seems like it should be similar as 'if' still done eagerly in normal order...
(gcd 206 40)
(gcd 40 (remainder 206 40))
; if() evaluates b: 6
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; if() evaluates b: 4
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; if() evaluates b: 2
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40))(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; if evaluates b: 0
(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
; So remainder evaluates around 18 times in total in normal order!
; Applicative order: 4ish

; 1.21
; 199, 1999, 7

; 1.22
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
    (= (remainder b a) 0))
(define (square n) (* n n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-milliseconds) 
                       start-time)) #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= n (smallest-divisor n))
  )

(define (sfp start count)
  (cond ((= 3 count) (display "done"))
        ((timed-prime-test start) ;nb this works, so (display) must be truthy
         (sfp (+ start 2) (+ count 1)))
        (else (sfp (+ start 2) count))
        )
  )

(define (search-for-primes start)
  (if (even? start)
      (sfp (+ 1 start) 0)
      (sfp start 0)
      ))

(define (even? n)
  (= 0 (modulo n 2))
  )

; smallest primes larger than:
; 1009, 13, 19. All 0ms.
; 10007, 9, 37. all 0ms
; 100003, 19, 43. all 0ms
; 1000003, 33, 37. all 0ms
; ... adding some zeros so we can get somewhere:
; 10^9: 4ms
; 10^10: 12ms
; 10^11: 36ms
; 10^12: 134ms
; So yes, this matches theta(sqrt(n)) very well.
; So yeah, it is compatible.

; With a next method: 12->8, 36->22, 134->74.
; Observed ratio: about 2/3.
; Conclusion: it doesn't spend all its time in find-divisor...?
; or cos it has to call a new function..?

; 1.24: can't test anything above 10^9, which was 0ms
; ran into `random: contract violation
  ;expected: (or/c (integer-in 1 4294967087) pseudo-random-generator?)
  ;given: 10000000000
; would expect 10^6 to take twice as long as 10^3 though

; 1.25: no, as the new one takes remainders as it goes along, rather than doing all the
; exponentiation first

; 1.26: in louis's version, for even exp, it calls two copies of expomod, not one.
; so this increases the number of times it's called exponentially
; O(exp(log(n))) = O(n)
