; 1.1
10
12
8
3
6
19
#f
4
16
6
16

; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* (- 6 2) (- 2 7))
)  #=> -37/50

; 1.3
(define (ssottl x y z)
        (cond ((first-smallest x y z) (sum-of-squares y z))
              ((first-smallest y x z) (sum-of-squares x z))
              ((first-smallest z y x) (sum-of-squares y x))))

(define (first-smallest x y z)
        (not (or (> x y) (> x z))))

; 1.4
; a + |b|

; 1.5
; if applciative order:
(test 0 (p))
(test 0 (p))
...
; normal order:
(if (= 0 0) 0 (p))
(if #t 0 (p))
0
; 1.6
; Infinite loop -- in applicative order, new-if's arguments all get evaluated before getting injected into the body of new-if. which means the interpreter keeps trying to improve.

; 1.7
; small numbers:
; > (sqrt-iter 1.0 0.0001)
0.03230844833048122
; (should have been 0.01)
; good-enough is too large.
;
; big numbers:
; > (sqrt-iter 1.0 (expt 10 48)) gives an immediate answer
; > (sqrt-iter 1.0 (expt 10 49)) gives an infinite loop
; it's never good enough because difference between each
; float and the next is > 0.0001

;better: replace 0.001 in good-enough? with
(* 0.001 x)
; -- works from around 10^-300 up to 10^300

; 1.8
(define (cube-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-iter (improve guess x) x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (square x) (* x x))
(define (cube x) (* x x x))


