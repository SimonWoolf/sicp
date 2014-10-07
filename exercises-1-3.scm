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
