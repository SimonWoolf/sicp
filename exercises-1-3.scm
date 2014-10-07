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
