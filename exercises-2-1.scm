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
