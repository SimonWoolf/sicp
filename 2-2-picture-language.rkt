#lang racket
; Credit to http://stackoverflow.com/a/13612435 for the racket viewport/posn approach
(require graphics/graphics)
(open-graphics)
(define vpsize 1000)
(define vp (open-viewport "A Picture Language" vpsize vpsize))
(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

(define (vector-to-posn v)
  ; vpsize - y as racket puts (0,0) at the top-left
  (make-posn (xco v) (- vpsize (yco v))))

(define make-vect cons)
(define xco car)
(define yco cdr)

(define (add-vect u v)
  (make-vect (+ (xco u) (xco v))
             (+ (yco u) (yco v))))

(define (sub-vect u v)
  (make-vect (- (xco u) (xco v))
             (- (yco u) (yco v))))

(define (scale-vect s v)
  (make-vect (* s (xco v))
             (* s (yco v))))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame cddr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect 
      (scale-vect (xco v)
                  (edge1-frame frame))
      (scale-vect (yco v)
                  (edge2-frame frame))))))

(define (segments->painter segment-list)   
  (lambda (frame)     
   (for-each     
     (lambda (segment)        
      (line         
        (vector-to-posn ((frame-coord-map frame) (start-segment segment)))         
        (vector-to-posn ((frame-coord-map frame) (end-segment segment)))))      
      segment-list)))

; Lets you specify segments as '(x1 y1 x2 y2)
(define (seg->pai list)
  (define (helper list result)
    (if (empty? list)
        result
        (helper (cdr list)
                (cons (make-segment (make-vect (caar list) (cadar list))
                                    (make-vect (caddar list) (cadddr (car list)))) result))))
    (segments->painter (helper list '())))

(define (transform-painter 
         painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                  (sub-vect (m corner1) 
                            new-origin)
                  (sub-vect (m corner2)
                            new-origin)))))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left  (transform-painter 
                        painter1
                        (make-vect 0.0 0.0)
                        split-point
                        (make-vect 0.0 1.0)))
          (paint-right (transform-painter
                        painter2
                        split-point
                        (make-vect 1.0 0.0)
                        (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (split firstop secondop)
  (define (result painter n)
    ; Note: smaller is defined as a zero-arity function, not
    ; a variable, to stop the body being evaluated eagerly
    (define (smaller) (result painter (- n 1)))
    (if (= n 0)
      painter
      (firstop painter (secondop (smaller) (smaller)))))
  result)

(define (below painter1 painter2)
  (rotate-ccw 90
              (beside (rotate-ccw 270 painter1)
                      (rotate-ccw 270 painter2))))


(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right 
                                   right))
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))

(define (flip-vert painter)
  (transform-painter 
   painter
   (make-vect 0.0 1.0)   ; new origin
   (make-vect 1.0 1.0)   ; new end of edge1
   (make-vect 0.0 0.0))) ; new end of edge2

(define (flip-horiz painter)
  (transform-painter 
   painter
   (make-vect 1.0 0.0)
   (make-vect 0.0 0.0)
   (make-vect 1.0 1.0)))

(define (rotate-ccw degrees painter)
  (define theta (degrees->radians degrees))
  (define (trig offset)
    ; add 0.5 to translate from centre-of-square-origin
    ; coordinates to bottom-left-origin coordinates
    (+ 0.5
       (* (/ 1 (sqrt 2)) ; radius of the circumcircle of the square
          (cos (+ offset (* pi (/ 5 4))))))) ; since zero-point of theta is bottom left
  (transform-painter 
   painter
   (make-vect (trig theta) (trig (- theta (/ pi 2))))
   (make-vect (trig (+ theta (/ pi 2))) (trig theta))
   (make-vect (trig (- theta (/ pi 2))) (trig (- theta pi)))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) 
                       (tr painter)))
          (bottom (beside (bl painter) 
                          (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 
         (square-of-four flip-horiz 
                         identity
                         (lambda (p) (rotate-ccw 180 p)) 
                         flip-vert)))
    (combine4 (corner-split painter n))))

(define frame-outline-painter
  (seg->pai '((0 0 0 1)
              (0 0 1 0)
              (0 1 1 1)
              (1 0 1 1))))

(define x-painter
  (seg->pai '((0 0 1 1)
              (0 1 1 0))))

(define diamond-painter
  (seg->pai '((0.5 0 0 0.5)
              (0.5 1 1 0.5)
              (0.5 0 1 0.5)
              (0 0.5 0.5 1))))

; ...close enough...
(define wave-painter
  (seg->pai '((0.2 0 0.3 0.6)
              (0.8 0 0.7 0.6)
              (0.3 0 0.4 0.3)
              (0.7 0 0.6 0.3)
              (0.4 0.3 0.6 0.3)
              (0.3 0.6 0.15 0.5)
              (0.15 0.5 0 0.6)
              (0.7 0.6 1 0.4)
              (0.4 1 0.4 0.8)
              (0.6 1 0.6 0.8)
              (0.4 0.8 0.6 0.8)
              )))

(define test-frame
  (make-frame (make-vect 50 50) (make-vect 900 0) (make-vect 0 900)))

((square-limit wave-painter 5) test-frame)