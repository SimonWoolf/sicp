; 2.77
; Without the magnitude part from the complex package, fails on the third set below
; assume it was rectangular.

(magnitude z) ; global namespace
(apply-generic 'magnitude z)
(get 'magnitude 'complex (contents z))
(magnitude (contents z)) ; in the complex package
(apply-generic 'magnitude (contents z))
(get 'magnitude 'rectangular (contents (contents z)))
(magnitude (contents (contents z))) ; in the rectangular package
(sqrt (+ (square (real-part (contents (contents z))))
             (square (imag-part (contents (contents z))))))

; apply-generic invoked twice
;
; afaics we don't technically need 'real-part, 'imag-part, and 'angle in the complex package to do magnitude? so APH's suggestion seems overkill...

; 2.78
(define (attach-tag type-tag contents)
  (if (equal? (type-tag 'scheme-number))
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum)
         (car datum))
        ((number? datum)
         'scheme-number)
        (else (error "Bad tagged datum:
                     TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum)
         (cdr datum))
        ((number? datum)
         datum)
        (else (error "Bad tagged datum:
                     CONTENTS" datum))))

; 2.79
(define (install-equality-package)
  ; internal helper procedures (numer, denom, etc) are assumed to be available.
  ; (in practice you'd just add these to install-rational-package etc to get
  ; the helpers)
  (define (swap-args fun)
    (lambda (a b) (fun b a)))
  ; I'm assuming a machine-epsilon global is defined.
  ; It's about 2.22e-16 for racket floats
  (define (approx-eq x y)
    (< (abs (- a b))
       machine-epsilon))
  (define (equ-num num1 num2)
   (= num2 num1))
  (define (equ-rat rat1 rat2)
   (and (= (numer rat1) (numer rat2))
        (= (denom rat1) (denom rat2))))
  (define (equ-com com1 com2)
   (and (= (real-part com1) (real-part com2))
        (= (imag-part com1) (denom com2))))
  (define (equ-com-num com num)
    (if (not (= 0 (imag-part com)))
      #f
      (= (real-part com) num)))
  (define (equ-rat-com rat com)
    (if (not (= 0 (imag-part com)))
      #f
      (approx-eq (real-part com)
                 (/ (numer-rat) (denom-rat)))))
  (define (equ-rat-num rat num)
    (approx-eq num
               (/ (numer-rat) (denom-rat))))
  (put 'equ? '(scheme-number scheme-number) equ-num)
  (put 'equ? '(rational rational) equ-rat)
  (put 'equ? '(complex complex) equ-com)
  (put 'equ? '(complex scheme-number) equ-com-num)
  (put 'equ? '(scheme-number complex) (swap-args equ-com-num))
  (put 'equ? '(rational scheme-number) equ-rat-num)
  (put 'equ? '(scheme-number rational) (swap-args equ-rat-num))
  (put 'equ? '(rational complex) equ-rat-com)
  (put 'equ? '(complex rational) (swap-args equ-rat-com))))

(define (equ? a b) (apply-generic 'equ? a b))

; 2.80
; This is a bit cheeky, but may as well take advantage of doing all the work to
; make equ? work across different argument types
(define (=zero? a) (equ? a 0))

; 2.81
; 1.
; Having not found an ('exp '(complex complex)), would try t1->t2 and t2->t1.
; both would succeed, so the cond would succeed first on t1->t2, which would
; try again to find an ('exp '(complex complex)), which is doomed to fail.

; 2.
; It's fine as it is. If the two have the same type, then coercing one to the
; type of another will never result in a different pair of types to what we
; started with, so will never get a procedure that (get op type-tags) failed to
; find in the first place.
;
; This may change when we start doing functions of more than two arguments! eg
; then you could have a fun of '(complex complex integer) which then needs to
; coerce the second two arguments into complex, so the middle one may need a
; complex->complex depending on how apply-generic is coded.

; 3.
(if (and (= (length args) 2)
         (not (eq? type1 type2))))

; 2.82
; This assumes we do have louis-reasoner's i->i methods
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (not (apply eq? type-tags))
          (define coercions-table
            (map (lambda (to-type)
                   (map (lambda (from-type)
                          (get-coercion from-type to-type))
                        type-tags))
                 type-tags))
          (define result (foldl (lambda (coercion-row acc)
                                  (or acc
                                      (if (any? null? coercion-row)
                                        '()
                                        ; multiple-input map - zips coercion-row
                                        ; with args and applies the corresponding
                                        ; coercion to each arg
                                        (apply-generic op (map (lambda (coercion arg)
                                                                 (coercion arg))
                                                               coercion-row args))))
                                  ) '() coercions-table))
          (or result (error "No method for these types"
                            (list op type-tags))))))))

; This is not sufficiently general in the non-tower case where everything needs to be coerced to some higher common denominator. Eg an isosceles and right-angled triange which 

; 2.83
