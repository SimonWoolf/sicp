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
