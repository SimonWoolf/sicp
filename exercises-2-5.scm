; NB: most of these answers are buggy (were written without running them, as
; didn't have get and put).  Later did actual running versions, can be found in
; polynomials.rkt

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
  (if (equal? type-tag 'scheme-number)
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
                 (/ (numer rat) (denom rat)))))
  (define (equ-rat-num rat num)
    (approx-eq num
               (/ (numer rat) (denom-rat))))
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
  (define (alleqv? . args)
    (foldl (lambda (x acc)
             (and acc (eqv? x (car args))))
           #t args))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (apply alleqv? type-tags)
          (error "No method for these types" (list op type-tags))
          (begin
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
                              (list op type-tags)))))))))

; This is not sufficiently general in the non-tower case where everything needs
; to be coerced to some higher common denominator. Eg an isosceles and
; right-angled triange which both need to be raised to triange

; 2.83
(define (install-raise-package)
  ; internal helper procedures (numer, denom, make-foo etc) are assumed to be
  ; available.  (in practice you'd just add these to individual packages etc to
  ; get the helpers).  Also assume integer package exists. And I guess the
  ; scheme-number package is now called 'real'? This is getting a bit
  ; hypothetical. pity we can't actually run these...
  (define (raise-int int)
    (make-rational int 1))
  (define (raise-rat rat)
    (make-scheme-number (/ (numer rat) (denom rat))))
  (define (raise-real real)
    (make-complex-from-real-imag real 0))
  (put 'raise '(integer) raise-int)
  (put 'raise '(rational) raise-rat)
  (put 'raise '(real) raise-real))

(define (raise x) (apply-generic 'raise x))

; 2.84
(define (apply-generic op . args)
  ; tests if one is higher than two
  (define (higher-than? one two)
    (define raise-two (get 'raise (list (type-tag two))))
    (cond ((null? raise-two) #f)
          ((= (type-tag one) (type-tag (raise-two two))) #t)
          (else (higher-than? one (raise-two two))))) ; recurse

  ;(define (highest-type-in-args) (car (sort args higher-than)))
  ; technically that was cheating as haven't done sort in sicp yet...
  (define (highest-type-in-args)
    (foldl (lambda (x acc)
             (if (or (null? acc) (higher-than? x acc))
               x
               acc)) '() args))

  ; Assumes that (raise-to from to-type) will only be called if from is
  ; raisable to to-type (true for its use here since confirmed by higher-than)
  (define (raise-to from to-type)
    (if (= (type-tag from) to-type)
      from
      (raise-to (raise from) to-type)))

  (define type-tags (map type-tag args))

  (define proc (get op type-tags))

  (define (alleqv? . args)
    (foldl (lambda (x acc)
             (and acc (eqv? x (car args))))
           #t args))

  ; Question specifies 'tower'. So can ignore case when everything needs to be
  ; raised beyond what they originally were, since in a tower, highest common
  ; parent node == highest type in args. So only one row in coercion table.
  (if proc
    (apply proc (map contents args))
    (if (apply alleqv? type-tags)
      (error "No method for these types" (list op type-tags))
      (begin
        (define to-type (highest-type-in-args))
        (define raise-fns
          (map (lambda (from)
                 (raise-to from to-type))
          type-tags))
        (define result
          (apply-generic op (map (lambda (raise-fn arg)
                                   (raise-fn arg))
                                 raise-fns args)))
        (or result (error "No method for these types"
                          (list op type-tags)))))))

; 2.85
(define (install-project-package)
  ; internal helper procedures (numer, denom, make-foo etc) are assumed to be
  ; available.  (in practice you'd just add these to individual packages etc to
  ; get the helpers).
  ; Also assume integer package exists.
  ; Also the scheme-number package is now called 'real' for some reason?
  (define (project-complex com)
    (make-scheme-number (real-part com)))
  (define (project-real real)
    ; feels a bit cheeky to use racket builtin rational helpers for this, but eh
    (define exact (inexact->exact real))
    (make-rational (numerator exact) (denominator exact)))
  (define (project-rational rational)
    (make-integer (round rational)))
  (put 'project '(complex) project-complex)
  (put 'project '(real) project-real)
  (put 'project '(rational) project-rational))

(define (project x) (apply-generic 'project x))

(define (drop object)
  (define proj-fn (get 'project (type-tag object)))
  (define projected
    (and proj-fn (proj-fn object)))
  ; so that (raise projected) won't be evaluated if projected is nil
  (if (and projected (equ? object (raise projected)))
    (drop projected)
    object))

; new version of apply-generic omitted - just wrap the result in a drop...

; 2.86

; Main change to the install-rectangular and install-polar packages are to
; make all internal operations able to cope with tagged data. So add rather
; than +, cosine rather than cos, etc. etc.
; All real & subsets numbers will have to implement new sine, cosine, arctan,
; and square root operations

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (square x) (mul x x))
  (define (magnitude z)
    (square-root (add (square (real-part z))
                      (square (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))
  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (square x) (mul x x))
  (define (make-from-real-imag x y)
    (cons (square-root (add (square x) (square y)))
          (arctan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'sine '(scheme-number)
       (lambda (x) (tag (sin x))))
  (put 'cosine '(scheme-number)
       (lambda (x) (tag (cos x))))
  (put 'arctan '(scheme-number)
       (lambda (x) (tag (atan x))))
  (put 'square-root '(scheme-number)
       (lambda (x) (tag (sqrt x))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (to-real x) (/ (numer x) (denom x)))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (square-root-rat x)
    (make-rat (sqrt (numer x))
              (sqrt (denom x))))
  ; The sine function returns a real - nothing says
  ; these operations have to have closure
  (define (sine-rat x)
    (make-scheme-number (sin (to-real x))))
  (define (cosine-rat x)
    (make-scheme-number (cos (to-real x))))
  (define (arctan-rat x)
    (make-scheme-number (atan (to-real x))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'square-root '(rational)
       (lambda (x) (tag (square-root-rat x))))
  (put 'sine '(rational)
       (lambda (x) (tag (sine-rat x))))
  (put 'cosine '(rational)
       (lambda (x) (tag (cosine-rat x))))
  (put 'arctan '(rational)
       (lambda (x) (tag (arctan-rat x))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arctan x) (apply-generic 'arctan x))
(define (square-root x) (apply-generic 'square-root x))


; 2.87
; ok, enough coding blind, this is bullshit. I want to be able to actually run
; things. Previous few exercises were probably riddled with syntax errors.
; Googled some get and put helpers for racket (using make-hash), will be
; implementing the number tower and polynomial exercises in full in 2-5-polynomials.rkt.

; My =zero? relies on equ?, so should just work once these are implemented:
; should work for integer, rational, and real, as = compares across those types
(define (equ-poly-num poly num)
  (define terms (term-list poly))
  (if (= num 0)
    (null? terms)
    (and (= 1 (length terms))
         (= 0 (order (car terms)))
         (= num (coeff (car terms))))))

(put 'equ? '(polynomial scheme-number)
     equ-poly-num)
(put 'equ? '(scheme-number polynomial)
     (swap-args equ-poly-num))
(put 'equ? '(polynomial rational)
     equ-poly-num)
(put 'equ? '(rational polynomial)
     (swap-args equ-poly-num))
(put 'equ? '(polynomial integer)
     equ-poly-num)
(put 'equ? '(integer polynomial)
     (swap-args equ-poly-num))

; 2.88
(define (sub-poly p1 p2)
  (if (same-variable? (variable p1)
                      (variable p2))
    (make-poly
      (variable p1)
      (sub-terms (term-list p1)
                 (term-list p2)))
    (error "Polys not in same var: SUB-POLY"
           (list p1 p2))))

(define (sub-terms L1 L2)
  (add-terms L1 (negate-terms L2)))

(define (negate-terms L)
  ; Turns out racket has pattern-matching! First term in the
  ; body array is the match expr, second is the output
  (map (match-lambda [(list order coeff)
                      (list order (- coeff))])
       L))


; 2.89
(define (adjoin-term term term-list)
  ; The length of the term-list is 1 plus the
  ; order of the highest-order term currently in it
  (cond ((= (order term) (length term-list))
         (cons (coeff term) term-list))
        ((> (order term) (length term-list))
         (adjoin-term term (cons 0 term-list)))
        (else (error "adjoining term must be higher order than already here"))))

; can no longer just car a term off, since a term-list is no longer a list of terms
(define (first-term term-list)
  (make-term (- (length term-list) 1)
             (car term-list)))
(define (rest-terms term-list) (cdr term-list))

(define (negate-terms L)
  (map (lambda (coeff) (- coeff))
       L))

; 2.90
; Implemented in 2-5-polynomials.rkt
; using the get/put system, but avoiding raising or dropping.
; Mixed operations all convert dense to sparse (as sparse is at worst only a
; constant factor less efficient than dense)

; 2.91
(define (div-poly dividend divisor)
  (cond ((same-variable? (variable dividend)
                         (variable divisor))
         (define result
           (div-terms (term-list dividend)
                      (term-list divisor)))
         (define quotient (car result))
         (define remainder (cdr result))
         (define var (variable dividend))
         (list (make-poly var quotient)
               (make-poly var remainder)))
        (else (error "Polys not in same var: DIV-POLY" (list dividend divisor)))))

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
    (list (the-empty-termlist)
          (the-empty-termlist))
    (let ((t1 (first-term L1))
          (t2 (first-term L2)))
      (if (> (order t2) (order t1))
        (list (the-empty-termlist) L1)
        (let ((new-c (div (coeff t1)
                          (coeff t2)))
              (new-o (- (order t1)
                        (order t2))))
          (let ((rest-of-result
                  (div-terms (sub-terms L1
                                        (mul-terms L2
                                                   (list (make-term new-o
                                                                    new-c))))
                             L2)))
            (let ((quotient (cons (make-term new-o new-c)
                                  (car rest-of-result)))
                  (remainder (cdr rest-of-result)))
              (cons quotient remainder))))))))

; 2.92

; As well as the below, needed some changes to type dispatching for this to
; work: needed raise-to to raise numbers to polynomials of the 'right'
; variable. So special-cases polynomials, which is a bit messy. See
; polynomials.rkt

; Defines an arbitrary variable ordering to get
; consistency in polynomial variable coercion.
; symbol<? uses unicode codepoint order
(define (choose-variable a b)
  (if (symbol<? a b)
      a
      b))

(define (coerce-to-var poly var)
  (if (equal? var (variable poly))
      poly
      (make-poly var (list (list 0 (tag poly))))))

(define (add-poly p1 p2)
  (cond ((same-variable? (variable p1)
                         (variable p2))
         (make-poly
          (variable p1)
          (add-terms (term-list p1)
                     (term-list p2))))
        (else
         (define final-var
           (choose-variable (variable p1)
                            (variable p2)))
         (make-poly
          final-var
          (add-terms (term-list (coerce-to-var p1 final-var))
                     (term-list (coerce-to-var p2 final-var)))))))


(define (mul-poly p1 p2)
  (cond ((same-variable? (variable p1)
                         (variable p2))
         (make-poly
           (variable p1)
           (mul-terms (term-list p1)
                      (term-list p2))))
        (else
          (define final-var
            (choose-variable (variable p1)
                             (variable p2)))
          (make-poly
            final-var
            (mul-terms (term-list (coerce-to-var p1 final-var))
                       (term-list (coerce-to-var p2 final-var)))))))

; 2.93
; see 2-5-polynomials.rkt for modified rat package

> (add rf rf)
'(rational (sparse-poly x (5 2) (3 2) (2 2) (0 2)) sparse-poly x (4 1) (2 2) (0 1))

; 2.94

(define (remainder-terms dividend divisor)
  (cadr (div-terms dividend divisor)))

(define (gcd-terms a b)
  (if (empty-termlist? b)
    a
    (gcd-terms b (remainder-terms a b))))

(define (gcd-poly p1 p2)
  (if (same-variable? (variable p1)
                      (variable p2))
    (make-poly
      (variable p1)
      (gcd-terms (term-list p1)
                 (term-list p2)))
    (error "Polys not in same var: GCD-POLY"
           (list p1 p2))))

; > (greatest-common-divisor p1 p2)
; '(x (2 -1) (1 1))

; This is correct, x^3 - 1 = -(x - x^2)(1 + x)

; 2.95
(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (0 7))))
(define p3 (make-polynomial 'x '((1 13) (0 5))))
(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

;> q1
;'(sparse-poly x (4 11) (3 -22) (2 18) (1 -14) (0 7))
;> q2
;'(sparse-poly x (3 13) (2 -21) (1 3) (0 5))

;(greatest-common-divisor q1 q2) ; =>
;'(gcd-terms ((4 11) (3 -22) (2 18) (1 -14) (0 7)) ((3 13) (2 -21) (1 3) (0 5)))
;'(div-terms ((4 11) (3 -22) (2 18) (1 -14) (0 7)) ((3 13) (2 -21) (1 3) (0 5)))
;'(div-terms ((4 -2) (3 -1) (2 15) (1 -19) (0 7)) ((3 13) (2 -21) (1 3) (0 5)))
;'(div-terms ((4 -2) (3 -1) (2 15) (1 -19) (0 7)) ((3 13) (2 -21) (1 3) (0 5)))
; ...

; An infinite loop. div-terms is calculating the new coefficient of each term as
(div (coeff t1) (coeff t2))
; which, as t1 and t2 are racket native integers, returns an integer (ie any
; fractional part truncated).

; 2.96
; 1
(define (pseudoremainder-terms dividend divisor)
  (define integerizing-factor
    (expt (coeff (car divisor))
          (+ 1 (order (car dividend)) (- (order (car divisor))))))
  (define new-dividend
    (mul-terms dividend `((0 ,integerizing-factor))))
  (cadr (div-terms new-dividend divisor)))

;> (greatest-common-divisor q1 q2)
;'(sparse-poly x (2 1458) (1 -2916) (0 1458))

; 2
(define (extract-coeffs term-list)
  (map (match-lambda [(list order coeff) coeff])
       term-list))

(define (remove-common-factors-terms poly)
  (define hcf (apply gcd (extract-coeffs poly)))
  (displayln `(removing common factor ,hcf from ,poly result: ,(div-terms poly `((0 ,hcf)))))
  (div-terms poly `((0 ,hcf))))

(define (gcd-terms a b)
  (if (empty-termlist? b)
    (remove-common-factors-terms a)
    (gcd-terms b (pseudoremainder-terms a b))))

;> (greatest-common-divisor q1 q2)
;'(sparse-poly x ((2 1) (1 -2) (0 1)) ())

; 2.97
(define (reduce-terms n d)
  (define gcd (gcd-terms n d))
  (define integerizing-factor
    (expt (coeff (car gcd))
          (+ 1 (max (order (car d))
                    (order (car n)))
             (- (order (car gcd))))))
  (define if-term `((0 ,integerizing-factor)))
  (list (quotient-from-div-terms (div-terms n if-term))
        (quotient-from-div-terms (div-terms d if-term))))

(define (reduce-poly p1 p2)
  (if (same-variable? (variable p1)
                      (variable p2))
    (make-poly
      (variable p1)
      (reduce-terms (term-list p1)
                    (term-list p2)))
    (error "Polys not in same var: REDUCE-POLY"
           (list p1 p2))))


