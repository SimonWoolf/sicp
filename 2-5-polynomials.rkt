#lang racket

; Contains full implementation of number tower (integer, rational, real, complex),
; using racket builtins for integer and real, and custom types for rational and complex

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETTING UP TYPE SYSTEM ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/flonum)
(define machine-epsilon
  (let loop ([n 1.0])
    (define next-n (fl/ n 2.0))
    (if (fl= 1.0 (fl+ 1.0 next-n))
      n
      (loop next-n))))

(define *op-table* (make-hash))
(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))
(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(define (attach-tag type-tag contents)
  (cond ((equal? type-tag 'scheme-number)
         contents)
        ((equal? type-tag 'integer)
         contents)
        (else
          (cons type-tag contents))))

(define (type-tag datum)
  (cond ((pair? datum)
         (car datum))
        ((exact-integer? datum)
         'integer)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     APPLY-GENERIC      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (apply-generic op . args)
  (displayln (list "apply-generic" op args))

  (define (conditional-drop result)
    (if (memq op '(raise project equ? make))
      result
      (drop result)))

  ; tests if one is higher than two
  (define (higher-than? one two)
    (define raise-two (get 'raise (list (type-tag two))))
    (cond ((null? raise-two) #f)
          ((eq? (type-tag one) (type-tag (raise-two (contents two)))) #t)
          (else (higher-than? one (raise-two (contents two)))))) ; recurse

  ;(define (highest-type-in-args) (car (sort args higher-than)))
  ; technically that was cheating as haven't done sort in sicp yet...
  (define (highest-type-in-args)
    (type-tag (foldl (lambda (x acc)
                       (if (or (null? acc) (higher-than? x acc))
                         x
                         acc)) '() args)))

  ; Assumes that (raise-to from to-type) will only be called if from is
  ; raisable to to-type (true for its use here since confirmed by higher-than)
  (define (raise-to from to-type)
    (if (eq? (type-tag from) to-type)
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
  (if (not (null? proc))
    (conditional-drop (apply proc (map contents args)))
    (cond ((apply alleqv? type-tags)
           (error "No method for these types" (list op type-tags)))
          (else
            (define to-type (highest-type-in-args))
            (define raised-args
              (map (lambda (from)
                     (raise-to from to-type))
                   args))
            (define result
              (apply apply-generic (cons op raised-args)))
            (if (null? result)
              (error "No method for these types" (list op type-tags))
              (conditional-drop result))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (make x) (apply-generic 'make x))
(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    RAISING/DROPPING    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-raise-package)
  (define numer car)
  (define denom cdr)
  (define (raise-int int)
    (make-rational int 1))
  (define (raise-rat rat)
    (make-scheme-number (/ (numer rat) (denom rat))))
  (define (raise-real real)
    (make-complex-from-real-imag real 0))
  (put 'raise '(integer) raise-int)
  (put 'raise '(rational) raise-rat)
  (put 'raise '(scheme-number) raise-real))

(define (raise x) (apply-generic 'raise x))

(define (install-project-package)
  (define numer car)
  (define denom cdr)
  (define (project-complex com)
    (make-scheme-number (real-part com)))
  (define (project-real real)
    ; feels a bit cheeky to use racket builtin rational helpers for this, but eh
    (define exact (inexact->exact real))
    (make-rational (numerator exact) (denominator exact)))
  (define (project-rational rational)
    (make-integer (round (/ (numer rational) (denom rational)))))
  (put 'project '(complex) project-complex)
  (put 'project '(scheme-number) project-real)
  (put 'project '(rational) project-rational))

(define (project x) (apply-generic 'project x))

(define (drop object)
  (define proj-fn (get 'project (list (type-tag object))))
  (define projected
    (and (not (null? proj-fn)) (proj-fn (contents object))))
  ; so that (raise projected) won't be evaluated if projected is #f
  (if (and projected (equ? object (raise projected)))
    (drop projected)
    object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        COMPLEX         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (square x) (* x x))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
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
  (define (square x) (* x x))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
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

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag
        'rectangular)
   x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang
        'polar)
   r a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   INTEGERS / REALS     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (using scheme builtins)

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
  (put 'make 'scheme-number
       ; exact->inexact to prevent confusion with
       ; rationals/integers -- 'scheme number' here = real
       (lambda (x) (tag (exact->inexact x))))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; actually just uses racket exact integers
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (round (/ x y)))))
  (put 'make 'integer
       (lambda (x) (tag (inexact->exact (round x)))))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       RATIONALS        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-rational-package)
  (define numer car)
  (define denom cdr)
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
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        EQUALITY        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-equality-package)
  (define numer car)
  (define denom cdr)
  (define (swap-args fun)
    (lambda (a b) (fun b a)))
  ; I'm assuming a machine-epsilon global is defined.
  ; It's about 2.22e-16 for racket floats
  (define (approx-eq x y)
    (< (abs (- x y))
       machine-epsilon))
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
               (/ (numer rat) (denom rat))))
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'equ? '(integer integer) =)
  (put 'equ? '(scheme-number integer) =)
  (put 'equ? '(integer scheme-number) =)
  (put 'equ? '(rational rational) equ-rat)
  (put 'equ? '(complex complex) equ-com)
  (put 'equ? '(complex scheme-number) equ-com-num)
  (put 'equ? '(scheme-number complex) (swap-args equ-com-num))
  (put 'equ? '(rational scheme-number) equ-rat-num)
  (put 'equ? '(scheme-number rational) (swap-args equ-rat-num))
  (put 'equ? '(complex integer) equ-com-num)
  (put 'equ? '(integer complex) (swap-args equ-com-num))
  (put 'equ? '(rational integer) equ-rat-num)
  (put 'equ? '(integer rational) (swap-args equ-rat-num))
  (put 'equ? '(rational complex) equ-rat-com)
  (put 'equ? '(complex rational) (swap-args equ-rat-com)))

(define (equ? a b) (apply-generic 'equ? a b))

(define (=zero? a) (equ? a 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      POLYNOMIALS       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-polynomial-package)
  (define (swap-args fun)
    (lambda (a b) (fun b a)))
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define variable? symbol?)
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))

  ; needed for =zero? check
  ; should work for integer, rational, and
  ; real, as = compares across those types
  (define (equ-poly-num poly num)
    (define terms (term-list poly))
    (if (= num 0)
      (null? terms)
      (and (= 1 (length terms))
           (= 0 (order (first-term terms)))
           (= num (coeff (first-term terms))))))

  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list)
    (null? term-list))
  (define (make-term order coeff)
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1
                       (add-terms (rest-terms L1)
                                  L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2
                       (add-terms
                         L1
                         (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term
                          (order t1)
                          (add (coeff t1)
                               (coeff t2)))
                        (add-terms
                          (rest-terms L1)
                          (rest-terms L2)))))))))

  (define (negate-terms L)
    ; Turns out racket has pattern-matching! First term in the body array is
    ; the match expr, second is the output
    (map (match-lambda [(list order coeff)
                        (list order (- coeff))])
         L))

  (define (sub-terms L1 L2)
    (add-terms L1 (negate-terms L2)))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms
        (mul-term-by-all-terms
          (first-term L1) L2)
        (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms
            t1
            (rest-terms L))))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
      (make-poly
        (variable p1)
        (add-terms (term-list p1)
                   (term-list p2)))
      (error "Polys not in same var:
             ADD-POLY"
             (list p1 p2))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
      (make-poly
        (variable p1)
        (sub-terms (term-list p1)
                   (term-list p2)))
      (error "Polys not in same var: SUB-POLY"
             (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
      (make-poly
        (variable p1)
        (mul-terms (term-list p1)
                   (term-list p2)))
      (error "Polys not in same var:
             MUL-POLY"
             (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))
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
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    INSTALL PACKAGES    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(install-project-package)
(install-scheme-number-package)
(install-integer-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-equality-package)
(install-polynomial-package)
(install-raise-package)
