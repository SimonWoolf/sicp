; 2.53
(list 'a 'b 'c) => '(a b c)
(list (list 'george)) => '((george))
(cdr '((x1 x2) (y1 y2))) => '((y1 y2))
(cadr '((x1 x2) (y1 y2))) => '(y1 y2)
(pair? (car '(a short list))) => #f
(memq 'red '((red shoes) (blue socks))) => #f
(memq 'red '(red shoes blue socks)) => '(red shoes blue socks)

; 2.54
(define (my-equal? a b)
  (cond
    ((and (number? a) (number? b)) (= a b))
    ((and (symbol? a) (symbol? b)) (eq? a b))
    ; need or rather than and for null else if one is
    ; null and the other isn't it'll be caught by the
    ; list branch and try to call car on it
    ((or (null? a) (null? b)) (eq? a b))
    ((and (list? a) (list? b))
     (and (my-equal? (car a) (car b))
          (my-equal? (cdr a) (cdr b))))
    (else #f)
    ))

; Or using an all? helper I just wrote:
(define (all? proc list)
  (if
    (null? list)
    #t
    (and (proc (car list))
         (all? proc (cdr list)))
    ))

(define (any? proc list)
  (if
    (null? list)
    #f
    (or (proc (car list))
        (all? proc (cdr list)))
    ))

(define (my-equal? a b)
  (cond
    ((all? number? (list a b)) (= a b))
    ((all? symbol? (list a b)) (eq? a b))
    ; need any? rather than all? for null else if one is
    ; null and the other isn't it'll be caught by the
    ; list branch and try to call car on it
    ((any? null? (list a b)) (eq? a b))
    ((all? list? (list a b))
     (and (my-equal? (car a) (car b))
          (my-equal? (cdr a) (cdr b))))
    (else #f)
    ))

; 2.55
; ''abracadabra evaluates to '(quote abracadabra) = ('quote 'abracadabra)

; 2.56
((exponential? exp)
 (make-product (exponent exp)
               (make-exponential
                 (base exp)
                 (- (exponent exp) 1))))

(define (exponential? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))

(define (make-exponential base exponent)
  (cond
    ((not (number? exponent))
     (error "Can't do non-numerical exponents"))
    ((= exponent 0) 1)
    ((= exponent 1) base)
    (else (list '** base exponent))))

; 2.57

(define (make-sum . addends)
  (let ((sum-numeric-addends (foldr + 0 (filter number? addends)))
        (non-numeric-addends (filter (lambda (x) (not (number? x))) addends)))
    (define net-addends (if (= 0 sum-numeric-addends)
                          non-numeric-addends
                          (cons sum-numeric-addends non-numeric-addends)))
    (if (null? (cdr net-addends)) ; only one item
      (car net-addends)
      (cons '+ net-addends))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (exponential? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))

(define (make-exponential base exponent)
  (cond
    ((not (number? exponent))
     (error "Can't do non-numerical exponents"))
    ((= exponent 0) 1)
    ((= exponent 1) base)
    (else (list '** base exponent))))
