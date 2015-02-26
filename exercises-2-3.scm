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
; ''abracadabra evaluates to '(quote abracadabra)

