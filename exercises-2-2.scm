; 2.17
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

; 2.18
(define (reverse l)
  (if (= 1 (length l))
      l
      (append (reverse (cdr l))
              (list (car l)))))

; or better still, handle length 0 lists
; (needed for count-change):
(define (reverse l)
  (if (= 0 (length l))
      '()
      (append (reverse (cdr l))
              (list (car l)))))

; 2.19
(define (first-denomination coin-values)
  (car (last-pair coin-values)))

(define (except-first-denomination coin-values)
  (reverse (cdr (reverse coin-values))))

(define (no-more? coin-values)
  (= 0 (length coin-values)))

; Order shouldn't affect it, no
; though might affect efficiency

; 2.20
(define (same-parity x . y)
  (define (same-parity-helper x . y)
  (if (= 0 (length y))
      null
  (if (= (modulo x 2)
         (modulo (car y) 2))
      (cons (car y)
            (apply same-parity-helper x (cdr y)))
      (apply same-parity-helper x (cdr y)))))
  (cons x (apply same-parity-helper x y)))

; will's suggestion: signature of same-parity-helper
; can just be (x y), so you don't have to use apply
