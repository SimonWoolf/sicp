; 2.73
;
; 1. numbers are lisp primitives, variables are just the symbol of the variable name - neither have type tags lookable up by get?. (Well, number does have a type, but a builtin one, not part of our constructed type system)
;
; 2.
; This assums that relevant helpers (make-sum, etc) are in the global
; namespace, since the question did too
(define (install-deriv-sum)
  (define (deriv-sum expr var)
   (make-sum (deriv (addend expr) var)
             (deriv (augend expr) var)))
  (put 'deriv '(sum) deriv-sum))

(define (install-deriv-product)
  (define (deriv-product expr var)
   (make-sum
     (make-product
       (multiplier expr)
       (deriv (multiplicand expr) var))
     (make-product
       (deriv (multiplier expr) var)
       (multiplicand expr))))
  (put 'deriv '(product) deriv-product))

; 3.

(define (install-deriv-exp)
  (define (deriv-exp expr var)
    (make-product (exponent expr)
                  (make-exponential
                    (base expr)
                    (- (exponent expr) 1)))
  (put 'deriv '(exp) deriv-exp)))

; 4.
; You only need to swap the order equivalently in 'put'. ie row and column are
; switched in your dispatch table. But then the result will still have a type
; 'deriv',  so will be harder to do stuff with it...

; 2.74
; 1.
(define (get-record file employee-name)
  (define division (type-tag file))
  ((get 'record division) (contents file) employee-name))

; file should be (attach-tag type-tag existing-file) where type-tag is a symbol
; unique to that file's layout

; 2.
(define (get-salary file employee-name)
  (define division (type-tag file))
  (define record (get-record file employee-name))
  ((get 'salary division) record))

; In this scheme, each record is not tagged with a separate type, it just uses
; the overall division type (to avoid having to edit every record). Which is
; fine as long as all the records in a division are structured the same.

; 3.
(define (find-employee-record files employee-name)
  (foldl (lambda (file acc)
           (if (not (null? acc))
             acc
             (get-record file employee-name))
           ) '() files))

; 4.
; Assuming new company uses scheme records in a similar way to Insatiable's
; divisions, they'll just have to use put to add new types for their records
; with appropriate operations

; 2.75
(define (make-from-mag-ang r theta)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos theta)))
          ((eq? op 'imag-part) (* r (sin theta)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) theta)
          (else
            (error "Unknown op:
                   MAKE-FROM-MAG-ANG" op))))
    dispatch)

; 2.76
; For generic operations with explicit dispatch:
; - adding new types: hard - need to edit every operation to add a new clause to the cond
; - adding new operations: easy - need that big cond clause, but no editing existing things. (Although: need to worry about naming conflicts)
;
; For data-directed style:
; - adding new types: easy - need to add a single new 'install' function which defines every operation
; - adding new operations: in theory easy (can make a new install procedure that puts that new op for every type into the table), but in practice will want to reference the helper functions in the original install procedure for each type, so will probably want to edit those and run them again. But then, that's also true of generic operations, so for consistency: easy.
;
; For message-passing style:
; - adding new types: easy - need a create a new object which implements all existing operations
; - adding new operations: hard - need to edit every existing operation
;
; For a system when new types must often be added: message-passing (or maybe data-directed). For a system when new operations must often be added, data-directed (or maybe explicit dispatch).
