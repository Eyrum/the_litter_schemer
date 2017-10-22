(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) (quote ()))
      (else
        ((test? (car l) a) (cdr l))
        (else
          (cons (car l) (rember-f (test? a (cdr l))))
        )
      )
    )
  )
)

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a)
    )
  )
)

; function x (a) {return function(x) {return a === x}}
; var s = x(2)
; s(2)

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote()))
        ((test? (car l) a) (cdr l))
        (else
          (cons (car l) ((rember-f test?) a (cdr l)))
        )
      )
    )
  )
)

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l)(quote()))
        ((test? (car l) old)(cons new (cons old (cdr l))))
        (else
          (cons (car l) ((insertL-f test?) new old (cdr l)))
        )
      )
    )
  )
)

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l)(quote()))
        ((test? (car l) old)(cons old (cons new (cdr l))))
        (else
          (cons (car l) ((insertR-f test?) new old (cdr l)))
        )
      )
    )
  )
)

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat)(quote()))
      ((eq? (car lat) a)(multirember a (cdr lat)))
      (else
        (cons (car lat)(multirember a (cdr lat)))
      )
    )
  )
)

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote()))
        ((test? (car lat) a)((multirember-f test?) a (cdr lat)))
        (else
          (cons (car lat) ((multirember-f test?) a (cdr lat)))
        )
      )
    )
  )
)

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? (lat))(quote()))
      ((test? (car lat))(multiremberT test? (cdr lat)))
      (else
        (cons (car lat) (multiremberT test? (cdr lat)))
      )
    )
  )
)

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) oldL)(cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)(cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else
        (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))
      )
    )
  )
)