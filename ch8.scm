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
