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