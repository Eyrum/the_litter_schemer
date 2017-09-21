(define (list-length lst)
  (if (null? lst)
    0
    (+ 1 (list-length (cdr lst)))))