(define numbered?
  (lambda (axep)
    (cond
      ((atom? axep)(number? axep))
      ((eq? (car (cdr axep)) (quote +))
        (and (numbered? (car axep)) (numbered? (car (cdr (cdr axep)))))
      )
      ((eq? (car (cdr axep)) (quote *))
      (and (numbered? (car axep)) (numbered? (car (cdr (cdr axep)))))
      )
      ((eq? (car (cdr axep)) (quote ^))
      (and (numbered? (car axep)) (numbered? (car (cdr (cdr axep)))))
      )
    )
  )
)

(define numbered?
  (lambda (axep)
    (cond
      ((atom? axep) (number? axep))
      (else
        (and (numbered? (car axep)) (numbered? (car (cdr (cdr axep)))))
      )
    )
  )
)

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) (quote +)) (+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote *)) (* (value (car nexp)) (value (car (cdr (cdr nexp))))))
      (else (^ (value (car nexp)) (value (car (cdr (cdr nexp))))))
    )
  )
)