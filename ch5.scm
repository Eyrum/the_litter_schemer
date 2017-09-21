(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (rember* a (cdr l)))
         (else
           (cons (car l) (rember* a (cdr l))))))
      (else
        (cons (rember* a (car l)) (rember* a (cdr l)))))))

(rember*
  'cup
  '((coffee) cup ((tea) cup) (and (hick)) cup))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
        (cond
          ((eq? (car l) old) (cons old (cons new (insertR* new old (car l)))))
          (else (cons (car l) (insertR* new old (cdr l))))
        )
      )
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l))))
    )
  )
)

(define add1
  (lambda (n)
    (+ n 1)
  )
)

(define sub1
  (lambda (n)
    (- n 1)
  )
)

(define plus
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (plus n (sub1 m))))
    )
  )
)

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
        (cond
          ((eq? (car l) a) (add1 (occur* a (cdr l))))
          (else (occur* a (cdr l)))
        )
      )
      (else
        (plus (occur* a (car l)) (occur* a (cdr l)))
      )
    )
  )
)

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
        (cond
          ((eq? (car l) old)
            (cons new (subst* new old (cdr l)))
            (else cons (car l) (subst* new old (cdr l)))
          )
        )
      )
      (else (cons (subst* new old (car l)) (subst* new old (cdr l))))
    )
  )
)

(define inserL*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
        (cond
          ((eq? (car l) old)
            (cons new (cons old (inserL* new old (cdr l))))
          )
          (else
            (cons (car l) (inserL* new old (cdr l)))
          )
        )
      )
      (else
        (cons (inserL* new old (car l)) (inserL* new old (cdr l)))
      )
    )
  )
)

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
        (or (eq? (car l) a) (member* a (cdr l)))
      )
      (else
        (or (member* a (car l)) (member* a (cdr l)))
      )
    )
  )
)

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else
        (leftmost (car l))
      )
    )
  )
)

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1) (car l2)))
        (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1)(cdr l2)))
      )
      ((or (atom? (car l1) (car l2))) #f)
      (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
    )
  )
)

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
        (eqan? s1 s2)
      )
      ((or (atom? s1) (atom? s2)) #f)
      (else
        (eqlist? s1 s2)
      )
    )
  )
)

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
    )
  )
)

(define rember
  (lambda (s l)
    (cond
      ((null? l) (quote()))
      ((equal? (car l) s) (cdr l))
      (else
        (cons (car l) (rember s (cdr l)))
      )
    )
  )
)