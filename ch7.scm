(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)))
      (else (set? (cdr lat)))
    )
  )
)

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat))))
    )
  )
)

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else
        (cons (car lat) (makeset (rember* (car lat) (cdr lat))))
      )
    )
  )
)

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (and (member? (car set1) set2) (subset? (cdr set1) set2))
    )
  )
)

(define eqset?
  (lambda (set1 set2))
  (and (subset? set1 set2) (subset? set2 set1))
)

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (or (member? (car set1) set2) (intersect? (cdr set1) set2))
    )
  )
)

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else
        (intersect (cdr set1) set2)
      )
    )
  )
)

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else
        (cons (car set1) (union (cdr set1) set2))
      )
    )
  )
)

(define xxx
  (lambda (set1 set2))
  (cond
    ((null? set1)(quote ()))
    ((member? (car set1) set1) (xxx (cdr set1) set2))
    (else
      (cons (car set1) (xxx (cdr set1) set2))
    )
  )
)

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else
        (intersect (car l-set) (intersectall (cdr l-set)))
      )
    )
  )
)

(define a-pair?
  (lambda (x)
    (cond
      ((null? x) #f)
      ((atom? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f)
    )
  )
)

(define first
  (lambda (p)
    (car p)
  )
)

(define second
  (lambda (p)
    (car (cdr p))
  )
)

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))
  )
)

(define fun?
  (lambda (rel)
    (set? (first rel))
  )
)

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      (else
        (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel)))
      )
    )
  )
)

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))
  )
)

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      (else (cons (revpair (car rel)) (revrel (cdr rel))))
    )
  )
)

(define firsts
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      (else
        (cons (first (car rel)) (firsts (cdr rel)))
      )
    )
  )
)

(define seconds
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      (else
        (cons (second (car rel)) (seconds (cdr rel)))
      )
    )
  )
)

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))
  )
)

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))
  )
)