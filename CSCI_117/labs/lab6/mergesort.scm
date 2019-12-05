(define (split x)
    (cond   ((null? x) (cons '() '()))
            ((null? (cdr x)) (cons x '()))
            (else 
                (cons (cons (car x) (car (split (cddr x)))) (cons (cadr x) (cdr (split (cddr x)))))
            )
    )
)

(define (merge x y)
    (cond   ((null? x) y)
            ((null? y) x)
            (else   
                (if (<= (car x) (car y))
                    (cons (car x) (merge (cdr x) y))
                    (cons (car y) (merge x (cdr y)))
                )
            )
        
    )
)

(define (merge_sort x)
    (cond   ((null? x) x)
            ((null? (cdr x)) x)
            (else 
                (merge (merge_sort (car (split x))) (merge_sort (cdr (split x))))
            )
    )
)