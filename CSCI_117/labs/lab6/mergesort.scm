(define (split x)
    (cond   ((null? x) (cons '() '()))
            ((null? (cdr x)) (cons x '()))
            (else ((1)))
    )
)

(define (test x)
    (+ x 3))