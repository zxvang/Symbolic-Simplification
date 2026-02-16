#lang racket

(define (fold-constants expr)
  (if (or (not (list? expr)) (not (and (number? (cadr expr)) (number? (caddr expr)))))
      expr
      (let ([op (car expr)] [left (cadr expr)] [right (caddr expr)])
        (cond [(eq? op '+) (+ left right)]
              [(eq? op '-) (- left right)]
              [(eq? op '*) (* left right)]
              [(and (eq? op '/) (not (zero? right)))
               (if (integer? (/ left right)) (exact-floor (/ left right)) (/ left right))]
              [else expr]))))

(define (reduce-identities expr)
  (if (not (list? expr))
      expr
      (let ([op (car expr)] [left (cadr expr)] [right (caddr expr)])
        (cond [(and (eq? op '+) (number? left) (zero? left)) right]
              [(and (eq? op '+) (number? right) (zero? right)) left]
              [(and (eq? op '+) (list? left) (list? right)
                    (eq? (car left) '*) (eq? (car right) '*)
                    (equal? (caddr left) (caddr right))
                    (number? (cadr left)) (number? (cadr right)))
               (list '* (+ (cadr left) (cadr right)) (caddr left))]
              [(and (eq? op '-) (number? right) (zero? right)) left]
              [(and (eq? op '-) (list? right) (eq? (car right) '-)
                    (number? (cadr right)) (zero? (cadr right)))
               (caddr right)]
              [(and (eq? op '*) (number? left) (zero? left)) 0]
              [(and (eq? op '*) (number? right) (zero? right)) 0]
              [(and (eq? op '*) (number? left) (= left 1)) right]
              [(and (eq? op '*) (number? right) (= right 1)) left]
              [(and (eq? op '/) (number? right) (= right 1)) left]
              [else expr]))))

(define (simplify-expression expr)
  (if (not (list? expr))
      expr
      (let* ([op (car expr)] [left (cadr expr)] [right (caddr expr)]
             [left-simplified (simplify-expression left)]
             [right-simplified (simplify-expression right)]
             [new-expr (list op left-simplified right-simplified)]
             [folded (fold-constants new-expr)]
             [reduced (reduce-identities folded)])
        (if (equal? reduced expr) reduced (simplify-expression reduced)))))

(provide simplify-expression)

(module+ test
  (displayln (simplify-expression '(+ x 0)))
  (displayln (simplify-expression '(+ 5 (+ 10 2))))
  (displayln (simplify-expression '(+ (* 1 x) (* y 0))))
  (displayln (simplify-expression '(+ 0 (+ 0 (+ 0 z)))))
  (displayln (simplify-expression '(* (+ 1 0) (+ x 0))))
  (displayln (simplify-expression '(+ (* 1 x) (+ 0 y))))
  (displayln (simplify-expression '(- 20 (/ (+ (* 4 5)(* 2 5))(- 8 2)))))
  (displayln (simplify-expression '(+ (* 2 x) (* 3 x))))
  (displayln (simplify-expression '(+ (* 2 x) (* 3 y)))))