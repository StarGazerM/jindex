;; some display function and pretty-printer

;; Yihao Sun <ysun67@syr.edu>

#lang racket

(provide (all-defined-out))

;; format a point to relation map into JSON
(define (format-point2rel pt)
  (for/fold ([res '()])
            ([from (in-list (hash-keys pt))]
             [to (in-list (hash-values pt))])
    (cons `(,(format "~s" from) ,(format "~s" to)) res)))

(define (format-stream s)
  (map (λ (p)
         (match p
           [`(,from ,to)
            `(,(format "~s" from) ,(format "~s" to))]))
       s))
