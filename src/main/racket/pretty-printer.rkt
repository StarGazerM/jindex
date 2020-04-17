;; some display function and pretty-printer

;; Yihao Sun <ysun67@syr.edu>

#lang racket

;; format a point to relation map into JSON
(define (format-point2rel/json pt)
  (format "{~a}"
          (string-join
           (reverse
            (for/fold ([res '()])
                      ([from (in-list (hash-keys pt))]
                       [to (in-list (hash-values pt))])
              (cons (format "\"~a\" : \"~a\"" from to) res)))
           ", ")))
