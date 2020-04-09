;; This is Lens util function can look into a nested list stucture!

;; Yihao Sun<ysun67@syr.edu>

#lang racket

;; look up a data structure by predicate
;; (listof Any? data) →  (listof predicate?) → (listof Any? subdata)
(define (lens-look data prs)
  (match prs
    ['() '()]
    [`(,hd . ,rst)
     (append-map (λ (sd) (lens-look sd rst)) (filter hd  data))]))

(define (lens-mod data prs mod)
  (match prs
    ['() (map mod data)]
    [`(,hd . ,rst)
     (foldl (λ (sd)
              (match sd
                [(? hd) (lens-mod sd rst mod)]
                [else sd]))
            '()
            data)]))
