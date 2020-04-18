;; a racket sever will communicate with Java program and abstract machine

;; Yihao Sun <ysun67@syr.edu>

#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/dispatch
         web-server/http/json
         json
         "ir.rkt"
         "machine.rkt"
         "pretty-printer.rkt")

;; for test
(define (default req)
  (response/jsexpr "hello world"))

(define (analyze req)
  (let*-values
      ([(jdata) (string->jsexpr
                 (bytes->string/utf-8
                  (request-post-data/raw req)))]
       [(k-value) 0]
       [(pt-map cfg)
        (run (read (open-input-string (hash-ref jdata 'ir)))
             `(,(string->number (hash-ref jdata 'startx))
               ,(string->number (hash-ref jdata 'starty))))]
       [(res) (hash 'point-to-map (format-point2rel pt-map)
                    'CFG (format-stream cfg))])
    (response/jsexpr res)))

(define (not-found req)
  (response #:code 404
            #:message "Not Found"))

(define-values (routes _)
  (dispatch-rules
   [("") default]
   [("analyze") #:method "post" analyze]
   [else not-found]))

(serve/servlet routes
               #:port 8081
               #:servlet-regexp #rx"")

