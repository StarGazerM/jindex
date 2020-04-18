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
      ([(ir)
        (match (bindings-assq #"ir" (request-bindings/raw req))
          [(? binding:form? b)
           (bytes->string/utf-8 (binding:form-value b))])]
       [(start)
        (match (bindings-assq #"start" (request-bindings/raw req))
          [(? binding:form? b)
           (bytes->string/utf-8 (binding:form-value b))])]
       [(k-value) 0]
       [(pt-map cfg) (run ir start)]
       [(res) (hash 'point-to-map pt-map
                    'CFG cfg)])
    (response/jsexpr res)))

(define (not-found req)
  (response #:code 404
            #:message "Not Found"))

(define-values (routes _)
  (dispatch-rules
   [("") default]
   [("start") #:method "post" analyze]
   [else not-found]))

(serve/servlet routes
               #:port 8081
               #:servlet-regexp #rx"")

