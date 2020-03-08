;; Predicate for s-expr like Java IR
;;

;; Yihao Sun <ysun67@syr.edu>

#lang racket

(provide (all-defined-out))


(define (compile-unit? cu)
  (match cu
    [`(CompUnit ,pkg ,(? list? imports) ,tdecl) #t]
    [else #f]))

(define (class-def? c)
  (match))

(define (method? m)
  (match m
    [`(,pos Method ,(? list? mods) ,head ,(? list? body)) (andmap block? body)]
    [else #f]))

(define (method-header? mh)
  (match mh
    ;; some throw and generic precision is miss here
    [`(,pos MethodHeader ,retT ,_ ,name ,(? list? args) ,_) #t]
    [else #f]))

(define (formal-param? fp)
  (match fp
    [`(,pos Arg ,mods ,type ,name) #t]
    [else #f]))

(define (block? b)
  (match b
    [`(,pos Block ,(? list? insns))
     (andmap (or/c local-var-decl?
                statement?)
             insns)]
    [else #f]))

;; loose precision in final
(define (local-var-decl? vd)
  (match vd
    [`(,pos LocalVar ,_ ,type ,(? list? inits))
     (andmap (λ (i) (match i
                       [`(= ,name ()) #t]
                       [`(= ,name ,init-v) #t]
                       [else #f]))
             inits)]))

;; need some no short if case here
(define (statement? st)
  (match st
    [(? block?) #t]
    ['mt #t]
    [(? expr?) #t]
    [`(,pos Break) #t]
    [`(,pos Return) #t]
    [`(,pos Continue) #t]
    [`(,pos Switch ,(? expr? guard) ,(? block? b)) #t]
    [`(,pos Case ,(? expr? e)) #t]
    [`(,pos Default) #t]
    [`(,pos IfThen ,(? expr? guard) ,(? block? b)) #t]
    [`(,pos IfElse ,(? expr? guard) ,(? statement? other) ,(? block? el)) #t]
    [`(,pos While (? expr? guard) ,(? statement? block)) #t]
    [else #f]))

(define (assigment? as)
  (match as
    [`(,pos (= ,(? lhs? le) ,(? expr? re))) #t]
    [else #f]))

(define (lhs? le)
  (match le
    [(? symbol?) #t]
    ;; TODO: handle array here
    [(? field-access?) #t]
    [else #f]))

(define (field-access? fa)
  (match fa
    [`(,pos FieldAccess ,(? primary? p) ,(? symbol? name)) #t]
    [`(,pos FieldAccess SUPER ,(? symbol? name)) #t]
    [else #f]))

(define (primary? p)
  (match p
    [`(,pos THIS) #t]))

;; no type/syntax check here
(define (expr? e)
  (match e
    ;; logic
    [`(,pos Or ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos And ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos InOr ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos ExOr ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos Eq ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos NotEq ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos Not ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos BitNot ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos < ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos > ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos <= ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos >= ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos isinstanceof ,(? expr? e₀) (? expr? e₁)) #t]
    ;; bitwise
    [`(,pos << ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos >> ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos >>> ,(? expr? e₀) (? expr? e₁)) #t]
    ;; arith
    [`(,pos + ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos - ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos * ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos / ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos Mod ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos Add1 ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos Sub1 ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos PAdd1 ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos PSub1 ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos Neg ,(? expr? e₀) (? expr? e₁)) #t]
    [`(,pos Or ,(? expr? e₀) (? expr? e₁)) #t]
    ;; method invoke
    [`(,pos MethodInvoc THIS ,name ,(? list? args))
     (andmap expr? args)]
    [`(,pos MethodInvoc SUPER ,name ,(?list args)) #t]
    [`(,pos MethodInvoc (? primary? e) ,name ,(?list args)) #t]
    [`(,pos MethodInvoc ,type ,name ,(?list args)) #t]
    ))
