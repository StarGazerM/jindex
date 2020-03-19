;; Predicate for s-expr like Java IR
;;

;; Yihao Sun <ysun67@syr.edu>

#lang racket

(provide (all-defined-out))


(define (compile-unit? cu)
  (match cu
    [`(CompUnit ,pkg ,(? list? imports) ,(? class-def?)) #t]
    [else #f]))

(define (import? i)
  (match i
    [`(,pos Import ,list-of-type-name) #t]
    [else #f]))

(define (class-def? c)
  (match c
    [`(,pos Class ,name ,mods ,type-params ,super-class ,superInterFace ,body) #t]
    [else #f]))

;; no inner class for now
(define (class-body? cb)
  (match cb
    [(? field?) #t]
    [(? method?) #t]
    [else #f]))

(define (field? f)
  (match f
    [`(,pos Field ,mods ,types ,(? list? inits))
     (andmap (λ (i) (match i
                       [`(= ,name ()) #t]
                       [`(= ,name ,init-v) #t]
                       [else #f]))
             inits)]))

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
    [(? assigment?) #t]
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

(define (lit? l) (or/c symbol? number? boolean?))

;; assume we are in ANF
(define (primary? p)
  (match p
    [`(,pos THIS) #t]
    [(? lit?) #t]
    ;; reflection
    [`(,pos Refl ,typename) #t]
    [(? field-access?) #t]
    [(? method-invoc?) #t]))

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
    [(? method-invoc?) #t]
    ;; class creation
    [(? class-init?) #t]
    [else #f]
    ))

;;class instance creation
(define (class-init? ci)
  (match ci
    ;; no need for type Arg here, because of "<>" in Java8
    ;; and for now won't work for class overload.
    [`(,pos New ,name ,_ ,args ,_) #t]
    [else #f]))

(define (method-invoc? mi)
  (match mi
    [`(,pos MethodInvoc THIS ,name ,(? list? args))
     (andmap expr? args)]
    [`(,pos MethodInvoc SUPER ,name ,(? list? args)) #t]
    [`(,pos MethodInvoc (? primary? e) ,name ,(? list? args)) #t]
    [`(,pos MethodInvoc ,type ,name ,(? list? args)) #t]
    [else #f]))

(define (constructor? c)
  (match c
    [`(,pos Constructor ,mods ,type ,(? list? args) ,throws ,body)
     (andmap formal-param? args)]
    [else #f]))

(define (cons-body? cb)
  (match cb
    [`(,pos Consbody ,cons-invocs ,stmts)
     (andmap statement? stmts)]
    [else #f]))

(define test
  '(CompUnit
    ((4 0) Package com.foo.bar)
    (((6 0) Import (java util List )) )
    ((8 0) Class Foo
           ()
           ()
           ()
           ()
           #(((9 4) Field () int (((9 8) = data ()) ))
             ((11 4) Constructor (public ) Foo (()) ()
                     ((11 16) ConsBody ()
                              (((12 8) = ((12 8) FieldAccess ((12 8) THIS) data) ((12 20) 0))
                               ((13 8) MethodInvoc System.out println (((13 27) "this is foo!") ))
                               ((14 8) MethodInvoc ((14 8) THIS) bar (((14 17) 1) )) )))
             ((17 4) Method (public ) ((17 11) MethodHeader (void) bar ((17 20) Arg () int a)  ())
                     ((17 27) Block
                              (((18 8) LocalVar () int (((18 12) = c ()) ))
                               ((19 8) LocalVar () int (((19 12) = b ((19 16) 0)) ))
                               ((20 8) = ((20 8) FieldAccess ((20 8) THIS) data) ((20 20) a)) )))
             )))

  )
