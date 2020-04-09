;; Predicate for s-expr like Java IR
;;
;; need some passes before
;; 1. to ANF
;; 2. add return for void's implicit return

;; Yihao Sun <ysun67@syr.edu>

#lang racket

(provide (all-defined-out))

(define (position? p)
  (match p
    [`(,row ,clo) #t]
    [else #f]))

(define (compile-unit? cu)
  (match cu
    [`(CompUnit ,pkg ,(? list? imports) ,(? class-def?) ...) #t]
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

;; 
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
    [`(,pos Block ,insns ...)
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

;; atomic express
(define (aexpr? a)
  (match a
    [(? number?) #t]
    [(? symbol?) #t]
    [else #f]))

;; some syntax piece can be direct solved by racket builtin function
(define (prim?)
  (member '(Or And InOr ExOr Eq NotEq Not BitNot < > <= >=  << >> +
               - * / Mod Add1 Sub1 PSub1 PAdd1 Neg)))

(define (eval-prim p)
  (hash-ref
   (hash 'Eq equal? 'NotEq (λ (a b) (not (equal? a b)))
         'Not not 'BitNot bitwise-not '< < '> > '>= >= '<= <=
         'Add1 add1 'Sub1 sub1 'PAdd1 add1 'PSub1 sub1 'Neg -
         '+ + '- - '* * '/ /
         )))

(define (arith-expr? e)
  (match e
    [`(,pos Or ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos And ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos InOr ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos ExOr ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos Eq ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos NotEq ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos Not ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos BitNot ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos < ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos > ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos <= ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos >= ,(? expr? e₀) ,(? expr? e₁)) #t]
    ;; bitwise
    [`(,pos << ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos >> ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos >>> ,(? expr? e₀) ,(? expr? e₁)) #t]
    ;; arith
    [`(,pos + ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos - ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos * ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos / ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos Mod ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos Add1 ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos Sub1 ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos PAdd1 ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos PSub1 ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos Neg ,(? expr? e₀) ,(? expr? e₁)) #t]
    [`(,pos Or ,(? expr? e₀) ,(? expr? e₁)) #t]))

;; no type/syntax check here
;; though put expr inside of expr, but actually in ANF it can only be aexpr
(define (expr? e)
  (match e
    [(? aexpr?) #t]
    ;; logic
    [(? arith-expr?) #t]
    ;; object type check!
    [`(,pos isinstanceof ,(? expr? e₀) ,(? expr? e₁)) #t]
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
    [`(,pos MethodInvoc ,(? primary? e) ,name ,(? list? args)) #t]
    [`(,pos MethodInvoc ,type ,name ,(? list? args)) #t]
    [else #f]))

(define (constructor? c)
  (match c
    [`(,pos Constructor ,mods ,name ,(? list? args) ,throws ,body)
     (andmap formal-param? args)]
    [else #f]))

(define (cons-body? cb)
  (match cb
    [`(,pos Consbody ,cons-invocs ,stmts ...)
     (andmap statement? stmts)]
    [else #f]))


;; util function

;; everything here start with a syntax
(define (find-syntax-by-pos code pos)
  (define atomic? (or/c symbol? number? string? boolean?))
  (let/ec return
    (define (helper c p)
      (match c
        ['() #f]
        [(? atomic?) #f]
        [(? list?)
         (if (member p c)
             (return c)
             (map (λ (x) (helper x p)) c))]))
    (helper code pos)))

;; find the block contain that statment
(define (find-out-syntax-by-pos code pos)
  (define atomic? (or/c symbol? number? string? boolean?))
  (let/ec return
    (define (helper c p)
      (match c
        ['() #f]
        [(? atomic?) #f]
        [(? list?)
         (if (ormap
              (λ (x)
                (if (list? x) (member p x) #f)) c)
             (return c)
             (map (λ (x) (helper x p)) c))]))
    (helper code pos)))

(define test
  '(CompUnit
    ((4 0) Package com.foo.bar)
    (((6 0) Import (java util List )) )
    ((8 0) Class Foo
           ()
           ()
           ()
           ()
           (((9 4) Field () int (((9 8) = data ()) ))
            ((11 4) Constructor (public ) Foo (()) ()
                    ((11 16) ConsBody ()
                             ((12 8) = ((12 8) FieldAccess ((12 8) THIS) data) ((12 20) 0))
                             ((13 8) MethodInvoc System.out println (((13 27) "this is foo!") ))
                             ((14 8) MethodInvoc ((14 8) THIS) bar (((14 17) 1) ))
                             ((15 8) Return ()) ))
            ((18 4) Method (public ) ((18 11) MethodHeader (void) bar ((18 20) Arg () int a)  ())
                    ((18 27) Block
                     ((19 8) LocalVar () int (((19 12) = c ()) ))
                     ((20 8) LocalVar () int (((20 12) = b ((20 16) 0)) ))
                     ((21 8) = ((21 8) FieldAccess ((21 8) THIS) data) ((21 20) a))
                     ((22 8) Return ()) ))
            ))))

(define test2
  '(CompUnit
    ((4 0) Package com.foo.bar)
    ()
    ((6 0)
     Class Bar
     (public )
     ()
     ()
     ()
     (((7 4) Field () int (((7 8) = www ()) ))
      ((8 4) Constructor (public ) Bar (()) ()
             ((8 17) ConsBody () ((9 8) = ((9 8) FieldAccess ((9 8) THIS) www) ((9 19) 1)) ))))
    ((13 0) Class Foo
            ()
            ()
            ()
            ()
            (((14 4) Field () int (((14 8) = data ()) ))
             ((15 4) Field () StringBuilder (((15 18) = sb ()) ))
             ((17 4) Constructor (public ) Foo (()) ()
                     ((17 16) ConsBody ()
                              ((18 8) = ((18 8) FieldAccess ((18 8) THIS) data) ((18 20) 0))
                              ((19 8) = ((19 8) FieldAccess ((19 8) THIS) sb)
                                      ((19 18) ((19 18) New StringBuilder () () ())))
                              ((20 8) MethodInvoc System.out println (((20 27) "this is foo!") ))
                              ((21 8) MethodInvoc ((21 8) THIS) bar (((21 17) 1) )) ((22 8) Return ()) ))
             ((25 4) Method (public ) ((25 11) MethodHeader (void) bar ((25 20) Arg () int a)  ())
                     ((25 27) Block
                              ((26 8) LocalVar () int (((26 12) = c ()) ))
                              ((27 8) LocalVar () int (((27 12) = b ((27 16) 0)) ))
                              ((28 8) LocalVar () Bar (((28 12) = barclass
                                                                ((28 23) ((28 23) New Bar () () ())))))
                              ((29 8) = ((29 8) barclass.www) ((29 23) ((29 23) b)))
                              ((30 8) = ((30 8) FieldAccess ((30 8) THIS) data) ((30 20) ((30 20) a)))
                              ((31 8) Return ()) ))
             ))))
