;; A time stamped CESK* abstract machine in 0-CFA style
;; assume our IR is already "ANF" and has now implicit return
;; of course since we are in Imperetive language it is actually a loose SSA, since we
;; still allow function assign, clearly function call may not be atomic here (evaled in
;; in one step)
;; clearify more, only return point in this ANF like languge is assignment! (void function
;; still return to void but in interpretation it is logically inlined)
;; assume import has been optimized
;; the code is based on m-cfa paper
;; http://matt.might.net/papers/might2010mcfa.pdf

;; TODO:
;; need to handle type properly
;; find how to inject a program without start position
;; in compile time make all arithmic in racket sexpr
;; ANF also means it will be no trained access to object field
;; add support for SUPER keyword(many be resolve this even during compiling as a pass)
;; support static method invok
;; support Casting
;; support arithmetic
;; reduce multi initialization into single
;; eliminate empty block
;; remove redundent position in sexpr IR (like in New)
;; implement static Class and static invocation
;; change run function into real small-step style

;; Yihao Sun <ysun67@syr.edu>
#lang racket

(require racket/hash)
(require "ir.rkt"
         "len.rkt"
         "test.rkt")

(provide run
         MVALUE)

(define MVALUE (make-parameter 0))

;; Addr
(define (vaddr? va)
  (match va
    [(? position?) #t]
    [`(KontAddr ,_) #t]
    [else #f]))

;; control string is just statement
(define control? statement?)

;; β ∈ BEnv = var ↦ vaddr
(define (benv? β)
  (and (andmap symbol? (hash-keys β))
       (andmap vaddr? (hash-values β))))

;; continuation with position infomation
;; k ∈ Kont = Var × Position × BEnv × KontPtr
(define (kont? k)
  (match k
    ;; empty continuation
    ['mt #t]
    [`(Kont ,var ,pos ,(? benv?) ,(? vaddr?)) #t]
    [else #f]))

;; d ∈ D = Value

;; v ∈ Value = Postion × (Object + Kont)
(define (value? v)
  (match v
    [(? number?) #t]
    [(? boolean?) #t]
    [`(Val ,(? position?) (? number?)) #t]
    [`(Val ,(? position?) (? string?)) #t]
    [`(Val ,(? position?) (Arith ,_)) #t]
    ;; when something flow to unknown
    [`(Val ⊤ ,info) #t]
    [`(Val ⊥ ,info) #f]
    [(? vobj?) #t]
    [(? kont?) #t]
    [else #f]))

;; vaddr ↦ (set value/kont)
;; use list for set in Racket~
(define (sto? σ)
  (and (andmap vaddr? (hash-keys σ))
       (andmap (listof (or/c value? kont?)) (hash-values σ))))

;; Object with postition infomation
;; o ∈ VObject = ClassName × Position × Type × BEnv
(define (vobj? o)
  (match o
    [`(VObject ,(? position?) ,name ,type ,(? benv?)) #t]
    [else #f]))

;; pₖ ∈ KontPtr ⊆ Addr

;; t ∈ Time is a set of timestmap, tunnable, depend one what CFA used
;; 0-CFA is always the same means context free

;; <C,E,S,K*,T>
;; ς ∈ Σ = Stmt × BEnv × Store × KontPtr × Times
;; times is finite set of timestamp
(define (state? ς)
  (match ς
    [`(,c ,(? benv? ρ) ,(? sto? σ) ,(? vaddr?) (? list?)) #t]
    ;; end state
    ['☠  #t]
    [else #f]))

;; ψ ∈ PointToRelation = Postion ↦ Position
;; point to relation is how a position is point to another, it will be final
;; result of this program
(define (point-to-rel? ψ)
  (and (andmap position? (hash-keys ψ))
       (andmap position? (hash-values ψ))))

;; some util function

;; find the proper polymorphic constructor
;; TODO: switch to type based
;; for now all constructor with same arg number will be conflated
;; CompileUnit -> name -> (listof Type) -> (listof Constructor)
(define (find-constructor-in-class cu class-name args-type)
  ;; first try to found in local
  (let ([c (find-class/name cu class-name)])
    (match c
      [`(,pos Class ,name ,mods ,type-params ,super-class ,superInterFace ,body)
       ;; match out all possible consturctor
       (filter (λ (x)
                 (match x
                   [`(,pos Constructor ,mods ,cname ,(? list? args) ,throws ,body)
                    (equal? cname name)]
                   [else #f]))
               body)])))

;; find target class in a compiler unit
(define (find-class/name cu class-name)
  (match cu
    [`(CompUnit ,pkg ,(? list? imports) ,(? class-def? cs) ...)
     ;; no duplicate name
     (let/ec return
         (foldl (λ (c res)
                  (match c
                    [`(,pos Class ,name ,mods ,type-params ,super-class ,superInterFace ,body)
                     (if (equal? name class-name)
                         (return c)
                         res)]))
                #f
                cs))]))


;; find a pos is in which class
(define (find-class-name/pos cu pos)
  (match cu
    [`(CompUnit ,pkg ,(? list? imports) ,(? class-def? cs) ...)
     ;; no duplicate name
     (let/ec return
       (foldl (λ (c res)
                (if (pos-in? c pos)
                    (match c
                      [`(,pos Class ,name ,mods ,type-params ,super-class ,superInterFace ,body)
                       (return name)])
                    res))
              #f
              cs))]))

(define (find-filed/class c)
  (match c
    [`(,pos Class ,name ,mods ,type-params ,super-class ,superInterFace ,body-list)
     (filter field? body-list)]))

(define (find-method/class c mname margs)
  (match c
    [`(,pos Class ,name ,mods ,type-params ,super-class ,superInterFace ,body-list)
     (let/cc return
       (foldl (λ (b res)
                (match b
                  ;; TODO: check arg type here
                  [(? method?)
                   #:when (equal? mname (find-name/method b))
                   (return b)]
                  [else res]))
              `(Val ⊤ ,pos) body-list))]))

(define (find-name/method m)
  (match m
    [`(,pos Method ,(? list? mods) ,head ,(? list? body))
     (match head
       [`(,pos MethodHeader ,retT ,name ,(? list? args) ,_)
        name])]))

;; find the type with a var name
(define (find-type/var v β σ)
  (match (hash-ref σ (hash-ref β v))
    [`(VObject ,name ,type ,(? benv?)) type]))

;; find chained closure
(define (find-clo/chain c β σ)
  (match c
    [`(,pos (ChainName ,cnames ...))
     (define (helper names env)
       (match names
         ;; ccan't happen
         ['() (error "a chained name has more than one var name")]
         [`(,name) (error "a chained name has more than one var name")]
         [`(,name ,_) (hash-ref σ (hash-ref env name))]
         [`(,hd . ,rst)
          (define next-clos (hash-ref σ (hash-ref env hd)))
          (append-map (λ (nc)
                        (match nc
                          ;; must be a closure here
                          [`(VObject ,p ,n ,t ,e)
                           (helper rst e)]))
                      next-clos)]))
     (helper cnames β)]))

(define (pos-in? s p)
  (let/ec return
    (define (helper ss)
      (ormap (λ (x)
               (cond
                 [(equal? x p) (return #t)]
                 [(not (list? x)) #f]
                 [else (helper x)]))
             ss))
    (helper s)))

;; find next line statement to run
;; we only need to worry about block level thing's here
;; (listof ClassBody) -> Position -> Instruction
(define (succ code current-pos)
  (let/ec return
    (define outer (find-out-syntax-by-pos code current-pos))
    (define (helper ss)
      (match ss
        ;; implicit return
        [`(,ret)
         (if (member current-pos  ret)
             `(,(first ret) Return)
             ;; impossible
             #f)]
        [`(,h ,next . ,tail)
         (if (pos-in? h current-pos)
             (return next)
             (helper (cons next tail)))]))
    (match outer
      ;; in cons body
      [`(,pos ConsBody ,_  ,stmts ...)
       (helper stmts)]
      ;; in block
      [`(,pos Block ,insns ...)
       (helper insns)])))

(define (intermidiate-state? ς)
  (match ς
    [`((,pos = (,lhs ,(? (listof value?)))) ,_ ,_ ,_ ,_) #t]
    [else #f]))

;; 0-CFA style

;; abstract allocation function for 0-cfa
;; using the location of a syntax piece as abstract address
;; State → Position → vaddr
(define (alloc₀ ς pos)
  (match ς
    [(? kont?) `(KontAddr ,pos)]
    [else pos]))

;; abstract time tick function for 0-CFA
;; always return empty
(define (tick₀ ς)
  '())

(define (tick₁ pos)
  `(,pos))

;; union value into new store
(define (sto-∪ σ addr vs)
  (hash-update σ addr (λ (old) (set-union old vs)) '()))

;; inject a java program into abstact machine with a start point
;; we should have different stituation of how code start, because we want to do
;; analysis even if now main function, one way to solve this do analysis on
;; life circle of a class, this will make our analysis more "Object"
;; CompileUnit -> Position -> Σ
(define (inject/start code start-pos)
  `(,(find-syntax-by-pos code start-pos) ,(hash) ,(hash 'mt 'mt) mt ()))


;; eval atomic expr
;; since we are arctually fix data/control flow analysis so some point to relation should
;; also be returned here
;; AExp -> BEnv -> Sto -> -> PointToRelation -> (listof value) + PointToRelation
(define (aeval a β σ k ψ)
  (match a
    ;; if a is already a value, this can happen, because I do allow some intermidiate step
    ;; to make assign code shorter
    [(? value?) (values a ψ)]
    [(? (listof value?)) a]
    ;; empty usually means something is uninit
    ['() '()]
    [`(,pos ,(? string? d)) (list `(Val ,pos ,d))]
    [`(,pos ,(? boolean? d)) (list `(Val ,pos ,d))]
    [`(,pos ,(? number? d)) (list `(Val ,pos ,d))]
    ;; var ref
    [(? symbol?) (hash-ref σ (hash-ref β a) '⊤)]
    [`(,pos ,(? symbol? v))
     (hash-ref σ (hash-ref β v) '⊤)]
    ;; chained member access
    [`(,pos (ChainedName ,cnames ...))
     (define (helper names env)
       (match names
         ;; can't happen
         ['() (error "a chained name has no var name")]
         [`(,name) (hash-ref σ (hash-ref env name))]
         [`(,hd . ,rst)
          (define next-clos (hash-ref σ (hash-ref env hd)))
          (append-map (λ (nc)
                        (match nc
                          ;; must be a closure here
                          [`(VObject ,p ,n ,t ,e)
                           (helper rst e)]))
                      next-clos)]))
     (helper cnames β)]
    ;; field access
    [`(,pos FieldAccess ,(? primary? p) ,(? symbol? f))
     (match p
       [(? symbol? v)
        (let* ([pclos (hash-ref σ (hash-ref β v))]
               [β-primes
                (map (λ (pclo) (match pclo [`(VObject ,pos ,name ,type ,(? benv? e)) e])) pclos)])
          (map (λ (β-prime) (hash-ref σ (hash-ref β-prime f))) β-primes))]
       ;; THIS 
       [`(,pos THIS)
        (list (hash-ref σ (hash-ref β f)))])]
    ;; of course we need to support super here
    #;[`(,pos FieldAccess SUPER ,(? symbol? name)) #t]
    ;; arith is not atomic but can also be evaled in one step
    ;; we can use a super conservative way! just leave the static text there without real value
    [(? arith-expr? ae)
     (match ae
       [`(,pos ,op ,nums ...)
        (list `(Val ,pos (Arith ,(foldl (λ (x) (aeval x β σ k)) '() nums))))])]))

;; this will collect data flow information inside each state
;; collect-dataflow :: IR → State → PointToRelation → PointToRelation
(define (collect-dataflow ir ς ψ)
  (match ς
    [`(,c ,β ,σ ,kₐ ,ls)
     (match c
       [`(,pos LocalVar ,mod ,type ((,pos-= = ,lhs ,rhs)))
        (hash-set ψ (first rhs) pos-=)]
       [`(,pos = ,lhs (,pos-rhs New ,cname ,_ ,args ,_))
        (let* ([konstructor (first (find-constructor-in-class ir cname args))]
               [cargs (match konstructor
                        [`(,pos Constructor ,mods ,cname ,(? list? cs) ,throws ,body)
                         cs])])
          (for/fold ([res ψ])
                    ([arg args]
                     [carg (in-list cargs)])
            (hash-set res (first arg) (first carg))))]
       ;; this is a good case to show control flow and dataflow is nested with each other
       ;; for now all logic is in control flow case
       [`(,pos = ,lhs (,pos-rhs MethodInvoc ,p ,mname ,(? list? args)))
        ψ]
       ;; some case will not contribute to dataflow
       [else ψ])]))

;; step function, in order to make read program easy, I also put a program arg here
;; since we finally want is some point-to relation, so each step will make some change.
;; in some step it will diverger, like method call
;; ⇝ := IR → State → PointToRelation → (listof (State × PointToRelation))
(define (⇝ ir ς ψ)
  (match ς
    [`(,c ,β ,σ ,kₐ ,ls)
     (define current-kont (hash-ref σ kₐ))
     (match c
       ;; a empty cause halting
       ['mt 'mt]
       ;; Block, step into first instruction inside a block
       ;; assume no empty block
       ;; also should mind block var scope here
       [`(,pos Block ,(? list? insns))
        (define tick-prime (tick₀ ς))
        `(((,(first insns) ,β ,σ ,kₐ ,tick-prime) ,ψ))]
       ;; local var definition
       ;; just declare
       [`(,pos LocalVar ,mod ,type ((,pos-assign = ,lhs ())))
        ;; handle primitive type here, maybe no need to consider?
        (define new-addr (alloc₀ ς  pos-assign))
        (define null-obj `(VObject ,pos ,lhs ,type ,(hash)))
        (define next-line (succ ir pos))
        `(((,next-line ,(hash-set β lhs new-addr) ,(sto-∪ σ new-addr (list null-obj)) ,kₐ ls)
           ,ψ))]
       ;; assume only single init here
       ;; here we can just init with a empty Object/value, and step into assigment state
       ;; this is not always true about Java compiler may have problem here
       [`(,pos LocalVar ,mod ,type ((,pos-assign = ,lhs ,rhs)))
        ;; handle primitive type here, maybe no need to consider?
        (define new-addr (alloc₀ ς  pos-assign))
        (define null-obj `(VObject ,pos ,lhs ,type ,(hash)))
        `((((,pos-assign = ,lhs ,rhs) ,(hash-set β lhs new-addr) ,(sto-∪ σ new-addr (list null-obj)) ,kₐ ls)
          ,(hash-set ψ (first rhs) pos-assign)))]
       ;; assignment
       ;; since we are in ANF, this should be ONLY place we will return to!
       ;; but we also need some trival assign like a = 1
       ;; return point will be used for "new Foo()" and "a.foo", in their we can handle through return
       ;; and continuation
       ;; so it can be potencially return point, check if  cloure is create, if it is created, step
       ;; should handle SUPER here
       ;; rhs is object creation
       ;; this case is for import and primitive Java class
       [`(,pos = ,lhs (,pos-rhs New ,cname ,_ ,args ,_))
        #:when (not (find-class/name ir cname))
        (define next-line (succ ir pos))
        (define fake-closure `(VObject ,pos-rhs ,lhs ,cname ,(hash)))
        `(((,next-line ,β ,σ ,kₐ ,ls)
          ,ψ))]
       [`(,pos = ,lhs (,pos-rhs New ,cname ,_ ,args ,_))
        (define tick-prime (tick₀ ς))
        (let* ([args-vs (map (λ (arg) (aeval arg β σ current-kont)) args)]
               ;; TODO: calculate type of eachs argument
               [konstructor (first (find-constructor-in-class ir cname args))]
               [cargs (match konstructor
                        [`(,pos Constructor ,mods ,cname ,(? list? cs) ,throws ,body)
                        cs])]
               [next-line
                (match konstructor
                  [`(,pos Constructor ,mods ,cname ,(? list? cargs) ,throws ,body)
                   ;; assume no empty body, even void has been inserted empty return instruction
                   ;; body is a ds
                   (match body
                     [`(,pos ConsBody ,cons-invocs ,stmts ...) (first stmts)])])]
               ;; init fields
               [cfields (find-filed/class (find-class/name ir cname))])
          ;; all fields to top, all arg to it's val
          (let*-values ([(new-β σ-prime)
                         (for/fold ([nb (hash)]
                                    [ns σ])
                                   ([f (in-list cfields)])
                           (let ([f-addr (alloc₀ ς (first f))])
                             ;; assume only one assignment at one time
                             (match f
                               [`(,fpos Field ,mod ,type ((,fepos = ,fname ,_)))
                                (values (hash-set nb fname f-addr)
                                        (sto-∪ ns f-addr '(⊤)))])))]
                        ;; point value position to constructor arg position
                        [(new-β-p σ-pp ψ-prime)
                         (for/fold ([nb new-β]
                                    [ns σ-prime]
                                    [ps ψ])
                                   ([arg (in-list cargs)]
                                    [avs (in-list args-vs)])
                           (let ([a-addr (alloc₀ ς (first arg))])
                             (values (hash-set nb (last arg) a-addr)
                                     (sto-∪ ns a-addr avs)
                                     (foldl (λ (av pres) (hash-set pres (second av) (first arg))) ps avs))))])
            ;; set new continuation
            (let* ([new-obj `((VObject ,pos ,lhs ,cname ,new-β))]
                   [new-obj-addr (alloc₀ ς pos)]
                   [new-kont `(Kont ,lhs ,pos ,(hash-set β lhs new-obj-addr) ,kₐ)]
                   [new-kont-addr (alloc₀ new-kont pos-rhs)]
                   )
              `(((,next-line ,new-β-p
                             ,(hash-set (hash-set σ-pp new-obj-addr new-obj) new-kont-addr new-kont)
                             ,new-kont-addr ,tick-prime)
                 ,ψ-prime)))))]
       ;; method invokation
       [`(,pos = ,lhs (,pos-rhs MethodInvoc ,p ,mname ,(? list? args)))
        (define tick-prime (tick₀ ς))
        (define pclos
          (match p
            [`(,pos 'THIS)
             (list `(VObject ,pos fake ,(find-class-name/pos ir pos) ,β))]
            ;; also need super case and Static case
            [else (hash-ref σ (hash-ref β (second p)))]))
        (define pclo-type (match (first pclos) [`(VObject ,pos ,name ,type ,pe) type]))
        (define pclo-βs (map (λ (pclo) (match pclo [`(VObject ,pos ,name ,type ,pe) pe])) pclos))
        (define args-vs (map (λ (arg) (aeval arg β σ current-kont)) args))
        (define meth (find-method/class (find-class/name ir pclo-type) mname args-vs))
        (define formal-params
          (match meth
            [`(,pos Method ,(? list? mods) ,head (,pos-block Block ,stmts ...))
             (match head
               [`(,pos MethodHeader ,retT ,name ,(? list? fargs) ,_)
                fargs])]))
        ;; step into method body
        (define next-line (match meth
                            [`(,pos Method ,(? list? mods) ,head (,pos Block ,stmts ...))
                             (first stmts)]))
        (for/fold ([res '()])
                  ([pclo-β (in-list pclo-βs)])
          (define-values (β-prime σ-prime ψ-prime)
            (for/fold ([nb pclo-β]
                       [ns σ]
                       [np ψ])
                      ([arg (in-list formal-params)]
                       [avs (in-list args-vs)])
              (let ([a-addr (alloc₀ ς (first arg))])
                (values (hash-set nb (last arg) a-addr)
                        (sto-∪ ns a-addr avs)
                        (foldl (λ (av pres) (hash-set pres (second av) (first arg))) np avs)))))
          (let* ([new-kont `(Kont ,lhs ,pos ,β-prime ,kₐ)]
                 [new-kont-addr (alloc₀ new-kont pos-rhs)])
            `(cons
              ((,next-line ,β-prime ,(hash-set new-kont-addr σ-prime) ,new-kont-addr ,tick-prime)
               ,ψ-prime)
              res)))]
       ;; static Method Invoke, for here we will skip staic thing
       [`(,pos MethodInvoc (Static ,p) ,mname ,args)
        (define next-line (succ ir pos))
        `(((,next-line ,β ,σ ,kₐ ,ls)
           ,ψ))]
      [`(,pos MethodInvoc ,p ,mname ,args)
        (define tick-prime (tick₀ ς))
        (define pclos
          (match p
            [`(,pos-t THIS)
             (list `(VObject ,pos fake ,(find-class-name/pos ir pos) ,β))]
            ;; also need super case and Static case
            [else (hash-ref σ (hash-ref β (second p)))]))
        (define pclo-type (match (first pclos) [`(VObject ,pos ,name ,type ,(? benv?)) type]))
        (define pclo-β (match (first pclos) [`(VObject ,pos ,name ,type ,pe) pe]))
        (define args-vs (map (λ (arg) (aeval arg β σ current-kont)) args))
        (define meth (find-method/class (find-class/name ir pclo-type) mname args-vs))
        ;; step into method body
        (define next-line (match meth
                            [`(,pos Method ,(? list? mods) ,head (,pos-block Block ,stmts ...))
                             (first stmts)]))
        (define formal-params
          (match meth
            [`(,pos Method ,(? list? mods) ,head (,pos-block Block ,stmts ...))
             (match head
               [`(,pos MethodHeader ,retT ,name ,(? list? fargs) ,_)
                fargs])]))
        (define-values (β-prime σ-prime ψ-prime)
          (for/fold ([nb pclo-β]
                     [ns σ]
                     [np ψ])
                    ([arg (in-list formal-params)]
                     [avs (in-list args-vs)])
            (let ([a-addr (alloc₀ ς (first arg))])
              (values (hash-set nb (last arg) a-addr)
                      (sto-∪ ns a-addr avs)
                      (foldl (λ (av pres) (hash-set pres (second av) (first arg))) np avs)))))
        (let* ([out-obj-name
                (match current-kont
                  [`(Kont ,var ,pos ,b ,va)
                   var])]
               [new-kont `(Kont ,out-obj-name ,pos ,β-prime ,kₐ)]
               [new-kont-addr (alloc₀ new-kont pos)])
          `(((,next-line ,β-prime ,(hash-set σ-prime new-kont-addr new-kont) ,new-kont-addr ,tick-prime)
             ,ψ-prime)))]
       ;; in order to simplify this, I play some trick here, in return case, I will create a temporary
       ;; state in this (similar to Fn Ar continuation idea but from my point of view not) rhs can be
       ;; an value case
       ;; rhs is atomic unevaled-value/evaled value
       [`(,pos = ,lhs ,rhs)
        (let/ec return
          (define tick-prime (tick₀ ς))
          (define rhs-vs
            (aeval rhs β σ (hash-ref σ kₐ)))
          (match lhs
            ;; local varible
            [(? symbol? vname)
             (define addr-lhs (hash-ref β vname))
             `(((,(succ ir pos) ,(hash-set β lhs addr-lhs) ,(sto-∪ σ addr-lhs rhs-vs) ,kₐ ,tick-prime)
                ,(foldl (λ (p res)
                          (let ([pos-rhs (second p)])
                            (hash-set res pos-rhs pos)))
                        ψ rhs-vs)))]
            [`(,pos-f FieldAccess ,p ,name)
             (define pclos
               (match p
                 [`(,p-t THIS)
                  (list `(VObject ,pos fake ,(find-class-name/pos ir pos) ,β))]
                 [else (hash-ref σ (hash-ref β p))]))
             ;; the closure of this var name is actually in upper level(in continuaion)
             (define β-primes
               (map (λ (pclo) (match pclo [`(VObject ,pos-vo ,oname ,type ,e) e])) pclos))
             (define ψ-prime
               ;; if lhs is this field, we should also let lhs point to  that field position
               (match p
                 [`(,pos-t THIS)
                  ;; TODO: black magic here, should be changed future
                  (hash-set ψ pos-f (hash-ref β name))]
                 [else ψ-prime]))
             (for/fold
                 ([res '()])
                 ([β-prime (in-list β-primes)])
               (cons
                `((,(succ ir pos) ,β ,(sto-∪ σ (hash-ref β-prime name) rhs-vs) ,kₐ ,tick-prime)
                  ,(foldl (λ (p res)
                            (let ([pos-rhs (second p)])
                              (hash-set res pos-rhs pos-f)))
                          ψ-prime rhs-vs))
                res))]
            [`(,pos-c (ChainName ,names ...))
             (let* ([pclos (find-clo/chain lhs β σ)]
                    [name (last names)]
                    [β-primes
                     (map (λ (pclo) (match pclo [`(VObject ,pos-vo ,oname ,type ,e) e])) pclos)])
               (for/fold
                   ([res '()])
                   ([β-prime (in-list β-primes)])
                 (cons
                  `((,(succ ir pos) ,β ,(sto-∪ σ (hash-ref β-prime name) rhs-vs) ,kₐ ,tick-prime)
                    ,(foldl (λ (p res)
                              (let ([pos-rhs (second p)])
                                (hash-set res pos-rhs pos-c)))
                            ψ rhs-vs))
                  res)))]
            ))]
       ;; return case
       ;; empty return point, just got next line of
       [`(,pos-ret Return ())
        (define tick-prime (tick₀ ς))
        (let/ec return
          (define next
            (match current-kont
              [`(Kont ,var ,pos ,_ ,_)
               (succ ir pos)]
              ;; in mt continuation return means program finish
              ['mt (return `((☠ ,ψ)))]))
          `(((,next ,β ,σ ,(last current-kont) ,tick-prime)
             ,ψ)))]
       ;; Java only allow single return value
       [`(,pos-ret Retrun ,ret)
        (match current-kont
          ;; some return but return value is abandoned
          [`(Kont () ,pos-p  ,β-k ,(? vaddr? kₚ))
           (define tick-prime (tick₀ ς))
           (let/ec return
             (define next
               (match current-kont
                 [`(Kont ,var ,pos ,_ ,_)
                  (succ pos)]
                 ;; in mt continuation return means program finish
                 ['mt (return `((☠ ,ψ))) ]))
             `(((,next ,β ,σ ,(last current-kont) ,tick-prime)
                ,ψ)))]
          [`(Kont ,var ,pos-p ,β-k ,(? vaddr? kₚ))
           (define return-point (find-syntax-by-pos ir pos-p))
           ;; since we are in ANF, return point can only be assigment!
           (match return-point
             ;; return from a constructor the return value is a colsure
             ;; should move this case into empty return condition......
             [`(,pos-= = ,lhs (,pos-rhs New ,cname ,_ ,args ,_))
              `((((,pos-= = ,lhs ((VObject ,pos-rhs ,lhs ,cname ,β))) ,β-k ,σ ,kₚ ,ls)
                ,ψ))]
             [`(,pos-= = ,lhs (,pos-rhs MethodInvoc ,p ,mname ,(? list? args)))
              (define ret-vals (aeval ret β σ current-kont))
              `((((,pos-= = ,lhs ,ret-vals) ,β-k ,σ ,kₚ ,ls)
                 ,(hash-set ψ pos-ret pos-=)))])])])]

    ['☠ '☠]))


;; run the program, util meet ☠ or reach fixpoint
;; TODO: undeterministic step should be change to small step
(define (run code start-pos)
  (define (mult-⇝ ir ς ψ states)
    (define (merge-ψ p1 p2)
      (hash-union p1 p2
                  #:combine/key (λ (k v1 v2) (remove-duplicates (append v1 v2)))))
    (cond
      [(equal? '☠ ς)  ψ]
      [else
       ;; remove repeat state
       (define next-res (⇝ ir ς ψ))
       (define next-states
         (foldl (λ (x res) (if (member x states) res (cons x res)))
                '()
                (map first next-res)))
       (define next-ψs (map second next-res))
       (displayln "step to ⇝ >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
       (pretty-display next-states)
       (cond
         ;; state repeat, meet a fixpoint
         [(empty?  next-states) (foldl merge-ψ (hash) next-ψs)]
         ;; diverge and merge different control flow
         [else
          (foldl
           merge-ψ
           (hash)
           (map (λ (st)
                  (mult-⇝ ir (first st) (second st) (set-union next-states states)))
                next-res))])]))
  (mult-⇝ code (inject/start code start-pos) (hash) '()))
