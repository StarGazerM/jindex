;; A time stamped CESK* abstract machine in 0-CFA style
;; assume our IR is already "ANF" and has now implicit return
;; of course since we are in Imperetive language it is actually a loose SSA, since we
;; still allow function assign, clearly function call may not be atomic here (evaled in
;; in one step)
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

;; Yihao Sun <ysun67@syr.edu>
#lang racket

(require "ir.rkt"
         "len.rkt")

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
;; k ∈ Kont = Var × Stmt × BEnv × KontPtr
(define (kont? k)
  (match k
    ;; empty continuation
    ['mt #t]
    [`(Kont ,var ,stmt ,(? benv?) ,(? vaddr?)) #t]
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
  (let ([c (find-class/name cu)])
    (match c
      [`(,pos Class ,name ,mods ,type-params ,super-class ,superInterFace ,body)
       ;; match out all possible consturctor
       (filter (λ (x)
                 (match x
                   [`(,pos Constructor ,mods ,cname ,(? list? args) ,throws ,body)
                    (equal? (length args) (args-type))]
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
                #f cs))]))

;; find the type with a var name
(define (find-type/var v β σ)
  (match (hash-ref σ (hash-ref β v))
    [`(VObject ,name ,type ,(? benv?)) type]))

;; find next line statement to run
;; we only need to worry about block level thing's here
;; (listof ClassBody) -> Position -> Instruction
(define (succ code current-pos)
  (define outer (find-out-syntax-by-pos code current-pos))
  (match outer
    [`(,pos ConsBody ,_  ,stmts ...)
     (let/ec return
       (define (helper ss)
         (match ss
           ;; implicit return
           [`(,ret)
            (if (member current-pos  ret)
                `(,(first ret) Return)
                ;; impossible
                #f)]
           [`(,h ,next ,tail)
            (if (member current-pos h)
                (return next)
                (helper (cons next tail)))]))
       (helper stmts))]))

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
;; AExp -> BEnv -> Sto -> (listof value)
(define (aeval a β σ k)
  (match a
    ;; empty usually means something is uninit
    ['() '()]
    [(? boolean?) (list `(Val ⊥ ,a))]
    [(? number?) (list `(Val ⊥ ,a))]
    ;; var ref
    [(? symbol?) (hash-ref σ (hash-ref β a))]
    ;; field access
    [`(,pos FieldAccess ,(? primary? p) ,(? symbol? f))
     (match p
       [(? symbol? v)
        (let* ([pclos (hash-ref σ (hash-ref β v))]
               [β-primes
                (map (λ (pclo) (match pclo [`(VObject ,pos ,name ,type ,(? benv? e)) e])) pclos)])
          (map (λ (β-prime) (hash-ref σ (hash-ref β-prime f))) β-primes))]
       ;; THIS can be solved by continuation
       [`(,pos THIS)
        (let* ([v (match k [`(Kont ,var ,stmt ,(? benv?) ,(? vaddr?)) var])]
               [pclos (hash-ref σ (hash-ref β v))]
               [β-primes
                (map (λ (pclo) (match pclo [`(VObject ,pos ,name ,type ,(? benv? e)) e])) pclos)])
          (map (λ (β-prime) (hash-ref σ (hash-ref β-prime f))) β-primes))])]
    ;; of course we need to support super here
    #;[`(,pos FieldAccess SUPER ,(? symbol? name)) #t]
    ;; arith is not atomic but can also be evaled in one step
    ;; we can use a super conservative way! just leave the static text there without real value
    [(? arith-expr? ae)
     (match ae
       [`(,pos ,op ,nums ...)
        (list `(Val ,pos (Arith ,(foldl (λ (x) (aeval x β σ k)) '() nums))))])]))

;; step function, in order to make read program easy, I also put a program arg here
;; since we finally want is some point-to relation, so each step will make some change.
;; in some step it will diverger, like method call
;; ⇝ := IR → State → PointToRelation → (listof (State × PointToRelation))
(define (⇝ ir ς ψ)
  (match ς
    [`(,c ,(? benv? β) ,(? sto? σ) ,(? vaddr? kₐ) ,(? list? ls))
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
       ;; assume only single init here
       ;; in ANF, rhs is always atomic or we say can be eavled in one step
       [`(,pos LocalVar ,mod ,type ((= ,lhs ,rhs)))
        (define rhs-vs (aeval rhs β σ (hash-ref σ kₐ)))
        (define tick-prime (tick₀ ς))
        (define addr-lhs (alloc₀ ς pos))
        ;; assume lhs can only a var
        `(((,(succ pos) ,(hash-set β lhs addr-lhs) ,(sto-∪ σ addr-lhs rhs-vs) ,kₐ ,tick-prime)
           ,(foldl (λ (p res)
                     ;; no first order continuation in Java
                     (define pos-rhs (second p))
                     (hash-set res pos-rhs pos))
                   ψ rhs-vs)))]
       ;; assignment
       ;; should handle SUPER here
       [`(,pos = (,lhs ,rhs))
         (define tick-prime (tick₀ ς))
         (define rhs-vs (aeval rhs β σ (hash-ref σ kₐ)))
         (match lhs
           ;; local varible
           [`(,pos-v ,(? symbol? vname))
            (define addr-lhs (hash-ref β vname))
            `(((,(succ pos) ,(hash-set β lhs addr-lhs) ,(sto-∪ σ addr-lhs rhs-vs) ,kₐ ,tick-prime)
               ,(foldl (λ (p res)
                         (let ([pos-rhs (second p)])
                           (hash-set res pos-rhs pos-v)))
                       ψ rhs-vs)))]
           [`(,pos-f FieldAccess ,p ,name)
            (define obj-name
              (match p
                [`(,p-t THIS)
                 (match (hash-ref σ kₐ) [`(Kont ,var ,stmt ,b ,va) var])]))
            (let* ([pclos (hash-ref σ (hash-ref β obj-name))]
                   [β-primes
                    (map (λ (pclo) (match pclo [`(VObject ,pos-vo ,oname ,type ,e) e])) pclos)])
              (for/fold
                  ([res '()])
                  ([β-prime (in-list β-primes)])
                  (cons
                   `((,(succ pos) ,β ,(sto-∪ σ (hash-ref β-prime name) rhs-vs) ,kₐ ,tick-prime)
                     ,(foldl (λ (p res)
                               (let ([pos-rhs (second p)])
                                 (hash-set res pos-rhs pos-f)))
                             ψ rhs-vs))
                   res)))])]
       )]))

(define (run code)
  'TODO)
