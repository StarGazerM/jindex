;; A time stamped CESK* abstract machine in 0-CFA style
;; assume our IR is already "ANF" and has now implicit return
;; assume import has been optimized
;; the code is based on m-cfa paper
;; http://matt.might.net/papers/might2010mcfa.pdf

;; TODO:
;; need to handle type properly
;; find how to inject a program without start position
;; in compile time make all arithmic in racket sexpr

;; Yihao Sun <ysun67@syr.edu>
#lang racket

(require "ir.rkt")

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

;; continuation
;; k ∈ Kont = Var × Stmt × BEnv × KontPtr
(define (kont? k)
  (match k
    ;; empty continuation
    ['mt #t]
    [`(Kont ,var ,stmt ,(? benv?) ,(? vaddr?)) #t]
    [else #f]))

;; d ∈ D = Value

;; v ∈ Value = Object + Kont
(define (value? v)
  (match v
    [(? vobj?) #t]
    [(? kont?) #t]
    [else #f]))

;; vaddr ↦ (set value/kont)
;; use list for set in Racket~
(define (sto? σ)
  (and (andmap vaddr? (hash-keys σ))
       (andmap (listof (or/c value? kont?)) (hash-values σ))))

;; o ∈ VObject = ClassName × BEnv
(define (vobj? o)
  (match o
    [`(VObject ,name ,(? benv?)) #t]
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


;; find next statement to run
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
(define (alloc₀ ς c)
  (match ς
    [(? kont?) `(KontAddr ,c)]
    [else c]))

;; abstract time tick function for 0-CFA
;; always return empty
(define (tick₀ ς)
  '())

;; inject a java program into abstact machine with a start point
;; CompileUnit -> Position -> Σ
(define (inject/start code start-pos)
  `(,(find-syntax-by-pos code start-pos) ,(hash) ,(hash 'mt 'mt) mt ()))

;; eval atomic expr
;; AExp -> BEnv -> Sto -> (listof value)
(define (aeval a β σ)
  (match a
    [(? number?) a]
    ;; var ref
    [(? symbol?) (hash-ref σ (hash-ref β a))]))

;; step function, in order to make read program easy, I also
;; put a program arg here
(define (⇝ code ς)
  (match ς
    [`(,c ,(? benv? β) ,(? sto? σ) ,(? vaddr? a) (? list? ls))
     (match c
       ;; a empty cause halting
       ['mt 'mt]
       [`(,pos Block ,(? list? insns)) 's])]))

(define (run code)
  'TODO)
