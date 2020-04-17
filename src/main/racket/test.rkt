;; This is the test script

#lang racket

(provide (all-defined-out))

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
      ((8 4) Constructor (public ) Bar () ()
             ((8 17) ConsBody () ((9 8) = ((9 8) FieldAccess ((9 8) THIS) www) ((9 19) 1)) ))))
    ((13 0)
     Class Foo
     ()
     ()
     ()
     ()
     (((14 4) Field () int (((14 8) = data ()) ))
      ((15 4) Field () StringBuilder (((15 18) = sb ()) ))
      ((17 4) Constructor (public ) Foo () ()
              ((17 16) ConsBody ()
                       ((18 8) = ((18 8) FieldAccess ((18 8) THIS) data) ((18 20) 0))
                       ((19 8) = ((19 8) FieldAccess ((19 8) THIS) sb)
                               ((19 18)  New StringBuilder () (((19 36) "") ) ()))
                       ((20 8) MethodInvoc (Static System.out) println (((20 27) "this is foo!") ))
                       ((21 8) MethodInvoc ((21 8) THIS) bar (((21 17) 1) ))
                       ((22 8) Return ()) ))
      ((25 4) Method (public ) ((25 11) MethodHeader VOID bar (((25 20) Arg () int a))  ())
              ((25 27) Block
                       ((26 8) LocalVar () int (((26 12) = c ()) ))
                       ((27 8) LocalVar () int (((27 12) = b ((27 16) 0)) ))
                       ((28 8) LocalVar () Bar (((28 12) = barclass
                                                         ((28 23) New Bar () () ()))))
                       ((29 8) = ((29 8) (ChainName barclass www)) ((29 23) b))
                       ((30 8) = ((30 8) FieldAccess ((30 8) THIS) data) ((30 20)  a))
                       ((31 8) Return ()) ))
      ))
    ((35 0) Class Main
            ()
            ()
            ()
            ()
            (((36 4) Method (public static ) ((36 18) MethodHeader VOID main (((36 28) Arg () String[] args))  ())
                     ((36 43) Block
                              ((37 8) LocalVar () Foo (((37 12) = f
                                                                ((37 16)  New Foo () () ())) ))
                              ((38 8) Return ()) ))))))


