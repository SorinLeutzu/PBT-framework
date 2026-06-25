module GrammarFuzzer.Examples where

import GrammarFuzzer.GrammarDefinitions


someGrammar :: Grammar
someGrammar = Grammar
  [ (N "A", [ [s (T "ab"), s (N "A"), s (T "bc")]
           , [s (N "A"), s (T "aaa"), s (N "B")]
           ])
  , (N "B", [ [s (T "ab"), s (N "A"), s (T "bc")]
           , [s (N "A"), s (T "aaa")]
           ])
  , (N "C", [ [s (T "ab"), s (T "bc")]
           , [s (N "B"), s (T "aaa")]
           ])
  ] (N "A")

someGrammar' = Grammar' someGrammar 4 10

someGrammar2 :: Grammar
someGrammar2 = Grammar
  [ (N "A", [ [s (T "ab"), s (N "A"), s (T "bc")]
           , [s (N "B"), s (T "aaa"), s (N "B")]
           ])
  , (N "B", [ [s (T "ab"), s (T "ffff"), s (T "bc")]
           , [s (N "A"), s (T "aaa")]
           ])
  , (N "C", [ [s (T "ab"), s (T "bc")]
           , [s (N "B"), s (T "aaa")]
           ])
  ] (N "A")

someGrammar3 :: Grammar
someGrammar3 =  Grammar
  [ (N "A", [ [s (T "ab"), s (N "A"), s (T "bc")]
           , [s (N "B"), s (T "aaa"), s (N "A")]
           ])
  , (N "B", [ [s (T "ab"), s (T "ffff"), s (T "bc")]
           , [s (N "A"), s (T "aaa")]
           , [s (N "A")]
           ])
  , (N "C", [ [s (T "ab"), s (T "bc")]
           , [s (N "B"), s (T "aaa")]
           ])
  , (N "D", [ [s (N "D"), s (N "D")]
           , [s (N "D"), s (T "aaa"), s (N "D"), s (N "D")]
           ])
  ] (N "A")

someGrammar4 :: Grammar
someGrammar4 =  Grammar
  [ (N "A",  [ [s (T "bla")]
           ])
  ] (N "A")


grammarDigits :: Grammar
grammarDigits = Grammar
  [ (N "Digits", [ [s (T "0"), s (N "Digits")], [s (T "1"), s (N "Digits")], [s (T "2"), s (N "Digits")]
                 , [s (T "3"), s (N "Digits")], [s (T "4"), s (N "Digits")], [s (T "5"), s (N "Digits")]
                 , [s (T "6"), s (N "Digits")], [s (T "7"), s (N "Digits")], [s (T "8"), s (N "Digits")], [s (T "9"), s (N "Digits")]
                 , [s (T "0")], [s (T "1")], [s (T "2")], [s (T "3")], [s (T "4")], [s (T "5")], [s (T "6")], [s (T "7")], [s (T "8")], [s (T "9")]
                 ])
  ] (N "Digits")

grammarGreeting :: Grammar
grammarGreeting = Grammar
  [ (N "Greeting", [ [s (T "hi")]
                   , [s (T "hi"), s (T " "), s (N "Greeting")]
                   ])
  ] (N "Greeting")

grammarAB :: Grammar
grammarAB = Grammar
  [ (N "S", [ [s (T "a"), s (N "S")], [s (T "b"), s (N "S")], [s (T "a")], [s (T "b")] ])
  ] (N "S")

grammarParens :: Grammar
grammarParens = Grammar
  [ (N "P", [ [s (T "("), s (T ")"), s (N "P")]
            , [s (T "("), s (N "P"), s (T ")"), s (N "P")]
            , [s (T "("), s (T ")")]
            ])
  ] (N "P")

grammarArithmetic' :: Grammar
grammarArithmetic' = Grammar
  [ (N "Expr",  [ [s (N "Term")]
                , [s (N "Expr"), s (T " + "), s (N "Term")]
                ])
  , (N "Term",  [ [s (N "Factor")]
                , [s (N "Term"), s (T " * "), s (N "Factor")]
                ])
  , (N "Factor", [ [s (N "Number")]
                 , [s (T "("), s (N "Expr"), s (T ")")]
                 ])
  , (N "Number", [ [s (N "Digit"), s (N "Number")]
                 , [s (N "Digit")]
                 ])
  , (N "Digit",  [ [s (T "0")], [s (T "1")], [s (T "2")], [s (T "3")], [s (T "4")]
                 , [s (T "5")], [s (T "6")], [s (T "7")], [s (T "8")], [s (T "9")]
                 ])
  ] (N "Expr")

grammarArithmetic :: Grammar
grammarArithmetic = Grammar
  [ (N "Expr",  [ [s (N "Term")]
                , [s (N "Expr"), s (T " + "), s (N "Term")]
                ])
  , (N "Term",  [ [s (N "Factor")]
                , [s (N "Term"), s (T " * "), s (N "Factor")]
                ])
  , (N "Factor", [ [s (N "Number")]
                 , [s (T "("), s (N "Expr"), s (T ")")]
                 ])
  , (N "Number", [ [s (N "Digit"), s (N "Number")]
                 , [s (N "Digit")]
                 ])
  , (N "Digit",  [ [s (T "0")], [s (T "1")], [s (T "2")], [s (T "3")], [s (T "4")]
                 , [s (T "5")], [s (T "6")], [s (T "7")], [s (T "8")], [s (T "9")]
                 ])
  ] (N "Expr")

simpleFileSystemTree :: ParseTree
simpleFileSystemTree =
  Tree (T "D root")
    [ Leaf (T "f hello.txt Hello from the fuzzer") []
    , Tree (T "D docs")
        [ Leaf (T "f readme.md # Readme\nThis was generated.") []
        , Leaf (T "E empty") []
        ]
    , Leaf (T "f notes.txt line1\nline2\nline3") []
    ]
