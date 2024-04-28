module Review.Dependencies exposing (elmCore, elmParser, elmUrl)

{-| Not to be confused with `Review.Test.Dependencies`. This is for internal purposes only.

These packages contain all known operators in the Elm ecosystem, and they are used to properly parse
Elm files. The dependencies here are simplified and **only** contain information relative to the operators.

-}

import Dict
import Elm.Dependency
import Elm.Interface
import Elm.Syntax.Infix
import Elm.Syntax.Node
import Elm.Syntax.Range


elmCore : Elm.Dependency.Dependency
elmCore =
    { name = "elm/core"
    , version = "1.0.0"
    , interfaces =
        Dict.fromList
            [ ( [ "Basics" ]
              , [ -- infix right 0 (<|) = apL
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Right
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 0
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "<|"
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "apL"
                    }
                , -- infix left  0 (|>) = apR
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Left
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 0
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "|>"
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "apR"
                    }
                , -- infix right 2 (||) = or
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Right
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 2
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "||"
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "or"
                    }
                , -- infix right 3 (&&) = and
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Right
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 3
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "&&"
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "and"
                    }
                , -- infix non   4 (==) = eq
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Non
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 4
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "=="
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "eq"
                    }
                , -- infix non   4 (/=) = neq
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Non
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 4
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "/="
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "neq"
                    }
                , -- infix non   4 (<)  = lt
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Non
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 4
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "<"
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "lt"
                    }
                , -- infix non   4 (>)  = gt
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Non
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 4
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange ">"
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "gt"
                    }
                , -- infix non   4 (<=) = le
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Non
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 4
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "<="
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "le"
                    }
                , -- infix non   4 (>=) = ge
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Non
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 4
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange ">="
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "ge"
                    }
                , -- infix right 5 (++) = append
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Right
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 5
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "++"
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "append"
                    }
                , -- infix left  6 (+)  = add
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Left
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 6
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "+"
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "add"
                    }
                , -- infix left  6 (-)  = sub
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Left
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 6
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "-"
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "sub"
                    }
                , -- infix left  7 (*)  = mul
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Left
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 7
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "*"
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "mul"
                    }
                , -- infix left  7 (/)  = fdiv
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Left
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 7
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "/"
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "fdiv"
                    }
                , -- infix left  7 (//) = idiv
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Left
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 7
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "//"
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "idiv"
                    }
                , -- infix right 8 (^)  = pow
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Right
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 8
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "^"
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "pow"
                    }
                , -- infix left  9 (<<) = composeL
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Left
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 9
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "<<"
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "composeL"
                    }
                , -- infix right 9 (>>) = composeR
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Right
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 9
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange ">>"
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "composeR"
                    }
                ]
              )
            , ( [ "List" ]
              , [ -- infix right 5 (::) = cons
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Right
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 5
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "::"
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "cons"
                    }
                ]
              )
            ]
    }


elmUrl : Elm.Dependency.Dependency
elmUrl =
    { name = "elm/url"
    , version = "1.0.0"
    , interfaces =
        Dict.fromList
            [ ( [ "Url", "Parser" ]
              , [ -- infix right 7 (</>) = slash
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Right
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 7
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "</>"
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "slash"
                    }
                , -- infix left  8 (<?>) = questionMark
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Left
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 8
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "<?>"
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "questionMark"
                    }
                ]
              )
            ]
    }


elmParser : Elm.Dependency.Dependency
elmParser =
    { name = "elm/parser"
    , version = "1.0.0"
    , interfaces =
        Dict.fromList
            [ ( [ "Parser" ]
              , [ -- infix left 5 (|=) = keeper
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Left
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 5
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "|="
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "keeper"
                    }

                -- infix left 6 (|.) = ignorer
                , Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Left
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 6
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "|."
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "ignorer"
                    }
                ]
              )
            , ( [ "Parser", "Advanced" ]
              , [ -- infix left 5 (|=) = keeper
                  Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Left
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 5
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "|="
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "keeper"
                    }

                -- infix left 6 (|.) = ignorer
                , Elm.Interface.Operator
                    { direction = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange Elm.Syntax.Infix.Left
                    , precedence = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange 6
                    , operator = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "|."
                    , function = Elm.Syntax.Node.Node Elm.Syntax.Range.emptyRange "ignorer"
                    }
                ]
              )
            ]
    }
