module Tests exposing (tests)

import CommentDoesNotUseCertainMarks
import DebugIsNotUsed
import ImportExposingIsExplicit
import LetValueOrFunctionIsTypeAnnotated
import ModuleAndExposesAreUsed
import ModuleExposingIsExplicit
import ModuleNameWithUnderscoreForbid
import ModuleValueOrFunctionIsTypeAnnotated
import PatternVariableIsUsed
import Review
import Review.Test
import Test exposing (Test)


tests : Test
tests =
    Test.describe "example reviews"
        [ moduleNameWithUnderscoreForbidTests
        , moduleValueOrFunctionIsTypeAnnotatedTests
        , letValueOrFunctionIsTypeAnnotatedTests
        , moduleAndExposesAreUsedTests
        , patternVariablesAreUsedTests
        , moduleExposingIsExplicitTests
        , importExposingIsExplicitTests
        , debugIsNotUsedTests
        , commentDoesNotUseCertainWordsTests
        ]


moduleNameWithUnderscoreForbidTests : Test
moduleNameWithUnderscoreForbidTests =
    Test.describe "ModuleNameWithUnderscoreForbid"
        [ Test.test "single module without errors"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)
                            a =
                                ""
                            """
                      }
                    ]
                , review = ModuleNameWithUnderscoreForbid.review
                , expectedErrors = []
                }
                    |> Review.Test.run
            )
        , Test.test "single module with error"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A_.elm"
                      , source = """
                            module A_ exposing (a)
                            a =
                                ""
                            """
                      }
                    ]
                , review = ModuleNameWithUnderscoreForbid.review
                , expectedErrors =
                    [ { path = "src/A_.elm"
                      , message = "module name contains _"
                      , details = [ "By convention, elm modules names use Pascal case (like `MyModuleName`). Please rename your module using this format." ]
                      , range = Review.Test.Under "A_"
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        ]


moduleValueOrFunctionIsTypeAnnotatedTests : Test
moduleValueOrFunctionIsTypeAnnotatedTests =
    Test.describe "ModuleValueOrFunctionIsTypeAnnotated"
        [ Test.test "allows annotated value and function declaration and un-annotated let value and function declaration"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)

                            a : String
                            a =
                                b ()
                            
                            b : () -> String
                            b () =
                                let
                                    c () =
                                        ""
                                    
                                    d =
                                        c ()
                                in
                                d
                            """
                      }
                    ]
                , review = ModuleValueOrFunctionIsTypeAnnotated.review
                , expectedErrors = []
                }
                    |> Review.Test.run
            )
        , Test.test "reports un-annotated value declaration"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)

                            a =
                                ""
                            """
                      }
                    ]
                , review = ModuleValueOrFunctionIsTypeAnnotated.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "declaration A.a has no type annotation"
                      , details =
                            [ "Type annotations help to quickly understand what's expected in the code, and it will help the compiler give better error messages."
                            , "Try adding a line a : ItsType above the declaration. If you don't know the type, you can start by using the \"infer type\" feature of your IDE or inserting the type like `Never` and letting the compiler fill you in on the details."
                            ]
                      , range = Review.Test.UnderExactly { section = "a", startingAt = { row = 3, column = 1 } }
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        ]


letValueOrFunctionIsTypeAnnotatedTests : Test
letValueOrFunctionIsTypeAnnotatedTests =
    Test.describe "LetValueOrFunctionIsTypeAnnotated"
        [ Test.test "allows annotated and un-annotated module-level value and function declaration and annotated let value and function declaration"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)

                            a : String
                            a =
                                b ()
                            
                            b : () -> String
                            b () =
                                ""
                            
                            c =
                                let
                                    d : () -> String
                                    d () =
                                        ""
                                    
                                    e : String
                                    e =
                                        d ()
                                in
                                e
                            """
                      }
                    ]
                , review = LetValueOrFunctionIsTypeAnnotated.review
                , expectedErrors = []
                }
                    |> Review.Test.run
            )
        , Test.test "reports un-annotated let value declaration"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)

                            a =
                                let
                                    b =
                                        ""
                                in
                                b
                            """
                      }
                    ]
                , review = LetValueOrFunctionIsTypeAnnotated.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "let declaration b has no type annotation"
                      , details =
                            [ "Type annotations help to quickly understand what's expected in the code, and it will help the compiler give better error messages."
                            , "Try adding a line b : ItsType above the declaration. If you don't know the type, you can start by using the \"infer type\" feature of your IDE or inserting the type like `Never` and letting the compiler fill you in on the details."
                            ]
                      , range = Review.Test.UnderExactly { section = "b", startingAt = { row = 5, column = 9 } }
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        ]


moduleExposingIsExplicitTests : Test
moduleExposingIsExplicitTests =
    Test.describe "ModuleExposingIsExplicit"
        [ Test.test "allows module exposing (value, function, type alias, choice type)"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (Alias, Choice(..), function, value)

                            value =
                                ""
                            
                            function () =
                                value
                            
                            type Choice
                                = Variant
                            
                            type alias Alias =
                                String
                            """
                      }
                    ]
                , review = ModuleExposingIsExplicit.review
                , expectedErrors = []
                }
                    |> Review.Test.run
            )
        , Test.test "reports module exposing (..) with value, function, type alias, choice type"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (..)

                            value =
                                ""
                            
                            function () =
                                value
                            
                            type Choice
                                = Variant
                            
                            type alias Alias =
                                String
                            """
                      }
                    ]
                , review = ModuleExposingIsExplicit.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "module A exposes everything, not explicit"
                      , details =
                            [ "Modules can have helpers for implementation details which the users of this module should not have to know about. Therefore, the API should be explicitly defined and as small as possible."
                            , "Try to explicitly pick the members you want to make public and put them where the .. is currently. To start with all the currently exposed members, accept the provided fix and then remove the undesired ones."
                            ]
                      , range = Review.Test.Under ".."
                      , fixedFiles =
                            [ { path = "src/A.elm"
                              , source = """
                                    module A exposing (Alias, Choice(..), function, value)

                                    value =
                                        ""
                                    
                                    function () =
                                        value
                                    
                                    type Choice
                                        = Variant
                                    
                                    type alias Alias =
                                        String
                                    """
                              }
                            ]
                      }
                    ]
                }
                    |> Review.Test.run
            )
        ]


importExposingIsExplicitTests : Test
importExposingIsExplicitTests =
    Test.describe "ImportExposingIsExplicit"
        [ Test.test "allows import exposing (value)"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (Alias, Choice(..), function, value)

                            value =
                                ""
                            
                            function () =
                                value
                            
                            type Choice
                                = Variant
                            
                            type alias Alias =
                                String
                            """
                      }
                    , { path = "src/B.elm"
                      , source = """
                            module B exposing (b)

                            import A exposing (value)

                            b =
                                value
                            """
                      }
                    ]
                , review = ImportExposingIsExplicit.review
                , expectedErrors = []
                }
                    |> Review.Test.run
            )
        , Test.test "reports import exposing (..) with .. being from explicit exposing value, function, type alias, choice type"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (Alias, Choice(..), function, value)

                            value =
                                ""
                            
                            function () =
                                value
                            
                            type Choice
                                = Variant
                            
                            type alias Alias =
                                String
                            """
                      }
                    , { path = "src/B.elm"
                      , source = """
                            module B exposing (b)

                            import A exposing (..)

                            b =
                                value
                            """
                      }
                    ]
                , review = ImportExposingIsExplicit.review
                , expectedErrors =
                    [ { path = "src/B.elm"
                      , message = "import A exposes everything, not explicit"
                      , details =
                            [ "When you import everything from a module without explicitly listing what you actually need, it becomes harder to know where a reference comes from and which \"domain\" it belongs to for example."
                            , "Try using qualified imports like ModuleName.member or if that becomes too inconvenient, explicitly list what exposes you want to import for use without qualification. To start by explicitly listing all of the current members, accept the automatic fix and clean up from there."
                            ]
                      , range = Review.Test.Under ".."
                      , fixedFiles =
                            [ { path = "src/A.elm"
                              , source = """
                                    module B exposing (b)

                                    import A exposing (Alias, Choice(..), function, value)

                                    b =
                                        value
                                    """
                              }
                            ]
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "reports import exposing (..) with .. being from exposing everything being value, function, type alias, choice type"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (..)

                            value =
                                ""
                            
                            function () =
                                value
                            
                            type Choice
                                = Variant
                            
                            type alias Alias =
                                String
                            """
                      }
                    , { path = "src/B.elm"
                      , source = """
                            module B exposing (b)

                            import A exposing (..)

                            b =
                                value
                            """
                      }
                    ]
                , review = ImportExposingIsExplicit.review
                , expectedErrors =
                    [ { path = "src/B.elm"
                      , message = "import A exposes everything, not explicit"
                      , details =
                            [ "When you import everything from a module without explicitly listing what you actually need, it becomes harder to know where a reference comes from and which \"domain\" it belongs to for example."
                            , "Try using qualified imports like ModuleName.member or if that becomes too inconvenient, explicitly list what exposes you want to import for use without qualification. To start by explicitly listing all of the current members, accept the automatic fix and clean up from there."
                            ]
                      , range = Review.Test.Under ".."
                      , fixedFiles =
                            [ { path = "src/A.elm"
                              , source = """
                                    module B exposing (b)

                                    import A exposing (Alias, Choice(..), function, value)

                                    b =
                                        value
                                    """
                              }
                            ]
                      }
                    ]
                }
                    |> Review.Test.run
            )
        ]


moduleAndExposesAreUsedTests : Test
moduleAndExposesAreUsedTests =
    Test.describe "ModuleAndExposesAreUsed"
        [ Test.test "unused exposed value, usage fully qualified"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a, b)
                            a =
                                1
                            b =
                                2
                            """
                      }
                    , { path = "src/Main.elm"
                      , source = """
                            module Main exposing (main)
                            import A
                            main =
                                A.b
                            """
                      }
                    ]
                , review = ModuleAndExposesAreUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "expose A.a isn't used outside of this module"
                      , details =
                            [ "Since all exposed members aren't used outside of this module, the whole module is unused."
                            , """Unused code might be a sign that someone wanted to use it for something but didn't do so, yet.
But maybe you've since moved in a different direction,
in which case allowing the unused code to sit can make it harder to find what's important."""
                            , """If intended for determined future use, try gradually using it.
If intended as a very generic utility, try moving it into a package
(possibly local-only, using `Review.ignoreErrorsForPathsWhere (String.startsWith "your-local-package-source-directory")`).
If you think you don't need it anymore or think it was added it prematurely, you can remove it from the exposing part of the module header by applying the provided fix which might reveal its declaration as unused."""
                            ]
                      , range = Review.Test.UnderExactly { section = "a", startingAt = { row = 1, column = 20 } }
                      , fixedFiles =
                            [ { path = "src/A.elm"
                              , source = """
                                    module A exposing (b)
                                    a =
                                        1
                                    b =
                                        2
                                    """
                              }
                            ]
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "unused exposed value as the only expose"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)
                            a =
                                b ()
                            b () =
                                a
                            """
                      }
                    , { path = "src/Main.elm"
                      , source = """
                            module Main exposing (main)
                            main =
                                ""
                            """
                      }
                    ]
                , review = ModuleAndExposesAreUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "module A isn't used"
                      , details =
                            [ "Since all exposed members aren't used outside of this module, the whole module is unused."
                            , """Unused code might be a sign that someone wanted to use it for something but didn't do so, yet.
But maybe you've since moved in a different direction,
in which case allowing the unused code to sit can make it harder to find what's important."""
                            , """If intended for determined future use, try gradually using it.
If intended as a very generic utility, try moving it into a package
(possibly local-only, using `Review.ignoreErrorsForPathsWhere (String.startsWith "your-local-package-source-directory")`).
If you think you don't need it anymore or think it was added it prematurely, you can remove it manually."""
                            ]
                      , range = Review.Test.UnderExactly { section = "a", startingAt = { row = 1, column = 20 } }
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "unused exposed value, usage qualified by alias"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a, b)
                            a =
                                1
                            b =
                                2
                            """
                      }
                    , { path = "src/Main.elm"
                      , source = """
                            module Main exposing (main)
                            import A as Aa
                            main =
                                Aa.b
                            """
                      }
                    ]
                , review = ModuleAndExposesAreUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "expose A.a isn't used outside of this module"
                      , details =
                            [ "Since all exposed members aren't used outside of this module, the whole module is unused."
                            , """Unused code might be a sign that someone wanted to use it for something but didn't do so, yet.
But maybe you've since moved in a different direction,
in which case allowing the unused code to sit can make it harder to find what's important."""
                            , """If intended for determined future use, try gradually using it.
If intended as a very generic utility, try moving it into a package
(possibly local-only, using `Review.ignoreErrorsForPathsWhere (String.startsWith "your-local-package-source-directory")`).
If you think you don't need it anymore or think it was added it prematurely, you can remove it from the exposing part of the module header by applying the provided fix which might reveal its declaration as unused."""
                            ]
                      , range = Review.Test.UnderExactly { section = "a", startingAt = { row = 1, column = 20 } }
                      , fixedFiles =
                            [ { path = "src/A.elm"
                              , source = """
                                    module A exposing (b)
                                    a =
                                        1
                                    b =
                                        2
                                    """
                              }
                            ]
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "unused exposed value, usage by import exposing"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a, b)
                            a =
                                1
                            b =
                                2
                            """
                      }
                    , { path = "src/Main.elm"
                      , source = """
                            module Main exposing (main)
                            import A exposing (b)
                            main =
                                b
                            """
                      }
                    ]
                , review = ModuleAndExposesAreUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "expose A.a isn't used outside of this module"
                      , details =
                            [ "Since all exposed members aren't used outside of this module, the whole module is unused."
                            , """Unused code might be a sign that someone wanted to use it for something but didn't do so, yet.
But maybe you've since moved in a different direction,
in which case allowing the unused code to sit can make it harder to find what's important."""
                            , """If intended for determined future use, try gradually using it.
If intended as a very generic utility, try moving it into a package
(possibly local-only, using `Review.ignoreErrorsForPathsWhere (String.startsWith "your-local-package-source-directory")`).
If you think you don't need it anymore or think it was added it prematurely, you can remove it from the exposing part of the module header by applying the provided fix which might reveal its declaration as unused."""
                            ]
                      , range = Review.Test.UnderExactly { section = "a", startingAt = { row = 1, column = 20 } }
                      , fixedFiles =
                            [ { path = "src/A.elm"
                              , source = """
                                    module A exposing (b)
                                    a =
                                        1
                                    b =
                                        2
                                    """
                              }
                            ]
                      }
                    ]
                }
                    |> Review.Test.run
            )
        ]


patternVariablesAreUsedTests : Test
patternVariablesAreUsedTests =
    Test.describe "PatternVariableIsUsed"
        [ Test.test "used pattern variables are not reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                        module A exposing (a)

                        type A
                            = A String
                        
                        a x0 ( x1, x2 ) { x3 } =
                            \\(() as x4) ->
                                let
                                    (A x5) =
                                        A x0
                                    
                                    ( _, _, x6 ) =
                                        ( x3, x4, x5 )
                                    
                                    b (((x7))) =
                                        case x1 of
                                            [ x8 ] ->
                                                [ x2, x6, x7, x8 ]
                                            
                                            _ ->
                                                []
                                in
                                b x0
                        """
                      }
                    ]
                , review = PatternVariableIsUsed.review
                , expectedErrors = []
                }
                    |> Review.Test.run
            )
        , Test.test "unused function declaration argument pattern variable is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                        module A exposing (a)
                        
                        a unused =
                            ""
                        """
                      }
                    ]
                , review = PatternVariableIsUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "pattern variable unused isn't used"
                      , details = [ "Maybe you wanted to use this variable for something? If you don't need it, remove the variable here by applying the automatic fix." ]
                      , range = Review.Test.Under "unused"
                      , fixedFiles =
                            [ { path = "src/A.elm"
                              , source = """
                                    module A exposing (a)
                                    
                                    a _ =
                                        ""
                                    """
                              }
                            ]
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "unused function declaration argument pattern variable from single-field record is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                        module A exposing (a)
                        
                        a { unused } =
                            ""
                        """
                      }
                    ]
                , review = PatternVariableIsUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "pattern variable unused isn't used"
                      , details = [ "Maybe you wanted to use this variable for something? If you don't need it, remove the variable here by applying the automatic fix." ]
                      , range = Review.Test.Under "unused"
                      , fixedFiles =
                            [ { path = "src/A.elm"
                              , source = """
                                    module A exposing (a)
                                    
                                    a _ =
                                        ""
                                    """
                              }
                            ]
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "unused function declaration argument pattern variable as first from multi-field record is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                        module A exposing (a)
                        
                        a { unused, used } =
                            used
                        """
                      }
                    ]
                , review = PatternVariableIsUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "pattern variable unused isn't used"
                      , details = [ "Maybe you wanted to use this variable for something? If you don't need it, remove the variable here by applying the automatic fix." ]
                      , range = Review.Test.Under "unused"
                      , fixedFiles =
                            [ { path = "src/A.elm"
                              , source = """
                                    module A exposing (a)
                                    
                                    a { used } =
                                        used
                                    """
                              }
                            ]
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "unused function declaration argument pattern variable as last from multi-field record is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                        module A exposing (a)
                        
                        a { used, unused } =
                            used
                        """
                      }
                    ]
                , review = PatternVariableIsUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "pattern variable unused isn't used"
                      , details = [ "Maybe you wanted to use this variable for something? If you don't need it, remove the variable here by applying the automatic fix." ]
                      , range = Review.Test.Under "unused"
                      , fixedFiles =
                            [ { path = "src/A.elm"
                              , source = """
                                    module A exposing (a)
                                    
                                    a { used } =
                                        used
                                    """
                              }
                            ]
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "unused function declaration argument pattern variable from as is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                        module A exposing (a)
                        
                        a (() as unused) =
                            ""
                        """
                      }
                    ]
                , review = PatternVariableIsUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "pattern variable unused isn't used"
                      , details = [ "Maybe you wanted to use this variable for something? If you don't need it, remove the variable here by applying the automatic fix." ]
                      , range = Review.Test.Under "unused"
                      , fixedFiles =
                            [ { path = "src/A.elm"
                              , source = """
                                    module A exposing (a)
                                    
                                    a (()) =
                                        ""
                                    """
                              }
                            ]
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "unused lambda argument pattern variable is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                        module A exposing (a)
                        
                        a =
                            \\unused -> ""
                        """
                      }
                    ]
                , review = PatternVariableIsUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "pattern variable unused isn't used"
                      , details = [ "Maybe you wanted to use this variable for something? If you don't need it, remove the variable here by applying the automatic fix." ]
                      , range = Review.Test.Under "unused"
                      , fixedFiles =
                            [ { path = "src/A.elm"
                              , source = """
                                    module A exposing (a)
                                    
                                    a =
                                        \\_ -> ""
                                    """
                              }
                            ]
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "unused case argument pattern variable is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                        module A exposing (a)
                        
                        a =
                            case () of
                                unused ->
                                    ""
                        """
                      }
                    ]
                , review = PatternVariableIsUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "pattern variable unused isn't used"
                      , details = [ "Maybe you wanted to use this variable for something? If you don't need it, remove the variable here by applying the automatic fix." ]
                      , range = Review.Test.Under "unused"
                      , fixedFiles =
                            [ { path = "src/A.elm"
                              , source = """
                                    module A exposing (a)
                                    
                                    a =
                                        case () of
                                            _ ->
                                                ""
                                    """
                              }
                            ]
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "unused let destructured argument pattern variable is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                        module A exposing (a)
                        
                        a =
                            let
                                ( unused, _ ) =
                                    ( (), () )
                            in
                            ""
                        """
                      }
                    ]
                , review = PatternVariableIsUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "pattern variable unused isn't used"
                      , details = [ "Maybe you wanted to use this variable for something? If you don't need it, remove the variable here by applying the automatic fix." ]
                      , range = Review.Test.Under "unused"
                      , fixedFiles =
                            [ { path = "src/A.elm"
                              , source = """
                                    module A exposing (a)
                                    
                                    a =
                                        let
                                            ( _, _ ) =
                                                ( (), () )
                                        in
                                        ""
                                    """
                              }
                            ]
                      }
                    ]
                }
                    |> Review.Test.run
            )
        ]


debugIsNotUsedTests : Test
debugIsNotUsedTests =
    Test.describe "DebugIsNotUsed"
        [ Test.test "using variables named log, toString, todo are allowed"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)
                            
                            a todo =
                                \\toString ->
                                    let
                                        log =
                                            toString
                                    in
                                    [ log, todo ]
                            """
                      }
                    ]
                , review = DebugIsNotUsed.review
                , expectedErrors = []
                }
                    |> Review.Test.run
            )
        , Test.test "using log with import Debug exposing (log) and module-declared log is allowed"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)

                            import Debug exposing (log)

                            log =
                                identity
                            
                            a =
                                log ""
                            """
                      }
                    ]
                , review = DebugIsNotUsed.review
                , expectedErrors = []
                }
                    |> Review.Test.run
            )
        , Test.test "using log with import Debug exposing (log) with alias and module-declared log is allowed"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)

                            import Debug as DebugAlias exposing (log)

                            log =
                                identity
                            
                            a =
                                log ""
                            """
                      }
                    ]
                , review = DebugIsNotUsed.review
                , expectedErrors = []
                }
                    |> Review.Test.run
            )
        , Test.test "using \"Debug.log\" though unambiguous import alias to a module different than Debug is allowed"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)

                            import Debug as DebugAlias
                            import Debug2 as Debug
                            
                            a =
                                Debug.log
                            """
                      }
                    , { path = "src/Debug2.elm"
                      , source = """
                            module Debug2 exposing (log)
                            
                            log =
                                identity
                            """
                      }
                    ]
                , review = DebugIsNotUsed.review
                , expectedErrors = []
                }
                    |> Review.Test.run
            )
        , Test.test "using Debug.todo from implicit import is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)
                            
                            a =
                                Debug.todo ""
                            """
                      }
                    ]
                , review = DebugIsNotUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "Debug.todo is used"
                      , details =
                            [ "Debug.todo marks missing functionality which needs to be added gradually."
                            ]
                      , range = Review.Test.Under "Debug.todo"
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "using Debug.log from implicit import is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)
                            
                            a =
                                Debug.log ""
                            """
                      }
                    ]
                , review = DebugIsNotUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "Debug.log is used"
                      , details =
                            [ """Debug.log is a quick and dirty way to display an elm value in the console
and can for example be used to inspect private (opaque) types.
It's nothing a published product should make use of.
Using any `Debug` member also prevents compiling in optimized mode and publishing as a package."""
                            ]
                      , range = Review.Test.Under "Debug.log"
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "using Debug.toString from implicit import is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)
                            
                            a =
                                Debug.toString ""
                            """
                      }
                    ]
                , review = DebugIsNotUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "Debug.toString is used"
                      , details =
                            [ """Debug.toString is a quick and dirty way to display an elm value somewhere
and can for example be used to inspect private (opaque) types.
It's nothing a published product should make use of.
Using any `Debug` member also prevents compiling in optimized mode and publishing as a package."""
                            ]
                      , range = Review.Test.Under "Debug.toString"
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "using Debug.toString qualified by import alias is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)

                            import Debug as Dbg
                            
                            a =
                                Dbg.toString ""
                            """
                      }
                    ]
                , review = DebugIsNotUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "Debug.toString is used"
                      , details =
                            [ """Debug.toString is a quick and dirty way to display an elm value somewhere
and can for example be used to inspect private (opaque) types.
It's nothing a published product should make use of.
Using any `Debug` member also prevents compiling in optimized mode and publishing as a package."""
                            ]
                      , range = Review.Test.Under "Dbg.toString"
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "using unqualified Debug.toString from explicit import exposing without module value/function declaration with the same name is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)

                            import Debug exposing (toString)
                            
                            a =
                                toString ""
                            """
                      }
                    ]
                , review = DebugIsNotUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "Debug.toString is used"
                      , details =
                            [ """Debug.toString is a quick and dirty way to display an elm value somewhere
and can for example be used to inspect private (opaque) types.
It's nothing a published product should make use of.
Using any `Debug` member also prevents compiling in optimized mode and publishing as a package."""
                            ]
                      , range = Review.Test.UnderExactly { section = "toString", startingAt = { row = 6, column = 5 } }
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "using qualified Debug.toString from explicit import exposing and import alias and without module value/function declaration with the same name is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)

                            import Debug as Dbg exposing (toString)
                            
                            a =
                                Dbg.toString ""
                            """
                      }
                    ]
                , review = DebugIsNotUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "Debug.toString is used"
                      , details =
                            [ """Debug.toString is a quick and dirty way to display an elm value somewhere
and can for example be used to inspect private (opaque) types.
It's nothing a published product should make use of.
Using any `Debug` member also prevents compiling in optimized mode and publishing as a package."""
                            ]
                      , range = Review.Test.Under "Dbg.toString"
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "using unqualified Debug.toString from explicit import exposing with alias and without module value/function declaration with the same name is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)

                            import Debug as Dbg exposing (toString)
                            
                            a =
                                toString ""
                            """
                      }
                    ]
                , review = DebugIsNotUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "Debug.toString is used"
                      , details =
                            [ """Debug.toString is a quick and dirty way to display an elm value somewhere
and can for example be used to inspect private (opaque) types.
It's nothing a published product should make use of.
Using any `Debug` member also prevents compiling in optimized mode and publishing as a package."""
                            ]
                      , range = Review.Test.UnderExactly { section = "toString", startingAt = { row = 6, column = 5 } }
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "using qualified Debug.toString from explicit import exposing without module value/function declaration with the same name is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)

                            import Debug exposing (toString)
                            
                            a =
                                Debug.toString ""
                            """
                      }
                    ]
                , review = DebugIsNotUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "Debug.toString is used"
                      , details =
                            [ """Debug.toString is a quick and dirty way to display an elm value somewhere
and can for example be used to inspect private (opaque) types.
It's nothing a published product should make use of.
Using any `Debug` member also prevents compiling in optimized mode and publishing as a package."""
                            ]
                      , range = Review.Test.Under "Debug.toString"
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "using qualified Debug.toString from explicit import exposing (..) and import alias and without module value/function declaration with the same name is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)

                            import Debug as Dbg exposing (..)
                            
                            a =
                                Dbg.toString ""
                            """
                      }
                    ]
                , review = DebugIsNotUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "Debug.toString is used"
                      , details =
                            [ """Debug.toString is a quick and dirty way to display an elm value somewhere
and can for example be used to inspect private (opaque) types.
It's nothing a published product should make use of.
Using any `Debug` member also prevents compiling in optimized mode and publishing as a package."""
                            ]
                      , range = Review.Test.Under "Dbg.toString"
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "using unqualified Debug.toString from import exposing (..) with alias and without module value/function declaration with the same name is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)

                            import Debug as Dbg exposing (..)
                            
                            a =
                                toString ""
                            """
                      }
                    ]
                , review = DebugIsNotUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "Debug.toString is used"
                      , details =
                            [ """Debug.toString is a quick and dirty way to display an elm value somewhere
and can for example be used to inspect private (opaque) types.
It's nothing a published product should make use of.
Using any `Debug` member also prevents compiling in optimized mode and publishing as a package."""
                            ]
                      , range = Review.Test.UnderExactly { section = "toString", startingAt = { row = 6, column = 5 } }
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "using unqualified Debug.toString from import exposing (..) without module value/function declaration with the same name is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)

                            import Debug exposing (..)
                            
                            a =
                                toString ""
                            """
                      }
                    ]
                , review = DebugIsNotUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "Debug.toString is used"
                      , details =
                            [ """Debug.toString is a quick and dirty way to display an elm value somewhere
and can for example be used to inspect private (opaque) types.
It's nothing a published product should make use of.
Using any `Debug` member also prevents compiling in optimized mode and publishing as a package."""
                            ]
                      , range = Review.Test.UnderExactly { section = "toString", startingAt = { row = 6, column = 5 } }
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        ]


commentDoesNotUseCertainWordsTests : Test
commentDoesNotUseCertainWordsTests =
    Test.describe "CommentDoesNotUseCertainWords"
        [ Test.test "comments without marks are allowed"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            port module A exposing (a)

                            {-| module documentation
                            -}

                            import Json.Encode

                            {-| port documentation
                            -}
                            port fromJs : (Json.Encode.Value -> event) -> Cmd event

                            {-| value documentation
                            -}
                            a =
                                -- single-line
                                ""

                            {-| function documentation
                            -}
                            b x =
                                {- multi-line -}
                                ""

                            {-| type alias documentation
                            -}
                            type alias TypeAlias =
                                String

                            {-| choice type documentation
                            -}
                            type ChoiceType
                                = A
                            """
                      }
                    ]
                , review = CommentDoesNotUseCertainMarks.review [ "TODO" ]
                , expectedErrors = []
                }
                    |> Review.Test.run
            )
        , Test.test "single-line comment with mark is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            port module A exposing (a)

                            a =
                                -- single-line TODO
                                ""
                            """
                      }
                    ]
                , review = CommentDoesNotUseCertainMarks.review [ "TODO" ]
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "comment uses TODO mark"
                      , details = [ "This mark has been placed in a comment for future notice. Read the comment carefully and decide what you want to do. Once you're done, remove the notice." ]
                      , range = Review.Test.Under "TODO"
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "multi-line comment with mark is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            port module A exposing (a)
                            
                            a =
                                {-
                                  multi-line
                                  TODO hey, listen!
                                -}
                                ""
                            """
                      }
                    ]
                , review = CommentDoesNotUseCertainMarks.review [ "TODO" ]
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "comment uses TODO mark"
                      , details = [ "This mark has been placed in a comment for future notice. Read the comment carefully and decide what you want to do. Once you're done, remove the notice." ]
                      , range = Review.Test.Under "TODO"
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "module documentation comment with mark is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            port module A exposing (a)

                            {-| module documentation
                            TODO hey, listen!
                            -}

                            import Json.Encode
                            
                            a =
                                ""
                            """
                      }
                    ]
                , review = CommentDoesNotUseCertainMarks.review [ "TODO" ]
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "comment uses TODO mark"
                      , details = [ "This mark has been placed in a comment for future notice. Read the comment carefully and decide what you want to do. Once you're done, remove the notice." ]
                      , range = Review.Test.Under "TODO"
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "value documentation comment with mark is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            port module A exposing (a)

                            {-| -}

                            import Json.Encode
                            
                            {-| value documentation
                            TODO hey, listen!
                            -}
                            a =
                                ""
                            """
                      }
                    ]
                , review = CommentDoesNotUseCertainMarks.review [ "TODO" ]
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "comment uses TODO mark"
                      , details = [ "This mark has been placed in a comment for future notice. Read the comment carefully and decide what you want to do. Once you're done, remove the notice." ]
                      , range = Review.Test.Under "TODO"
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "function documentation comment with mark is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            port module A exposing (a)

                            {-| -}

                            import Json.Encode
                            
                            {-| function documentation
                            TODO hey, listen!
                            -}
                            a x =
                                x
                            """
                      }
                    ]
                , review = CommentDoesNotUseCertainMarks.review [ "TODO" ]
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "comment uses TODO mark"
                      , details = [ "This mark has been placed in a comment for future notice. Read the comment carefully and decide what you want to do. Once you're done, remove the notice." ]
                      , range = Review.Test.Under "TODO"
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "type alias documentation comment with mark is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            port module A exposing (A)

                            {-| -}

                            import Json.Encode
                            
                            {-| type alias documentation
                            TODO hey, listen!
                            -}
                            type alias A =
                                String
                            """
                      }
                    ]
                , review = CommentDoesNotUseCertainMarks.review [ "TODO" ]
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "comment uses TODO mark"
                      , details = [ "This mark has been placed in a comment for future notice. Read the comment carefully and decide what you want to do. Once you're done, remove the notice." ]
                      , range = Review.Test.Under "TODO"
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "choice type documentation comment with mark is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            port module A exposing (A)

                            {-| -}

                            import Json.Encode
                            
                            {-| choice type documentation
                            TODO hey, listen!
                            -}
                            type A
                                = A
                            """
                      }
                    ]
                , review = CommentDoesNotUseCertainMarks.review [ "TODO" ]
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "comment uses TODO mark"
                      , details = [ "This mark has been placed in a comment for future notice. Read the comment carefully and decide what you want to do. Once you're done, remove the notice." ]
                      , range = Review.Test.Under "TODO"
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "port documentation comment with mark is reported"
            (\() ->
                { projectConfiguration = Review.Test.applicationConfigurationMinimal
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            port module A exposing (A)

                            {-| -}

                            import Json.Encode
                            
                            {-| port documentation
                            hey, listen! TODO
                            -}
                            port fromJs : (Json.Encode.Value -> event) -> Cmd event
                            """
                      }
                    ]
                , review = CommentDoesNotUseCertainMarks.review [ "TODO" ]
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "comment uses TODO mark"
                      , details = [ "This mark has been placed in a comment for future notice. Read the comment carefully and decide what you want to do. Once you're done, remove the notice." ]
                      , range = Review.Test.Under "TODO"
                      , fixedFiles = []
                      }
                    ]
                }
                    |> Review.Test.run
            )
        ]
