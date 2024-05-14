module Tests exposing (tests)

import ImportExposingIsExplicit
import LetValueOrFunctionIsTypeAnnotated
import ModuleAndExposesAreUsed
import ModuleExposingIsExplicit
import ModuleNameWithUnderscoreForbid
import ModuleValueOrFunctionIsTypeAnnotated
import Review
import Review.Test
import Test exposing (Test)


tests : Test
tests =
    Test.describe "example reviews"
        [ moduleNameWithUnderscoreForbidTests
        , moduleValueOrFunctionIsTypeAnnotatedTests
        , letValueOrFunctionIsTypeAnnotatedTests
        , exposesAreUsedTests
        , moduleExposingIsExplicitTests
        , importExposingIsExplicitTests
        ]


moduleNameWithUnderscoreForbidTests : Test
moduleNameWithUnderscoreForbidTests =
    Test.describe "ModuleNameWithUnderscoreForbid"
        [ Test.test "single module without errors"
            (\() ->
                { projectConfig = Review.Test.applicationConfigAfterElmInit
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
                { projectConfig = Review.Test.applicationConfigAfterElmInit
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
                      , fixedSource = Nothing
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
                { projectConfig = Review.Test.applicationConfigAfterElmInit
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
                { projectConfig = Review.Test.applicationConfigAfterElmInit
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
                      , fixedSource = Nothing
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
                { projectConfig = Review.Test.applicationConfigAfterElmInit
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
                { projectConfig = Review.Test.applicationConfigAfterElmInit
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
                      , fixedSource = Nothing
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
                { projectConfig = Review.Test.applicationConfigAfterElmInit
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
                { projectConfig = Review.Test.applicationConfigAfterElmInit
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
                      , fixedSource = Just """
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
                    |> Review.Test.run
            )
        ]


importExposingIsExplicitTests : Test
importExposingIsExplicitTests =
    Test.describe "ImportExposingIsExplicit"
        [ Test.test "allows import exposing (value)"
            (\() ->
                { projectConfig = Review.Test.applicationConfigAfterElmInit
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
                { projectConfig = Review.Test.applicationConfigAfterElmInit
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
                      , fixedSource = Just """
                            module B exposing (b)

                            import A exposing (Alias, Choice(..), function, value)

                            b =
                                value
                            """
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "reports import exposing (..) with .. being from exposing everything being value, function, type alias, choice type"
            (\() ->
                { projectConfig = Review.Test.applicationConfigAfterElmInit
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
                      , fixedSource = Just """
                            module B exposing (b)

                            import A exposing (Alias, Choice(..), function, value)

                            b =
                                value
                            """
                      }
                    ]
                }
                    |> Review.Test.run
            )
        ]


exposesAreUsedTests : Test
exposesAreUsedTests =
    Test.describe "ModuleAndExposesAreUsed"
        [ Test.test "unused exposed value, usage fully qualified"
            (\() ->
                { projectConfig = Review.Test.applicationConfigAfterElmInit
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
                      , fixedSource = Just """
                            module A exposing (b)
                            a =
                                1
                            b =
                                2
                            """
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "unused exposed value as the only expose"
            (\() ->
                { projectConfig = Review.Test.applicationConfigAfterElmInit
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
                      , fixedSource = Nothing
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "unused exposed value, usage qualified by alias"
            (\() ->
                { projectConfig = Review.Test.applicationConfigAfterElmInit
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
                      , fixedSource = Just """
                            module A exposing (b)
                            a =
                                1
                            b =
                                2
                            """
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , Test.test "unused exposed value, usage by import exposing"
            (\() ->
                { projectConfig = Review.Test.applicationConfigAfterElmInit
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
                      , fixedSource = Just """
                            module A exposing (b)
                            a =
                                1
                            b =
                                2
                            """
                      }
                    ]
                }
                    |> Review.Test.run
            )
        ]
