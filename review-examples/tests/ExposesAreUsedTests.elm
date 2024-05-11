module ExposesAreUsedTests exposing (tests)

import ExposesAreUsed
import Review.Test
import Test exposing (Test)


tests : Test
tests =
    Test.describe "ExposesAreUsed"
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
                , review = ExposesAreUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "expose A.a isn't used outside of this module"
                      , details = [ "Either use it or remove it from the exposing part of the module header which might reveal its declaration as unused." ]
                      , range = Review.Test.UnderExactly { section = "a", startingAt = { row = 1, column = 20 } }
                      , fixedSource = """
                                    module A exposing (b)
                                    a =
                                        1
                                    b =
                                        2
                                    """ |> Just
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
                , review = ExposesAreUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "expose A.a isn't used outside of this module"
                      , details = [ "Either use it or remove it from the exposing part of the module header which might reveal its declaration as unused." ]
                      , range = Review.Test.UnderExactly { section = "a", startingAt = { row = 1, column = 20 } }
                      , fixedSource = """
                                    module A exposing (b)
                                    a =
                                        1
                                    b =
                                        2
                                    """ |> Just
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
                , review = ExposesAreUsed.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "expose A.a isn't used outside of this module"
                      , details = [ "Either use it or remove it from the exposing part of the module header which might reveal its declaration as unused." ]
                      , range = Review.Test.UnderExactly { section = "a", startingAt = { row = 1, column = 20 } }
                      , fixedSource = """
                                    module A exposing (b)
                                    a =
                                        1
                                    b =
                                        2
                                    """ |> Just
                      }
                    ]
                }
                    |> Review.Test.run
            )
        ]
