module Tests exposing (tests)

import ExposesAreUsedTests
import Review.Test
import Test exposing (Test)
import UnderscoreInModuleNameForbid


tests : Test
tests =
    Test.describe "Review.Test"
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
                , review = UnderscoreInModuleNameForbid.review
                , expectedErrors = []
                }
                    |> Review.Test.run
            )
        , Test.test "single module with error"
            (\() ->
                { projectConfig = Review.Test.applicationConfigAfterElmInit
                , files =
                    [ { path = "src/A.elm"
                      , source = """
                            module A_ exposing (a)
                            a =
                                ""
                            """
                      }
                    ]
                , review = UnderscoreInModuleNameForbid.review
                , expectedErrors =
                    [ { path = "src/A.elm"
                      , message = "module name contains _"
                      , details = [ "By convention, elm modules names use Pascal case (like `MyModuleName`). Please rename your module using this format." ]
                      , range = Review.Test.Under "A_"
                      , fixedSource = Nothing
                      }
                    ]
                }
                    |> Review.Test.run
            )
        , ExposesAreUsedTests.tests
        ]
