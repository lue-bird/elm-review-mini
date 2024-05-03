module Tests exposing (suite)

import Codec
import Dict
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Review exposing (Review)
import Review.Test
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Review.Test"
        [ Test.test "single module without errors"
            (\() ->
                { elmJson = """
                    {
                        "type": "application",
                        "source-directories": [
                            "src"
                        ],
                        "elm-version": "0.19.1",
                        "dependencies": {
                            "direct": {
                                "elm/core": "1.0.5"
                            },
                            "indirect": {
                                "elm/json": "1.1.3"
                            }
                        },
                        "test-dependencies": {
                            "direct": {},
                            "indirect": {}
                        }
                    }
                    """
                , directDependencies = []
                , modules =
                    [ { path = "src/A.elm"
                      , source = """
                            module A exposing (a)
                            a =
                                ""
                            """
                      }
                    ]
                , extraFiles =
                    []
                }
                    |> Review.Test.run
                        { review = underscoreInModuleNameForbid
                        , expectedErrors = []
                        }
            )
        , Test.test "single module with error"
            (\() ->
                { elmJson = """
                    {
                        "type": "application",
                        "source-directories": [
                            "src"
                        ],
                        "elm-version": "0.19.1",
                        "dependencies": {
                            "direct": {
                                "elm/core": "1.0.5"
                            },
                            "indirect": {
                                "elm/json": "1.1.3"
                            }
                        },
                        "test-dependencies": {
                            "direct": {},
                            "indirect": {}
                        }
                    }
                    """
                , directDependencies = []
                , modules =
                    [ { path = "src/A.elm"
                      , source = """
                            module A_ exposing (a)
                            a =
                                ""
                            """
                      }
                    ]
                , extraFiles =
                    []
                }
                    |> Review.Test.run
                        { review = underscoreInModuleNameForbid
                        , expectedErrors =
                            [ { path = "src/A.elm"
                              , errors =
                                    [ { message = "_ in a module name"
                                      , details = [ "By convention, elm modules names use Pascal case (like `MyModuleName`). Please rename your module using this format." ]
                                      , range = Review.Test.Under "A_"
                                      , fixedSource = Nothing
                                      }
                                    ]
                              }
                            ]
                        }
            )
        ]


underscoreInModuleNameForbid : Review
underscoreInModuleNameForbid =
    Review.create
        { name = "NoUnderscoreInModuleName"
        , inspect =
            [ Review.inspectModule
                (\moduleData ->
                    let
                        moduleNameNode : Elm.Syntax.Node.Node Elm.Syntax.ModuleName.ModuleName
                        moduleNameNode =
                            moduleData.syntax.moduleDefinition |> Review.moduleHeaderNameNode

                        moduleNameString : String
                        moduleNameString =
                            moduleNameNode
                                |> Elm.Syntax.Node.value
                                |> String.join "."
                    in
                    if moduleNameString |> String.contains "_" then
                        Dict.singleton moduleData.path
                            { range = moduleNameNode |> Elm.Syntax.Node.range
                            }

                    else
                        Dict.empty
                )
            ]
        , contextMerge = \a b -> Dict.union a b
        , contextCodec =
            Codec.dict
                (Codec.object (\range -> { range = range })
                    |> Codec.field "range" .range Review.rangeCodec
                    |> Codec.buildObject
                )
        , report =
            \context ->
                context
                    |> Dict.toList
                    |> List.map
                        (\( path, moduleNameWithUnderscore ) ->
                            { target = Review.FileTarget { path = path }
                            , message = "_ in a module name"
                            , details = [ "By convention, elm modules names use Pascal case (like `MyModuleName`). Please rename your module using this format." ]
                            , range = moduleNameWithUnderscore.range
                            , fixes = []
                            }
                        )
        }
