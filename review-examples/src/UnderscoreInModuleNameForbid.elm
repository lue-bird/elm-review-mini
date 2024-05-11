module UnderscoreInModuleNameForbid exposing (review)

import Dict
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Review exposing (Review)


review : Review
review =
    Review.create
        { inspect =
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
        , knowledgeMerge = \a b -> Dict.union a b
        , report =
            \knowledge ->
                knowledge
                    |> Dict.toList
                    |> List.map
                        (\( path, moduleNameWithUnderscore ) ->
                            { path = path
                            , message = "module name contains _"
                            , details = [ "By convention, elm modules names use Pascal case (like `MyModuleName`). Please rename your module using this format." ]
                            , range = moduleNameWithUnderscore.range
                            , fix = []
                            }
                        )
        }
