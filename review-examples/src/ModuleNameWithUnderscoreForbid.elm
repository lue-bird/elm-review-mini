module ModuleNameWithUnderscoreForbid exposing (review)

import Dict
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Review


review : Review.Review
review =
    Review.create
        { inspect =
            [ Review.inspectModule
                (\moduleData ->
                    let
                        moduleNameNode : Elm.Syntax.Node.Node Elm.Syntax.ModuleName.ModuleName
                        moduleNameNode =
                            moduleData.syntax.moduleDefinition |> moduleHeaderNameNode

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


moduleHeaderNameNode : Elm.Syntax.Node.Node Elm.Syntax.Module.Module -> Elm.Syntax.Node.Node Elm.Syntax.ModuleName.ModuleName
moduleHeaderNameNode =
    \(Elm.Syntax.Node.Node _ moduleHeader) ->
        case moduleHeader of
            Elm.Syntax.Module.NormalModule data ->
                data.moduleName

            Elm.Syntax.Module.PortModule data ->
                data.moduleName

            Elm.Syntax.Module.EffectModule data ->
                data.moduleName
