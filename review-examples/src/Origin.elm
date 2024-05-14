module Origin exposing (ResourcesForEasyLookup, bundleResourcesForEasyLookup, determine)

import Elm.Syntax.Exposing
import Elm.Syntax.Import
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import FastDict
import FastDict.LocalExtra
import Set exposing (Set)
import Set.LocalExtra


determine : ResourcesForEasyLookup -> (( Elm.Syntax.ModuleName.ModuleName, String ) -> Maybe Elm.Syntax.ModuleName.ModuleName)
determine imports =
    \( qualification, unqualifiedName ) ->
        case imports |> FastDict.get qualification of
            Just _ ->
                qualification |> Just

            Nothing ->
                let
                    maybeOriginByAlias : Maybe Elm.Syntax.ModuleName.ModuleName
                    maybeOriginByAlias =
                        imports
                            |> FastDict.LocalExtra.firstJustMap
                                (\importModuleName import_ ->
                                    case import_.alias of
                                        Nothing ->
                                            Nothing

                                        Just alias ->
                                            if qualification == [ alias ] then
                                                importModuleName |> Just

                                            else
                                                Nothing
                                )
                in
                case maybeOriginByAlias of
                    Just aliasOriginModuleName ->
                        aliasOriginModuleName |> Just

                    Nothing ->
                        case qualification of
                            [] ->
                                imports
                                    |> FastDict.LocalExtra.firstJustMap
                                        (\importModuleName import_ ->
                                            if import_.exposed |> Set.member unqualifiedName then
                                                importModuleName |> Just

                                            else
                                                Nothing
                                        )

                            _ :: _ ->
                                Nothing


type alias ResourcesForEasyLookup =
    FastDict.Dict
        Elm.Syntax.ModuleName.ModuleName
        { alias : Maybe String
        , exposed : Set String -- includes names of variants
        }


{-| Create [`ResourcesForEasyLookup`](#ResourcesForEasyLookup) from given imports.
-}
bundleResourcesForEasyLookup :
    FastDict.Dict
        Elm.Syntax.ModuleName.ModuleName
        { exposedTypesWithVariantNames : FastDict.Dict String (Set String)
        , exposedSimpleNames : Set String
        }
    -> List Elm.Syntax.Import.Import
    -> ResourcesForEasyLookup
bundleResourcesForEasyLookup byModule syntaxImports =
    List.foldl
        (\import_ importsSoFar ->
            let
                moduleName : Elm.Syntax.ModuleName.ModuleName
                moduleName =
                    import_ |> .moduleName |> Elm.Syntax.Node.value

                importInfo : { exposed : Set String, alias : Maybe String }
                importInfo =
                    import_
                        |> importKnowledge
                            (byModule
                                |> FastDict.get moduleName
                                |> Maybe.withDefault
                                    { exposedTypesWithVariantNames = FastDict.empty
                                    , exposedSimpleNames = Set.empty
                                    }
                            )
            in
            importsSoFar
                |> insert moduleName
                    { alias = importInfo.alias, exposed = importInfo.exposed }
        )
        implicitImports
        syntaxImports


{-| From the `elm/core` readme:

>
> ### Default Imports

> The modules in this package are so common, that some of them are imported by default in all Elm files. So it is as if every Elm file starts with these imports:
>
>     import Basics exposing (..)
>     import List exposing (List, (::))
>     import Maybe exposing (Maybe(..))
>     import Result exposing (Result(..))
>     import String exposing (String)
>     import Char exposing (Char)
>     import Tuple
>     import Debug
>     import Platform exposing (Program)
>     import Platform.Cmd as Cmd exposing (Cmd)
>     import Platform.Sub as Sub exposing (Sub)

-}
implicitImports : ResourcesForEasyLookup
implicitImports =
    [ ( [ "Basics" ]
      , { alias = Nothing
        , exposed =
            [ "Int"
            , "Float"
            , "+"
            , "-"
            , "*"
            , "/"
            , "//"
            , "^"
            , "toFloat"
            , "round"
            , "floor"
            , "ceiling"
            , "truncate"
            , "=="
            , "/="
            , "<"
            , ">"
            , "<="
            , ">="
            , "max"
            , "min"
            , "compare"
            , "Order"
            , "LT"
            , "EQ"
            , "GT"
            , "Bool"
            , "True"
            , "False"
            , "not"
            , "&&"
            , "||"
            , "xor"
            , "++"
            , "modBy"
            , "remainderBy"
            , "negate"
            , "abs"
            , "clamp"
            , "sqrt"
            , "logBase"
            , "e"
            , "pi"
            , "cos"
            , "sin"
            , "tan"
            , "acos"
            , "asin"
            , "atan"
            , "atan2"
            , "degrees"
            , "radians"
            , "turns"
            , "toPolar"
            , "fromPolar"
            , "isNaN"
            , "isInfinite"
            , "identity"
            , "always"
            , "<|"
            , "|>"
            , "<<"
            , ">>"
            , "Never"
            , "never"
            ]
                |> Set.fromList
        }
      )
    , -- exposing (List) excluded because List.List is not valid.
      -- List is not exposed from module List
      -- and is instead provided through compiler magic
      ( [ "List" ], { alias = Nothing, exposed = Set.singleton "(::)" } )
    , ( [ "Maybe" ], { alias = Nothing, exposed = Set.fromList [ "Maybe", "Just", "Nothing" ] } )
    , ( [ "Result" ], { alias = Nothing, exposed = Set.fromList [ "Result", "Ok", "Err" ] } )
    , ( [ "String" ], { alias = Nothing, exposed = Set.singleton "String" } )
    , ( [ "Char" ], { alias = Nothing, exposed = Set.singleton "Char" } )
    , ( [ "Tuple" ], { alias = Nothing, exposed = Set.empty } )
    , ( [ "Debug" ], { alias = Nothing, exposed = Set.empty } )
    , ( [ "Platform" ], { alias = Nothing, exposed = Set.singleton "Program" } )
    , ( [ "Platform", "Cmd" ], { alias = Just "Cmd", exposed = Set.singleton "Cmd" } )
    , ( [ "Platform", "Sub" ], { alias = Just "Sub", exposed = Set.singleton "Sub" } )
    ]
        |> FastDict.fromList


importKnowledge :
    { exposedTypesWithVariantNames : FastDict.Dict String (Set String)
    , exposedSimpleNames : Set String
    }
    -> (Elm.Syntax.Import.Import -> { exposed : Set String, alias : Maybe String })
importKnowledge moduleExposes import_ =
    { alias = import_.moduleAlias |> Maybe.map (\(Elm.Syntax.Node.Node _ parts) -> parts |> String.join ".")
    , exposed =
        case import_.exposingList of
            Nothing ->
                Set.empty

            Just (Elm.Syntax.Node.Node _ existingExposing) ->
                case existingExposing of
                    Elm.Syntax.Exposing.All _ ->
                        [ moduleExposes.exposedSimpleNames
                        , moduleExposes |> .exposedTypesWithVariantNames |> FastDict.keys |> Set.fromList
                        , moduleExposes |> .exposedTypesWithVariantNames |> FastDict.values |> Set.LocalExtra.unionFromList
                        ]
                            |> Set.LocalExtra.unionFromList

                    Elm.Syntax.Exposing.Explicit exposes ->
                        exposes
                            |> List.foldl
                                (\(Elm.Syntax.Node.Node _ expose) soFar ->
                                    case expose of
                                        Elm.Syntax.Exposing.FunctionExpose exposeValueReferenceName ->
                                            soFar |> Set.insert exposeValueReferenceName

                                        Elm.Syntax.Exposing.TypeOrAliasExpose typeName ->
                                            soFar |> Set.insert typeName

                                        Elm.Syntax.Exposing.InfixExpose symbol ->
                                            soFar |> Set.insert symbol

                                        Elm.Syntax.Exposing.TypeExpose typeExpose ->
                                            Set.union
                                                soFar
                                                (moduleExposes
                                                    |> .exposedTypesWithVariantNames
                                                    |> FastDict.get typeExpose.name
                                                    |> Maybe.withDefault Set.empty
                                                    |> Set.insert typeExpose.name
                                                )
                                )
                                Set.empty
    }


insert : Elm.Syntax.ModuleName.ModuleName -> { alias : Maybe String, exposed : Set String } -> (ResourcesForEasyLookup -> ResourcesForEasyLookup)
insert moduleName importInfoToAdd imports =
    FastDict.update moduleName
        (\existingImport ->
            let
                newImportInfo : { alias : Maybe String, exposed : Set String }
                newImportInfo =
                    case existingImport of
                        Nothing ->
                            importInfoToAdd

                        Just import_ ->
                            { alias =
                                case import_.alias of
                                    Just alias ->
                                        alias |> Just

                                    Nothing ->
                                        importInfoToAdd.alias
                            , exposed = Set.union import_.exposed importInfoToAdd.exposed
                            }
            in
            Just newImportInfo
        )
        imports
