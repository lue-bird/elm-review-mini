module DebugIsNotUsed exposing (review)

{-|

@docs review

-}

import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.ModuleName
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Range
import Expression.LocalExtra
import FastDict
import FastDict.LocalExtra
import List.LocalExtra
import Review
import Set exposing (Set)


{-| Report using a member of [the `Debug` module](https://dark.elm.dmy.fr/packages/elm/core/latest/Debug)

  - `Debug.todo` marks missing functionality which needs to be added gradually
  - `Debug.log` is a quick and dirty way to display an elm value in the console
    and can for example be used to inspect private (opaque) types.
    It's nothing a published product should make use of.
  - `Debug.toString` is a quick and dirty way to display an elm value anywhere,
    similar to `Debug.log`

Using any `Debug` member also prevents compiling in optimized mode and publishing as a package.


### reported

    a =
        "" |> Debug.log "text"

    b =
        Debug.todo ""

    c =
        1 |> Debug.toString


### not reported

    a =
        consoleWarnPort "text"

    c =
        1 |> String.fromInt

-}
review : Review.Review
review =
    Review.create
        { inspect =
            [ Review.inspectModule moduleDataToKnowledge
            ]
        , knowledgeMerge = knowledgeMerge
        , report = report
        }


type alias Knowledge =
    { debugLogUses : List { modulePath : String, referenceRange : Elm.Syntax.Range.Range }
    , debugTodoUses : List { modulePath : String, referenceRange : Elm.Syntax.Range.Range }
    , debugToStringUses : List { modulePath : String, referenceRange : Elm.Syntax.Range.Range }
    }


moduleDataToKnowledge :
    { data_ | path : String, syntax : Elm.Syntax.File.File }
    -> Knowledge
moduleDataToKnowledge moduleData =
    let
        maybeDebugImport : Maybe { alias : Maybe String, exposes : List String }
        maybeDebugImport =
            moduleData.syntax.imports
                |> List.LocalExtra.firstJustMap
                    (\(Elm.Syntax.Node.Node _ import_) ->
                        case import_.moduleName of
                            Elm.Syntax.Node.Node _ [ "Debug" ] ->
                                Just
                                    { alias = import_.moduleAlias |> Maybe.map (\(Elm.Syntax.Node.Node _ alias) -> alias |> String.join ".")
                                    , exposes =
                                        case import_.exposingList |> Maybe.map Elm.Syntax.Node.value of
                                            Nothing ->
                                                []

                                            Just (Elm.Syntax.Exposing.Explicit explicitExposes) ->
                                                explicitExposes
                                                    |> List.filterMap
                                                        (\(Elm.Syntax.Node.Node _ expose) ->
                                                            case expose of
                                                                Elm.Syntax.Exposing.FunctionExpose valueOrFunctionName ->
                                                                    Just valueOrFunctionName

                                                                Elm.Syntax.Exposing.TypeExpose choiceTypeExpose ->
                                                                    Nothing

                                                                Elm.Syntax.Exposing.TypeOrAliasExpose exposeName ->
                                                                    Nothing

                                                                Elm.Syntax.Exposing.InfixExpose symbol ->
                                                                    Nothing
                                                        )

                                            Just (Elm.Syntax.Exposing.All _) ->
                                                [ "log", "todo", "toString" ]
                                    }

                            _ ->
                                Nothing
                    )
    in
    let
        moduleDeclaredValueOrFunctionNames : Set String
        moduleDeclaredValueOrFunctionNames =
            moduleData.syntax.declarations
                |> List.LocalExtra.justsToSetMap
                    (\(Elm.Syntax.Node.Node _ declaration) ->
                        case declaration of
                            Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
                                valueOrFunctionDeclaration.declaration
                                    |> Elm.Syntax.Node.value
                                    |> .name
                                    |> Elm.Syntax.Node.value
                                    |> Just

                            _ ->
                                Nothing
                    )

        referenceIsDebug debugFunctionName =
            \( qualification, unqualified ) ->
                (unqualified == debugFunctionName)
                    && (case maybeDebugImport of
                            -- implicit import
                            Nothing ->
                                qualification == [ "Debug" ]

                            Just debugImport ->
                                let
                                    maximumQualification : Elm.Syntax.ModuleName.ModuleName
                                    maximumQualification =
                                        case debugImport.alias of
                                            Just debugImportAlias ->
                                                [ debugImportAlias ]

                                            Nothing ->
                                                [ "Debug" ]
                                in
                                (qualification == maximumQualification)
                                    || ((debugImport.exposes |> List.member debugFunctionName)
                                            && not (moduleDeclaredValueOrFunctionNames |> Set.member debugFunctionName)
                                            && (qualification == [])
                                       )
                       )

        moduleValueOrFunctionDeclarationExpressions : FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) (List Elm.Syntax.Range.Range)
        moduleValueOrFunctionDeclarationExpressions =
            moduleData.syntax.declarations
                |> FastDict.LocalExtra.unionFromListWithMap
                    (\(Elm.Syntax.Node.Node _ declaration) ->
                        case declaration of
                            Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
                                valueOrFunctionDeclaration.declaration
                                    |> Elm.Syntax.Node.value
                                    |> .expression
                                    |> Expression.LocalExtra.identifiers
                                    |> FastDict.LocalExtra.excludeKeys
                                        (moduleDeclaredValueOrFunctionNames
                                            |> Set.map (\unqualified -> ( [], unqualified ))
                                        )

                            _ ->
                                FastDict.empty
                    )
                    (\a b -> a ++ b)
    in
    { debugLogUses =
        moduleValueOrFunctionDeclarationExpressions
            |> FastDict.filter (\reference _ -> reference |> referenceIsDebug "log")
            |> FastDict.values
            |> List.concat
            |> List.map
                (\referenceRange ->
                    { referenceRange = referenceRange, modulePath = moduleData.path }
                )
    , debugTodoUses =
        moduleValueOrFunctionDeclarationExpressions
            |> FastDict.filter (\reference _ -> reference |> referenceIsDebug "todo")
            |> FastDict.values
            |> List.concat
            |> List.map
                (\referenceRange ->
                    { referenceRange = referenceRange, modulePath = moduleData.path }
                )
    , debugToStringUses =
        moduleValueOrFunctionDeclarationExpressions
            |> FastDict.filter (\reference _ -> reference |> referenceIsDebug "toString")
            |> FastDict.values
            |> List.concat
            |> List.map
                (\referenceRange ->
                    { referenceRange = referenceRange, modulePath = moduleData.path }
                )
    }


knowledgeMerge : Knowledge -> Knowledge -> Knowledge
knowledgeMerge a b =
    { debugLogUses = a.debugLogUses ++ b.debugLogUses
    , debugTodoUses = a.debugTodoUses ++ b.debugTodoUses
    , debugToStringUses = a.debugToStringUses ++ b.debugToStringUses
    }


report : Knowledge -> List Review.Error
report knowledge =
    [ knowledge.debugLogUses
        |> List.map
            (\debugLogUse ->
                { path = debugLogUse.modulePath
                , message = "Debug.log is used"
                , details =
                    [ """Debug.log is a quick and dirty way to display an elm value in the console
and can for example be used to inspect private (opaque) types.
It's nothing a published product should make use of.
Using any `Debug` member also prevents compiling in optimized mode and publishing as a package."""
                    ]
                , range = debugLogUse.referenceRange
                , fix = [] -- TODO consider replacing Debug.log range by (\_ -> identity)
                }
            )
    , knowledge.debugToStringUses
        |> List.map
            (\debugToStringUse ->
                { path = debugToStringUse.modulePath
                , message = "Debug.toString is used"
                , details =
                    [ """Debug.toString is a quick and dirty way to display an elm value somewhere
and can for example be used to inspect private (opaque) types.
It's nothing a published product should make use of.
Using any `Debug` member also prevents compiling in optimized mode and publishing as a package."""
                    ]
                , range = debugToStringUse.referenceRange
                , fix = []
                }
            )
    , knowledge.debugTodoUses
        |> List.map
            (\debugTodoUse ->
                { path = debugTodoUse.modulePath
                , message = "Debug.todo is used"
                , details =
                    [ "Debug.todo marks missing functionality which needs to be added gradually."
                    ]
                , range = debugTodoUse.referenceRange
                , fix = []
                }
            )
    ]
        |> List.concat
