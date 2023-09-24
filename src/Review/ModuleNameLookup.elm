module Review.ModuleNameLookup exposing
    ( ModuleNameLookup, moduleNameFor, moduleNameAt
    , fullModuleNameFor, fullModuleNameAt
    , createForTests
    )

{-| Looks up the name of the module a function or type comes from based on the position of the element in the module's AST.

When encountering a `Expression.FunctionOrValue ModuleName String` (among other nodes where we refer to a function or value),
the module name available represents the module name that is in the source code. But that module name can be an alias to
a different import, or it can be empty, meaning that it refers to a local value or one that has been imported explicitly
or implicitly. Resolving which module the type or function comes from can be a bit tricky sometimes, and I recommend against
doing it yourself.

`elm-review` computes this for you already. Store this value inside your module context, then use
[`ModuleNameLookup.moduleNameFor`](./Review-ModuleNameLookup#moduleNameFor) or
[`ModuleNameLookup.moduleNameAt`](./Review-ModuleNameLookup#moduleNameAt) to get the name of the module the
type or value comes from.

@docs ModuleNameLookup, moduleNameFor, moduleNameAt
@docs fullModuleNameFor, fullModuleNameAt

Note: If you have been using [`elm-review-scope`](https://github.com/jfmengels/elm-review-scope) before, you should use this instead.


## Testing

@docs createForTests

-}

import Dict
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookup.Internal as Internal


{-| Associates positions in the AST of a module to the name of the module that the contained variable or type originates
from.
-}
type alias ModuleNameLookup =
    Internal.ModuleNameLookup


{-| Returns the name of the module the type, value, or operator referred to by this [`Node`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Node#Node) was defined in.

The function returns `Just []` if the type or value was defined in this module. It returns `Just moduleName` if the Node is among these kinds of AST nodes (and `Nothing` for all the others):

  - `Expression.FunctionOrValue`
  - `nodeForTheName` in `Expression.RecordUpdateExpression nodeForTheName modifiers`
  - `nodeForTheName` in `TypeAnnotation.Typed nodeForTheName args`
  - `Pattern.NamedPattern`
  - `Expression.PrefixOperator`
  - `Expression.OperatorApplication`

```elm
expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.FunctionOrValue _ "color" ->
            if ModuleNameLookup.moduleNameFor context.lookupTable node == Just [ "Css" ] then
                ( [ Rule.error
                        { message = "Do not use `Css.color` directly, use the Colors module instead"
                        , details = [ "We made a module which contains all the available colors of our design system. Use the functions in there instead." ]
                        }
                        (Node.range node)
                  ]
                , context
                )

            else
                ( [], context )

        _ ->
            ( [], context )
```

Note: If using a `Range` is easier in your situation than using a `Node`, use [`moduleNameAt`](#moduleNameAt) instead.

-}
moduleNameFor : ModuleNameLookup -> Node a -> Maybe ModuleName
moduleNameFor (Internal.ModuleNameLookup _ dict) (Node range _) =
    Dict.get (Internal.toRangeLike range) dict


{-| This is the same as [`moduleNameFor`](#moduleNameFor), except that the function will return the current module name
if the type or value was defined in this module, instead of `Just []`.
-}
fullModuleNameFor : ModuleNameLookup -> Node a -> Maybe ModuleName
fullModuleNameFor (Internal.ModuleNameLookup currentModuleName dict) (Node range _) =
    case Dict.get (Internal.toRangeLike range) dict of
        Just [] ->
            Just currentModuleName

        res ->
            res


{-| Returns the name of the module the type, value, or operator referred to by this [`Range`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Range#Range).

The function returns `Just []` if the type or value was defined in this module. It returns `Just moduleName` if the Node is among these kinds of AST nodes (and `Nothing` for all the others):

  - `Expression.FunctionOrValue`
  - `nodeForTheName` in `Expression.RecordUpdateExpression nodeForTheName modifiers`
  - `nodeForTheName` in `TypeAnnotation.Typed nodeForTheName args`
  - `Pattern.NamedPattern`
  - `Expression.PrefixOperator`
  - `Expression.OperatorApplication`

```elm
expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.RecordUpdateExpr (Node range name) _ ->
            case ModuleNameLookup.moduleNameAt context.lookupTable range of
                Just moduleName ->
                    ( [], markVariableAsUsed ( moduleName, name ) context )

                Nothing ->
                    ( [], context )

        _ ->
            ( [], context )
```

Note: If using a `Node` is easier in your situation than using a `Range`, use [`moduleNameFor`](#moduleNameFor) instead.

-}
moduleNameAt : ModuleNameLookup -> Range -> Maybe ModuleName
moduleNameAt (Internal.ModuleNameLookup _ dict) range =
    Dict.get (Internal.toRangeLike range) dict


{-| This is the same as [`moduleNameAt`](#moduleNameAt), except that the function will return the current module name
if the type or value was defined in this module, instead of `Just []`.
-}
fullModuleNameAt : ModuleNameLookup -> Range -> Maybe ModuleName
fullModuleNameAt (Internal.ModuleNameLookup currentModuleName dict) range =
    case Dict.get (Internal.toRangeLike range) dict of
        Just [] ->
            Just currentModuleName

        res ->
            res


{-| Creates a module name lookup table from a list of ranges and module names. The first argument is the name of the
module in which this lookup table would be used.

**NOTE**: This is only meant to be used for testing purposes, not for direct use in rules.

This can be useful if you want to test individual functions that take a `ModuleNameLookup` as an argument.

    ModuleNameLookup.createForTests [ "My", "Module" ] []

-}
createForTests : ModuleName -> List ( Range, ModuleName ) -> ModuleNameLookup
createForTests =
    Internal.fromList
