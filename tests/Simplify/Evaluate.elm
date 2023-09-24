module Simplify.Evaluate exposing (getBoolean, getInt, isAlwaysBoolean)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.ModuleNameLookup as ModuleNameLookup
import Simplify.AstHelpers as AstHelpers
import Simplify.Infer as Infer
import Simplify.Match exposing (Match(..))
import Simplify.Normalize as Normalize


getBoolean : Infer.Resources a -> Node Expression -> Match Bool
getBoolean resources baseNode =
    let
        node : Node Expression
        node =
            AstHelpers.removeParens baseNode
    in
    case node of
        Node functionRange (Expression.FunctionOrValue _ "True") ->
            case ModuleNameLookup.moduleNameAt resources.lookupTable functionRange of
                Just [ "Basics" ] ->
                    Determined True

                _ ->
                    Undetermined

        Node functionRange (Expression.FunctionOrValue _ "False") ->
            case ModuleNameLookup.moduleNameAt resources.lookupTable functionRange of
                Just [ "Basics" ] ->
                    Determined False

                _ ->
                    Undetermined

        Node functionRange (Expression.FunctionOrValue _ name) ->
            case
                ModuleNameLookup.moduleNameAt resources.lookupTable functionRange
                    |> Maybe.andThen (\moduleName -> Infer.get (Expression.FunctionOrValue moduleName name) (Tuple.first resources.inferredConstants))
            of
                Just (Expression.FunctionOrValue [ "Basics" ] "True") ->
                    Determined True

                Just (Expression.FunctionOrValue [ "Basics" ] "False") ->
                    Determined False

                Just _ ->
                    Undetermined

                Nothing ->
                    Undetermined

        _ ->
            case
                Infer.isBoolean
                    (Node.value (Normalize.normalize resources node))
                    (Tuple.first resources.inferredConstants)
            of
                Just bool ->
                    Determined bool

                Nothing ->
                    Undetermined


isAlwaysBoolean : Infer.Resources a -> Node Expression -> Match Bool
isAlwaysBoolean resources node =
    case Node.value (AstHelpers.removeParens node) of
        Expression.Application ((Node alwaysRange (Expression.FunctionOrValue _ "always")) :: boolean :: []) ->
            case ModuleNameLookup.moduleNameAt resources.lookupTable alwaysRange of
                Just [ "Basics" ] ->
                    getBoolean resources boolean

                _ ->
                    Undetermined

        Expression.LambdaExpression { expression } ->
            getBoolean resources expression

        _ ->
            Undetermined


getInt : Infer.Resources a -> Node Expression -> Maybe Int
getInt resources baseNode =
    let
        node : Node Expression
        node =
            AstHelpers.removeParens baseNode
    in
    case node of
        Node _ (Expression.Integer n) ->
            Just n

        Node _ (Expression.Hex n) ->
            Just n

        Node _ (Expression.Negation expr) ->
            Maybe.map negate (getInt resources expr)

        Node functionRange (Expression.FunctionOrValue _ name) ->
            case
                ModuleNameLookup.moduleNameAt resources.lookupTable functionRange
                    |> Maybe.andThen (\moduleName -> Infer.get (Expression.FunctionOrValue moduleName name) (Tuple.first resources.inferredConstants))
            of
                Just (Expression.Integer int) ->
                    Just int

                _ ->
                    Nothing

        _ ->
            Nothing
