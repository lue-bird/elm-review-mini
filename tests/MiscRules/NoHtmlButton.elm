module MiscRules.NoHtmlButton exposing (rule)

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Review.ModuleNameLookup as ModuleNameLookup exposing (ModuleNameLookup)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoHtmlButton" contextCreator
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema
        |> Rule.ignoreErrorsForFiles [ "src/Button.elm" ]


type alias Context =
    { moduleNameLookup : ModuleNameLookup
    }


contextCreator : Rule.ContextCreator Context
contextCreator =
    Rule.initContextCreator
        (\moduleNameLookup ->
            { moduleNameLookup = moduleNameLookup
            }
        )
        |> Rule.withModuleNameLookupTable


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        FunctionOrValue _ "button" ->
            if ModuleNameLookup.moduleNameFor context.moduleNameLookup node == Just [ "Html" ] then
                ( [ Rule.error
                        { message = "Do not use `Html.button` directly"
                        , details = [ "At fruits.com, we've built a nice `Button` module that suits our needs better. Using this module instead of `Html.button` ensures we have a consistent button experience across the website." ]
                        }
                        (Node.range node)
                  ]
                , context
                )

            else
                ( [], context )

        _ ->
            ( [], context )
