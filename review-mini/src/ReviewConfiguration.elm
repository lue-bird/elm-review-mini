module ReviewConfiguration exposing (configuration)

import Review
import ModuleAndExposesAreUsed
import ImportExposingIsExplicit
import LetValueOrFunctionIsTypeAnnotated
import ModuleExposingIsExplicit
import ModuleValueOrFunctionIsTypeAnnotated
import DebugIsNotUsed
import PatternVariableIsUsed
import CommentDoesNotUseCertainWords


configuration : { extraPaths : List String, reviews : List Review.Review }
configuration =
    { extraPaths =
        [ "README.md" ]
    , reviews =
        [ ImportExposingIsExplicit.review
        , ModuleExposingIsExplicit.review
        , ModuleAndExposesAreUsed.review
        , ModuleValueOrFunctionIsTypeAnnotated.review
        , LetValueOrFunctionIsTypeAnnotated.review
        , DebugIsNotUsed.review
        , PatternVariableIsUsed.review
        , CommentDoesNotUseCertainWords.review [ "TODO" ]
        ]
    }
