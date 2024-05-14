module ReviewConfiguration exposing (configuration)

import Review
import ExposesAreUsed
import ImportExposingIsExplicit
import LetValueOrFunctionIsTypeAnnotated
import ModuleExposingIsExplicit
import ModuleValueOrFunctionIsTypeAnnotated


configuration : { reviews : List Review.Review, extraPaths : List String }
configuration =
    { extraPaths =
        [ "README.md" ]
    , reviews =
        [ ImportExposingIsExplicit.review
        , ModuleExposingIsExplicit.review
        , ExposesAreUsed.review
        , ModuleValueOrFunctionIsTypeAnnotated.review
        , LetValueOrFunctionIsTypeAnnotated.review
        ]
    }
