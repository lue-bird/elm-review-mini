module ReviewConfiguration exposing (configuration)

import BindingIsUsed
import CommentDoesNotUseCertainMarks
import DebugIsNotUsed
import ImportExposingIsExplicit
import LetValueOrFunctionIsTypeAnnotated
import ModuleAndExposesAreUsed
import ModuleExposingIsExplicit
import ModuleValueOrFunctionIsTypeAnnotated
import RecordTypeAliasConstructorFunctionIsNotUsed
import Review


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
        , BindingIsUsed.review
        , CommentDoesNotUseCertainMarks.review [ "TODO" ]
        , RecordTypeAliasConstructorFunctionIsNotUsed.review
        ]
    }
