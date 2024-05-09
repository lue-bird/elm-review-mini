module ReviewConfiguration exposing (configuration)

import Review
import UnderscoreInModuleNameForbid


configuration : { reviews : List Review.Review, extraPaths : List String }
configuration =
    { extraPaths =
        [ "README.md" ]
    , reviews =
        [ UnderscoreInModuleNameForbid.review
        ]
    }
