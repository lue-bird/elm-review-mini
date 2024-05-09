module ReviewConfiguration exposing (configuration)

import Review


configuration : { reviews : List Review.Review, extraPaths : List String }
configuration =
    { extraPaths =
        [ "README.md" ]
    , reviews =
        []
    }
