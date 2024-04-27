module Review.Cache.EndAnalysis exposing
    ( Entry, create
    , match
    )

{-| Cache for the result of the final project evaluation analysis and data extract.

@docs Entry, create
@docs match

-}

import Review.Cache.ContextHash exposing (ComparableContextHash)


type Entry context
    = Entry
        { inputContextHashes : ComparableContextHash context
        }


create : ComparableContextHash context -> Entry context
create inputContextHashes =
    Entry
        { inputContextHashes = inputContextHashes
        }


match : ComparableContextHash context -> Entry context -> Bool
match context (Entry entry) =
    context == entry.inputContextHashes
