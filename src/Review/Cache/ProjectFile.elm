module Review.Cache.ProjectFile exposing
    ( Entry, create
    , match
    , outputContext, outputContextHash
    )

{-| Cache for the result of the analysis of a project file (elm.json, README.md, docs.json for dependencies).

@docs Entry, create
@docs match
@docs outputContext, outputContextHash

-}

import Review.Cache.ContentHash as ContentHash exposing (ContentHash)
import Review.Cache.ContextHash as ContextHash exposing (ComparableContextHash, ContextHash)


type Entry context
    = Entry
        { contentHash : Maybe ContentHash
        , inputContextHash : ComparableContextHash context
        , isFileIgnored : Bool
        , outputContext : context
        , outputContextHash : ContextHash context
        }


create :
    { contentHash : Maybe ContentHash
    , inputContextHash : ComparableContextHash context
    , isFileIgnored : Bool
    , outputContext : context
    }
    -> Entry context
create entry =
    Entry
        { contentHash = entry.contentHash
        , inputContextHash = entry.inputContextHash
        , isFileIgnored = entry.isFileIgnored
        , outputContext = entry.outputContext
        , outputContextHash = ContextHash.create entry.outputContext
        }


match : Maybe ContentHash -> ComparableContextHash context -> Entry context -> { isFileIgnored : Bool } -> Bool
match contentHash contexts (Entry entry) isFileIgnored =
    ContentHash.areEqualForMaybe contentHash entry.contentHash
        && (contexts == entry.inputContextHash)
        && (isFileIgnored.isFileIgnored == entry.isFileIgnored)


outputContext : Entry context -> context
outputContext (Entry entry) =
    entry.outputContext


outputContextHash : Entry context -> ContextHash context
outputContextHash (Entry entry) =
    entry.outputContextHash
