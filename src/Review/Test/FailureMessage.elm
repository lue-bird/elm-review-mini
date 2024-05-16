module Review.Test.FailureMessage exposing (dependencyDocsJsonParsingFailure, dependencyElmJsonParsingFailure, didNotExpectErrors, elmJsonFixedSourceParsingFailure, elmJsonParsingFailure, emptyDetails, expectedErrorsButFoundNone, fixedSourceMismatch, fixedSourceWhitespaceMismatch, hasCollisionsInFixRanges, locationIsAmbiguousInSourceCode, messageMismatch, missingFixes, moduleFixedSourceParsingFailure, moduleParsingFailure, tooFewErrors, tooManyErrors, unchangedSourceAfterFix, underMismatch, unexpectedDetails, unexpectedFixes, unknownFilesInExpectedErrors, wrongLocation)

{-| Failure messages for [`Review.Test`](Review-Test)
-}

import Ansi
import Diff
import Elm.Syntax.Range
import Json.Decode
import ListExtra
import Review


failureMessage : String -> String -> String
failureMessage title content =
    [ title |> Ansi.bold |> Ansi.red, "\n\n", content ] |> String.concat


locationToCodeString : Elm.Syntax.Range.Location -> String
locationToCodeString =
    \location ->
        [ "{ row = "
        , String.fromInt location.row
        , ", column = "
        , String.fromInt location.column
        , " }"
        ]
            |> String.concat


rangeToCodeString : Elm.Syntax.Range.Range -> String
rangeToCodeString =
    \range ->
        [ "{ start = "
        , range.start |> locationToCodeString
        , ", end = "
        , range.end |> locationToCodeString
        , " }"
        ]
            |> String.concat


didNotExpectErrors : String -> List { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String
didNotExpectErrors path errors =
    failureMessage "DID NOT EXPECT ERRORS"
        ([ """I expected no errors for the file at path """
         , path
         , """ but found:

"""
         , errors
            |> List.map
                (\error ->
                    [ "  - ", error.message, "\n    at ", rangeToCodeString error.range ] |> String.concat
                )
            |> String.join "\n"
         ]
            |> String.concat
        )


elmJsonParsingFailure : Json.Decode.Error -> String
elmJsonParsingFailure decodeError =
    let
        hint : String
        hint =
            """Hint: Try to start with a working elm.json from one of your projects and add dependencies from there"""

        details : String
        details =
            [ """I could not decode the provided elm.json because """
            , decodeError |> Json.Decode.errorToString
            ]
                |> String.concat
    in
    failureMessage "ELM.JSON FAILED TO PARSE" ([ details, "\n\n", hint ] |> String.concat)


elmJsonFixedSourceParsingFailure : Json.Decode.Error -> { message : String, details : List String } -> String
elmJsonFixedSourceParsingFailure decodeError errorInfo =
    let
        details : String
        details =
            [ """I could not decode the expected fixed elm.json for the error with the message \""""
            , errorInfo.message
            , """" because """
            , decodeError |> Json.Decode.errorToString
            ]
                |> String.concat
    in
    failureMessage "EXPECTED FIXED ELM.JSON FAILED TO PARSE"
        details


moduleParsingFailure : { path : String } -> String
moduleParsingFailure file =
    let
        hint : String
        hint =
            """Hint: Maybe you forgot to add the module definition at the top, like module A exposing (a)"""

        details : String
        details =
            [ """I could not parse the provided elm module source code at path """
            , file.path
            ]
                |> String.concat
    in
    failureMessage "MODULE FAILED TO PARSE" ([ details, "\n\n", hint ] |> String.concat)


dependencyElmJsonParsingFailure : Json.Decode.Error -> String
dependencyElmJsonParsingFailure jsonDecodeError =
    let
        hint : String
        hint =
            """Hint: Maybe you copy pasted the wrong file?"""

        details : String
        details =
            [ """I could not parse a provided dependency elm.json source: """
            , jsonDecodeError |> Json.Decode.errorToString
            ]
                |> String.concat
    in
    failureMessage "DEPENDENCY ELM.JSON FAILED TO PARSE" ([ details, "\n\n", hint ] |> String.concat)


dependencyDocsJsonParsingFailure : Json.Decode.Error -> String
dependencyDocsJsonParsingFailure jsonDecodeError =
    let
        hint : String
        hint =
            """Hint: Maybe you copy pasted the wrong file?"""

        details : String
        details =
            [ """I could not parse a provided dependency docs.json source: """
            , jsonDecodeError |> Json.Decode.errorToString
            ]
                |> String.concat
    in
    failureMessage "DEPENDENCY DOCS.JSON FAILED TO PARSE" ([ details, "\n\n", hint ] |> String.concat)


moduleFixedSourceParsingFailure : { path : String, errorInfo : { message : String, details : List String } } -> String
moduleFixedSourceParsingFailure info =
    let
        hint : String
        hint =
            """Hint: Maybe you forgot to add the module definition at the top, like module A exposing (a)"""

        details : String
        details =
            [ "I could not parse the provided expected fixed elm module source code at path "
            , info.path
            , " for the error with the message \""
            , info.errorInfo.message
            , "\""
            ]
                |> String.concat
    in
    failureMessage "EXPECTED FIXED MODULE SOURCE CODE PARSING ERROR"
        ([ details, "\n\n", hint ] |> String.concat)


messageMismatch : { message : String, details : List String, under : String } -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String
messageMismatch expectedError error =
    failureMessage "ERROR MESSAGE DOESN'T MATCH"
        ([ """I was looking for the error with the message

  """
         , expectedError.message
         , """

but I found the error message

  """
         , error.message
         ]
            |> String.concat
        )


formatSourceCodeWithFormatter : (List String -> List String) -> List String -> String
formatSourceCodeWithFormatter formatter lines =
    if List.length lines == 1 then
        formatter lines
            |> String.join "\n"

    else
        formatter lines
            |> List.map
                (\str ->
                    case str of
                        "" ->
                            ""

                        nonBlankLine ->
                            "    " ++ nonBlankLine
                )
            |> String.join "\n"


formatSourceCode : String -> String
formatSourceCode string =
    formatSourceCodeWithFormatter identity (String.lines string)


underMismatch : { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> { under : String, codeAtLocation : String } -> String
underMismatch error range =
    failureMessage "ERROR LOCATION DOESN'T MATCH"
        ([ """I found an error with the message

  """
         , error.message
         , """

and I was expecting it to be under

  """
         , formatSourceCode range.under
         , """

but I found it under

  """
         , formatSourceCode range.codeAtLocation
         , """

Hint: Maybe you're passing the range of a wrong node to the review error?"""
         ]
            |> String.concat
        )


unexpectedDetails : List String -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String
unexpectedDetails expectedDetails error =
    failureMessage "ERROR DETAILS DON'T MATCH"
        ([ """I found an error for a file with the message

  """
         , error.message
         , """

and I was expecting its details to be

  """
         , formatDetails expectedDetails
         , """

but I found the details

  """
         , formatDetails error.details
         ]
            |> String.concat
        )


formatDetails : List String -> String
formatDetails details =
    case details of
        [ detail ] ->
            detail

        details_ ->
            [ "\n"
            , details_
                |> List.map (\str -> "  " ++ str)
                |> String.join "\n\n"
            , "\n  "
            ]
                |> String.concat


emptyDetails : String -> String
emptyDetails errorMessage =
    failureMessage "ERROR DETAILS ARE EMPTY"
        ([ """I found an error with the message

  """
         , errorMessage
         , """

but its details were empty. Having details will
help the user who encounters the problem by for example
  - explaining what the problem is
  - explaining the reasoning behind the problem
  - giving suggestions on how to solve the problem or alternatives"""
         ]
            |> String.concat
        )


wrongLocation : { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> Elm.Syntax.Range.Range -> String -> String
wrongLocation error range under =
    failureMessage "ERROR LOCATION DOESN'T MATCH"
        ([ """I was looking for the error with the message

  """
         , error.message
         , """

under the code

  """
         , under |> formatSourceCode
         , """

and I found it, but the exact location you specified is not the one I found.

I was expecting the error at

  """
         , range.start |> locationToCodeString
         , """

but I found it at

  """
         , error.range.start |> locationToCodeString
         ]
            |> String.concat
        )


tooFewErrors : String -> Int -> List { message : String, details : List String, under : String } -> String
tooFewErrors moduleName expectedNumberOfErrors missingExpectedErrors =
    let
        errorCount : Int
        errorCount =
            missingExpectedErrors |> List.length
    in
    failureMessage "TOO FEW ERRORS"
        ([ "I expected to see "
         , expectedNumberOfErrors |> String.fromInt
         , " errors for module "
         , moduleName
         , " but only found "
         , (expectedNumberOfErrors - errorCount) |> String.fromInt
         , """.
Here are the """
         , errorCount |> String.fromInt
         , """ I could not find:

"""
         ]
            |> String.concat
        )
        ++ (missingExpectedErrors
                |> List.map (\expectedError -> "  - " ++ expectedError.message)
                |> String.join "\n"
           )


tooManyErrors : String -> List { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String
tooManyErrors moduleName extraErrors =
    let
        errorCount : Int
        errorCount =
            extraErrors |> List.length
    in
    failureMessage "TOO MANY ERRORS"
        ([ "I found "
         , errorCount |> String.fromInt
         , " "
         , pluralizeErrors errorCount
         , " too many for module "
         , moduleName
         , ":\n\n"
         , extraErrors
            |> List.map
                (\error ->
                    [ "  - ", error.message, "\n    at ", rangeToCodeString error.range ] |> String.concat
                )
            |> String.join "\n"
         ]
            |> String.concat
        )


pluralizeErrors : Int -> String
pluralizeErrors n =
    case n of
        1 ->
            "error"

        _ ->
            "errors"


locationIsAmbiguousInSourceCode : String -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String -> List Int -> String
locationIsAmbiguousInSourceCode sourceCode error under occurrencesInSourceCode =
    failureMessage "EXPECTED ERROR LOCATION IS AMBIGUOUS"
        ([ """The exact location where the error appears is ambiguous.

You are looking for the error message

  """
         , error.message
         , """

and expecting to see it under

  """
         , under |> formatSourceCode
         , """

I found """
         , occurrencesInSourceCode |> List.length |> String.fromInt
         , """ locations where that code appeared. Switch from Review.Test.Under to
Review.Test.UnderExactly to make the range you were targeting unambiguous.

Tip: I found them starting at:
"""
         , occurrencesInSourceCode
            |> List.map
                (\occurrence ->
                    "  - "
                        ++ (occurrence
                                |> startingLocationInSource sourceCode
                                |> locationToCodeString
                           )
                )
            |> String.join "\n"
         ]
            |> String.concat
        )


startingLocationInSource : String -> Int -> Elm.Syntax.Range.Location
startingLocationInSource sourceCode position =
    let
        linesBeforeAndIncludingPosition : List String
        linesBeforeAndIncludingPosition =
            sourceCode
                |> String.slice 0 position
                |> String.lines
    in
    { row = linesBeforeAndIncludingPosition |> List.length
    , column =
        (linesBeforeAndIncludingPosition
            |> ListExtra.last
            |> Maybe.withDefault ""
            |> String.length
        )
            + 1
    }


expectedErrorsButFoundNone : String
expectedErrorsButFoundNone =
    failureMessage "EXPECTED ERRORS BUT FOUND NONE"
        """I expected errors for this project since you gave me a list of them in expectedErrors
but I couldn't find a file in the given test source for which I could report anything."""


unknownFilesInExpectedErrors : String -> String
unknownFilesInExpectedErrors moduleName =
    failureMessage "UNKNOWN FILE PATHS IN EXPECTED ERRORS"
        ([ """I expected errors for the file at path """
         , moduleName
         , """ but I couldn't find a module/extra file/elm.json in the test source at that path.

Maybe there was a mistake during the writing of the test? Please
match the path of the modules in the sources to test to the ones in the
expected errors list."""
         ]
            |> String.concat
        )


missingFixes : { message : String, details : List String, under : String } -> String
missingFixes expectedError =
    failureMessage "MISSING FIXES"
        ([ """I expected that the error with the message

  """
         , expectedError.message
         , """

would provide some fixes, but I didn't find any.

Hint: Maybe the list of provided fixes was empty?"""
         ]
            |> String.concat
        )


unexpectedFixes : { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String
unexpectedFixes error =
    failureMessage "UNEXPECTED FIXES"
        ([ """I expected that the error with the message

  """
         , error.message
         , """

would not have any fixes, but it provided some.

Because the error provides fixes, I require providing the expected result
of the automatic fix. Otherwise, there is no way to know whether the fix
will result in a correct and in the intended result.

To fix this, you can replace the fixedSource field value Nothing by Just with the source after fix."""
         ]
            |> String.concat
        )


fixedSourceMismatch : String -> String -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String
fixedSourceMismatch resultingSource expectedSource error =
    failureMessage "FIXED SOURCE MISMATCH"
        ([ """I found a different fixed source code than expected for the error with the message:

  """
         , error.message
         , """

after the fixes have been applied, I expected the source to be

  """
         , formatSourceCode expectedSource
         , """

but I found the source

  """
         , formatSourceCode resultingSource
         ]
            |> String.concat
        )


fixedSourceWhitespaceMismatch : String -> String -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String
fixedSourceWhitespaceMismatch resultingSource expectedSource error =
    let
        ( expected, resulting ) =
            highlightDifferencesInSourceCodes resultingSource expectedSource
    in
    failureMessage "FIXED SOURCE MISMATCH (WHITESPACE ISSUE)"
        ([ """I found a different fixed source than expected for the error with the message

  """
         , error.message
         , """

The problem is related to """
         , "WHITESPACE!" |> Ansi.bold |> Ansi.yellow
         , """
after the fixes have been applied, I expected the source to be

  """
         , expected
         , """

but I found the source

  """
         , resulting
         ]
            |> String.concat
        )


highlightDifferencesInSourceCodes : String -> String -> ( String, String )
highlightDifferencesInSourceCodes a b =
    let
        ( resA, resB ) =
            highlightWhiteSpaceDifferences a b
    in
    ( formatSourceCodeWithFormatter replaceWhitespace (String.lines resA), formatSourceCodeWithFormatter replaceWhitespace (String.lines resB) )


highlightWhiteSpaceDifferences : String -> String -> ( String, String )
highlightWhiteSpaceDifferences aString bString =
    Diff.diff (String.toList aString) (String.toList bString)
        |> List.foldl
            (\change ( a, b ) ->
                case change of
                    Diff.NoChange str ->
                        ( a ++ String.fromChar str, b ++ String.fromChar str )

                    Diff.Added '\n' ->
                        ( [ a, Ansi.backgroundRed "↵", "\n" ] |> String.concat, b )

                    Diff.Added str ->
                        ( a ++ Ansi.backgroundRed (String.fromChar str), b )

                    Diff.Removed '\n' ->
                        ( a, [ b, Ansi.backgroundRed "↵", "\n" ] |> String.concat )

                    Diff.Removed str ->
                        ( a, b ++ Ansi.backgroundRed (String.fromChar str) )
            )
            ( "", "" )


replaceWhitespace : List String -> List String
replaceWhitespace lines =
    lines
        |> List.map (String.replace " " (Ansi.cyan "·"))
        |> String.join (Ansi.cyan "\n")
        |> String.split "\n"


unchangedSourceAfterFix : { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String
unchangedSourceAfterFix error =
    failureMessage "UNCHANGED SOURCE AFTER FIX"
        ([ """I got something unexpected when applying the fixes provided by the error
with the message

  """
         , error.message
         , """

I expected the fix to make some changes to the source code, but it resulted
in the same source code as before the fixes.

This is problematic because I will tell the user that this rule provides an
automatic fix, but I will have to disappoint them when I later find out it
doesn't do anything.

Hint: Maybe you inserted an empty string into the source code."""
         ]
            |> String.concat
        )


hasCollisionsInFixRanges : { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String
hasCollisionsInFixRanges error =
    failureMessage "FOUND COLLISIONS IN FIX RANGES"
        ([ """I got something unexpected when applying the fixes provided by the error
with the message

  """
         , error.message
         , """

I found that some fixes were targeting (partially or completely) the same
section of code. The problem with that is that I can't determine which fix
to apply first, and the result will be different and potentially invalid
based on the order in which I apply these fixes.

For this reason, I require that the ranges (for replacing and removing) and
the positions (for inserting) of every fix to be mutually exclusive.

Hint: Maybe you duplicated a fix, or you targeted the wrong node for one
of your fixes."""
         ]
            |> String.concat
        )
