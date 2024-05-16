module Review.Test.FailureMessage exposing (dependencyDocsJsonParsingFailure, dependencyElmJsonParsingFailure, didNotExpectErrors, elmJsonFixedSourceParsingFailure, elmJsonParsingFailure, emptyDetails, expectedErrorsButFoundNone, fixedCodeMismatch, fixedCodeWhitespaceMismatch, hasCollisionsInFixRanges, locationIsAmbiguousInSourceCode, messageMismatch, missingFixes, moduleFixedSourceParsingFailure, moduleParsingFailure, tooFewErrors, tooManyErrors, unchangedSourceAfterFix, underMismatch, unexpectedDetails, unexpectedFixes, unknownFilesInExpectedErrors, wrongLocation)

{-| Failure messages for the `Review.Test` module.
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


listErrorMessagesAndPositions : List { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String
listErrorMessagesAndPositions errors =
    errors
        |> List.map errorMessageAndPosition
        |> String.join "\n"


wrapInQuotes : String -> String
wrapInQuotes string =
    [ "`", string, "`" ] |> String.concat


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


errorMessageAndPosition : { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String
errorMessageAndPosition error =
    [ "  - ", wrapInQuotes error.message, "\n    at ", rangeToCodeString error.range ] |> String.concat


didNotExpectErrors : String -> List { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String
didNotExpectErrors path errors =
    failureMessage "DID NOT EXPECT ERRORS"
        ([ """I expected no errors for the file at path """
         , path
         , """ but found:

"""
         , listErrorMessagesAndPositions errors
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
    failureMessage "ELM.JSON PARSING ERROR" ([ details, "\n\n", hint ] |> String.concat)


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
    failureMessage "EXPECTED FIXED SOURCE ELM.JSON PARSING ERROR"
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
    failureMessage "MODULE SOURCE CODE PARSING ERROR" ([ details, "\n\n", hint ] |> String.concat)


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
    failureMessage "DEPENDENCY ELM.JSON SOURCE PARSING ERROR" ([ details, "\n\n", hint ] |> String.concat)


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
    failureMessage "DEPENDENCY DOCS.JSON SOURCE PARSING ERROR" ([ details, "\n\n", hint ] |> String.concat)


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
    failureMessage "EXPECTED FIXED MODULE SOURCE CODE PARSING ERROR" ([ details, "\n\n", hint ] |> String.concat)


messageMismatch : { message : String, details : List String, under : String } -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String
messageMismatch expectedError error =
    failureMessage "UNEXPECTED ERROR MESSAGE"
        ([ """I was looking for the error with the message

  """
         , wrapInQuotes expectedError.message
         , """

but I found the following error message:

  """
         , wrapInQuotes error.message
         ]
            |> String.concat
        )


formatSourceCodeWithFormatter : (List String -> List String) -> List String -> String
formatSourceCodeWithFormatter formatter lines =
    if List.length lines == 1 then
        formatter lines
            |> String.join "\n"
            |> wrapInQuotes

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
            |> wrapInTripleQuotes


wrapInTripleQuotes : String -> String
wrapInTripleQuotes str =
    [ "```\n", str, "\n  ```" ] |> String.concat


formatSourceCode : String -> String
formatSourceCode string =
    formatSourceCodeWithFormatter identity (String.lines string)


underMismatch : { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> { under : String, codeAtLocation : String } -> String
underMismatch error range =
    failureMessage "UNEXPECTED ERROR LOCATION"
        ([ """I found an error with the following message:

  """
         , wrapInQuotes error.message
         , """

and I was expecting it to be under:

  """
         , formatSourceCode range.under
         , """

but I found it under:

  """
         , formatSourceCode range.codeAtLocation
         , """

Hint: Maybe you're passing the `Range` of a wrong node to the review error."""
         ]
            |> String.concat
        )


unexpectedDetails : List String -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String
unexpectedDetails expectedDetails error =
    failureMessage "UNEXPECTED ERROR DETAILS"
        ([ """I found an error for a file with the following message:

  """
         , wrapInQuotes error.message
         , """

and I was expecting its details to be:

  """
         , formatDetails expectedDetails
         , """

but I found these details:

  """
         , formatDetails error.details
         ]
            |> String.concat
        )


formatDetails : List String -> String
formatDetails details =
    case details of
        [ detail ] ->
            wrapInQuotes detail

        details_ ->
            details_
                |> List.map (\str -> "  " ++ str)
                |> String.join "\n\n"
                |> (\str -> [ "```\n", str, "\n  ```" ] |> String.concat)


emptyDetails : String -> String
emptyDetails errorMessage =
    failureMessage "EMPTY ERROR DETAILS"
        ([ """I found an error with the following message:

  """
         , wrapInQuotes errorMessage
         , """

but its details were empty. I require having details as I believe they will
help the user who encounters the problem.

The details could:
- explain what the problem is
- explain the reasoning behind the problem
- give suggestions on how to solve the problem or alternatives"""
         ]
            |> String.concat
        )


wrongLocation : { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> Elm.Syntax.Range.Range -> String -> String
wrongLocation error range under =
    failureMessage "UNEXPECTED ERROR LOCATION"
        ([ """I was looking for the error with the following message:

  """
         , wrapInQuotes error.message
         , """

under the following code:

  """
         , formatSourceCode under
         , """

and I found it, but the exact location you specified is not the one I found.

I was expecting the error at:

  """
         , rangeToCodeString range
         , """

but I found it at:

  """
         , rangeToCodeString error.range
         ]
            |> String.concat
        )


tooFewErrors : String -> Int -> List { message : String, details : List String, under : String } -> String
tooFewErrors moduleName expectedNumberOfErrors missingExpectedErrors =
    let
        numberOfErrors : Int
        numberOfErrors =
            List.length missingExpectedErrors
    in
    failureMessage "RULE REPORTED LESS ERRORS THAN EXPECTED"
        ([ "I expected to see "
         , String.fromInt expectedNumberOfErrors
         , " errors for module `"
         , moduleName
         , "` but only found "
         , String.fromInt (expectedNumberOfErrors - numberOfErrors)
         , """.
Here are the """
         , String.fromInt numberOfErrors
         , """ I could not find:

"""
         ]
            |> String.concat
        )
        ++ (missingExpectedErrors
                |> List.map expectedErrorToString
                |> String.join "\n"
           )


expectedErrorToString : { a_ | message : String } -> String
expectedErrorToString expectedError =
    "  - " ++ wrapInQuotes expectedError.message


tooManyErrors : String -> List { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String
tooManyErrors moduleName extraErrors =
    let
        numberOfErrors : Int
        numberOfErrors =
            List.length extraErrors
    in
    failureMessage "RULE REPORTED MORE ERRORS THAN EXPECTED"
        ([ "I found "
         , String.fromInt numberOfErrors
         , " "
         , pluralizeErrors numberOfErrors
         , " too many for module `"
         , moduleName
         , "`:\n\n"
         , listErrorMessagesAndPositions extraErrors
         ]
            |> String.concat
        )


pluralizeErrors : Int -> String
pluralizeErrors n =
    if n == 1 then
        "error"

    else
        "errors"


locationIsAmbiguousInSourceCode : String -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String -> List Int -> String
locationIsAmbiguousInSourceCode sourceCode error under occurrencesInSourceCode =
    failureMessage "AMBIGUOUS ERROR LOCATION"
        ([ """The exact location where the error appears is ambiguous.

You are looking for the error message:

  """
         , error.message
         , """

and expecting to see it under:

  """
         , under |> formatSourceCode
         , """

I found """
         , occurrencesInSourceCode |> List.length |> String.fromInt
         , """ locations where that code appeared. Please use
Review.Test.UnderExactly to make the range you were targeting unambiguous.

Tip: I found them starting at:
"""
         , listOccurrencesAsLocations sourceCode under occurrencesInSourceCode
         ]
            |> String.concat
        )


listOccurrencesAsLocations : String -> String -> List Int -> String
listOccurrencesAsLocations sourceCode under occurrences =
    occurrences
        |> List.map
            (\occurrence ->
                "  - "
                    ++ (occurrence
                            |> positionAsRange sourceCode under
                            |> .start
                            |> locationToCodeString
                       )
            )
        |> String.join "\n"


positionAsRange : String -> String -> Int -> Elm.Syntax.Range.Range
positionAsRange sourceCode under position =
    let
        linesBeforeAndIncludingPosition : List String
        linesBeforeAndIncludingPosition =
            sourceCode
                |> String.slice 0 position
                |> String.lines

        startRow : Int
        startRow =
            List.length linesBeforeAndIncludingPosition

        startColumn : Int
        startColumn =
            linesBeforeAndIncludingPosition
                |> ListExtra.last
                |> Maybe.withDefault ""
                |> String.length
                |> (+) 1

        linesInUnder : List String
        linesInUnder =
            String.lines under

        endRow : Int
        endRow =
            startRow + List.length linesInUnder - 1

        endColumn : Int
        endColumn =
            if startRow == endRow then
                startColumn + String.length under

            else
                linesInUnder
                    |> ListExtra.last
                    |> Maybe.withDefault ""
                    |> String.length
                    |> (+) 1
    in
    { start =
        { row = startRow
        , column = startColumn
        }
    , end =
        { row = endRow
        , column = endColumn
        }
    }


expectedErrorsButFoundNone : String
expectedErrorsButFoundNone =
    failureMessage "EXPECTED ERRORS BUT FOUND NONE"
        """I expected errors for this project since you gave me a list of them in `Review.Test.expectErrors`
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
        ([ """I expected that the error with the following message

  """
         , wrapInQuotes expectedError.message
         , """

would provide some fixes, but I didn't find any.

Hint: Maybe you forgot to call a function like `Rule.errorWithFix` or maybe
the list of provided fixes was empty."""
         ]
            |> String.concat
        )


unexpectedFixes : { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String
unexpectedFixes error =
    failureMessage "UNEXPECTED FIXES"
        ([ """I expected that the error with the following message

  """
         , wrapInQuotes error.message
         , """

would not have any fixes, but it provided some.

Because the error provides fixes, I require providing the expected result
of the automatic fix. Otherwise, there is no way to know whether the fix
will result in a correct and in the intended result.

To fix this, you can call `Review.Test.whenFixed` on your error:

  Review.Test.error
      { message = "<message>"
      , details = "<details>"
      , under = "<under>"
      }
      |> Review.Test.whenFixed "<source code>\""""
         ]
            |> String.concat
        )


fixedCodeMismatch : String -> String -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String
fixedCodeMismatch resultingSourceCode expectedSourceCode error =
    failureMessage "FIXED CODE MISMATCH"
        ([ """I found a different fixed source code than expected for the error with the
following message:

  """
         , wrapInQuotes error.message
         , """

I expected the following result after the fixes have been applied:

  """
         , formatSourceCode expectedSourceCode
         , """

but I found:

  """
         , formatSourceCode resultingSourceCode
         ]
            |> String.concat
        )


fixedCodeWhitespaceMismatch : String -> String -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String
fixedCodeWhitespaceMismatch resultingSourceCode expectedSourceCode error =
    let
        ( expected, resulting ) =
            highlightDifferencesInSourceCodes resultingSourceCode expectedSourceCode
    in
    failureMessage "FIXED CODE MISMATCH (WHITESPACE ISSUE)"
        ([ """I found a different fixed source code than expected for the error with the
following message:

  """
         , wrapInQuotes error.message
         , """

The problem is related to """
         , "WHITESPACE!" |> Ansi.bold |> Ansi.yellow
         , """
I expected the following result after the fixes have been applied:

  """
         , expected
         , """

but I found:

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
with the following message:

  """
         , wrapInQuotes error.message
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
with the following message:

  """
         , wrapInQuotes error.message
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
