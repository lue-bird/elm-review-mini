> Status: package and common reviews unpublished, a few helpers still missing

Scan your [elm](https://elm-lang.org/) project to enforce conventions using reviews written in elm and [published as packages](https://dark.elm.dmy.fr/?q=elm-review-mini-).
It's heavily inspired by the phenomenal [`jfmengels/elm-review`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review/latest/) but comes with a much simpler API and much lighter internals (see also [the feeling section](#feelings)).

Use it in your project by adding this starter config with the CLI set up
```bash
curl -L https://github.com/lue-bird/elm-review-mini-cli-starter/tarball/master review-mini | tar xz
```
The created `review-mini/` is a regular elm application where you can add new reviews with `elm install`, then add them in your `review-mini/src/ReviewConfiguration.elm`:

```elm
module ReviewConfiguration exposing (configuration)
```
```elm
import SomeConvention
import Review

configuration : { extraPaths : List String, reviews : List Review.Review }
configuration =
    { extraPaths = [ "README.md" ]
    , reviews =
        [ SomeConvention.review SomeConvention.configurationOption
        -- , ...
        ]
    }
```
see also ["when to enable a review"](#when-to-create-or-enable-a-review).

An example of [creating a custom review](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-mini/1.0.0/Review#create) to fix a typo in a string that was made too often:
```elm
module StringSpellsCompanyNameCorrectly exposing (review)
```
```elm
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Node
import Elm.Syntax.Range
import Review

review : Review.Review
review =
    Review.create
        { inspect =
            [ Review.inspectModule moduleDataToKnowledge
            ]
        , knowledgeMerge = knowledgeMerge
        , report = report
        }

type alias Knowledge =
    { typosInStrings :
        List { modulePath : String, range : Elm.Syntax.Range.Range }
    }

moduleDataToKnowledge :
    { moduleData_ | path : String, syntax : Elm.Syntax.File.File }
    -> Knowledge
moduleDataToKnowledge =
    \moduleData ->
        { typosInStrings =
            moduleData.syntax.declarations
                |> List.concatMap declarationToTyposInStrings
                |> List.map
                    (\typoRange ->
                        { range = typoRange
                        , modulePath = moduleData.path
                        }
                    )
        }

declarationToTyposInStrings :
    Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration
    -> List Elm.Syntax.Range.Range
declarationToTyposInStrings =
    \(Elm.Syntax.Node.Node _ declaration) ->
        case declaration of
            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                functionDeclaration.declaration
                    |> Elm.Syntax.Node.value
                    |> .expression
                    |> expressionToTyposInStrings

            _ ->
                []

expressionToTyposInStrings :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> List Elm.Syntax.Range.Range
expressionToTyposInStrings =
    \expressionNode ->
        case expressionNode of
            Elm.Syntax.Node.Node range (Elm.Syntax.Expression.Literal string) ->
                string
                    |> Review.sourceRangesOf "frits.com"
                    |> List.map (rangeRelativeTo range.start)

            nonStringExpressionNode ->
                nonStringExpressionNode
                    |> Review.expressionSubs
                    |> List.concatMap expressionToTyposInStrings

knowledgeMerge : Knowledge -> Knowledge -> Knowledge
knowledgeMerge a b =
    { typosInStrings = a.typosInStrings ++ b.typosInStrings }

report : Knowledge -> List Review.Error
report knowledge =
    knowledge.typosInStrings
        |> List.map
            (\stringWithTypos ->
                { path = stringWithTypos.modulePath
                , message = "misspelled fruits.com"
                , details = [ "The typo of using frits.com instead of fruits.com has been made and noticed by users too many times. Our company is `fruits.com`, not `frits.com`." ]
                , range = stringWithTypos.range
                , fix =
                    [ { path = stringWithTypos.modulePath
                      , edits = [ Review.replaceRange stringWithTypos.range "fruits.com" ]
                      }
                    ]
                }
            )

rangeRelativeTo :
    Elm.Syntax.Range.Location
    -> (Elm.Syntax.Range.Range -> Elm.Syntax.Range.Range)
rangeRelativeTo baseStart =
    \offsetRange ->
        { start = offsetRange.start |> locationRelativeTo baseStart
        , end = offsetRange.end |> locationRelativeTo baseStart
        }

locationRelativeTo :
    Elm.Syntax.Range.Location
    -> (Elm.Syntax.Range.Location -> Elm.Syntax.Range.Location)
locationRelativeTo baseStart =
    \offsetLocation ->
        case offsetLocation.row of
            1 ->
                { row = baseStart.row
                , column = baseStart.column + offsetLocation.column
                }

            offsetRowAtLeast2 ->
                { row = baseStart.row + (offsetRowAtLeast2 - 1)
                , column = offsetLocation.column
                }
```

## when to create or enable a review

The bar to write or enable a review should be pretty high.
A new review can often turn out to be a nuisance to someone, sometimes in ways you didn't predict, so making sure the review solves a real problem, and that your team is on board with it, is important.
If a developer disagrees with a review, they may try to circumvent it, resulting in code that is even more error prone than the pattern that was originally forbidden.
So the value provided by the review should be much greater than the trouble it causes, and if you find that a review doesn't live up to this, consider disabling it.

Reviews are most useful when some concretely defined bad pattern must _never_ appear in the code and less useful when a pattern is _sometimes_ allowed to appear on a case-by-case basis (false positives). There is _no way to locally ignore specific review errors_, see ["How disable comments make static analysis tools worse" by Jeroen Engels](https://jfmengels.net/disable-comments/)
and similarly, there is no way to suppress legacy issues as lower-priority because in my opinion even these should always be visible as a (longer term) project checklist.

You can however [configure file paths (e.g. from vendored packages or generated code) for which no errors will be reported](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-mini/1.0.0/Review#ignoreErrorsForPathsWhere).
If you really need to make exceptions, which you most likely won't, the review should be configurable or detecting exception marks like `-- @allow-non-tco` must be written in the review itself.

Raise the bar for inclusion even higher for reviews that enforce a certain **coding style** or suggest simplifications to your code.
For example: If a record contains only one field, then I could suggest not using a record
and use the field value directly, which would make things simpler. But using a
record can have the advantage of being more explicit: `Dict String Range` is
harder to understand than `{ moduleInfosByPath : Dict String { headerNameRange : Range } }`.

Some reviews might suggest using advanced techniques to avoid pitfalls, which can make it harder for newcomers to get something done.
When enabling this kind of review, make sure that the message it gives is helpful enough to unblock users.

When wondering whether to enable a review, here's a checklist

  - We had problems with the set of patterns we want to forbid
  - We could not find a way to solve the problem by changing the API of the problematic code or introducing a new API
  - If the review already exists, we have read its documentation and the section about when not to enable the review
  - We have thought very hard about what the corner cases could be and what kind of patterns this would forbid that are actually acceptable
  - We think the review explains well enough how to solve the issue, to make sure beginners are not annoyed by it
  - I have communicated with my teammates and they all agree to enforce the review
  - I am ready to disable the review if it turns out to be more disturbing than helpful

## current rough spots of elm-review-mini

  - no LSP integration (shouldn't be too hard as it's similar to elm-review)
  - no ecosystem
  - the problem you encountered. Please [open an issue](https://github.com/lue-bird/elm-review-mini/issues) <3

--------


## feelings

I'm _scared_ of big projects, especially those with a lot of underlying covered area/complexity
because code structures become static:
  - You start to almost work around your code when you add new features – which again makes code harder to understand and harder to get into
  - Trying to make all those changes backwards-compatible introduces even more layers and wires everywhere
  - Keeping all the examples and tests up to date becomes daunting and somewhat draining, unexciting work

As a consequence, these big projects as a whole are more likely kept running by only a single (amazing) person, which sadly also makes the risk of abandonment or bus factor increasingly _scary_.
I know **I could neither maintain them well [🎈](https://jfmengels.net/a-nice-round-ball/) nor would I be happy doing so**.
I have no hopes that anyone else could either.

I'd be horribly sad for a tool as great as `elm-review` to become stale,
so I'll try hard to find any simplification or anything that reduces the covered area/responsibilities of the project, even if it makes things more rough for users (performance, convenience, even beginner friendliness!).
You have been warned: all your favorite APIs and tool interactions might get canned.
Most things that seem rather simple and super cool will sneakily demand "just this edge case as well". Wait a few years and we have elm-review all over again.
