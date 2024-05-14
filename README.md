> Status: unpublished, a few helpers still missing

elm-review-mini scans your [elm](https://elm-lang.org/) project to enforce conventions
using reviews written in elm and [published as packages](https://dark.elm.dmy.fr/?q=elm-review-mini-).
While heavily inspired by [`jfmengels/elm-review`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review/latest/), it has a much simpler API, much lighter internals (see also [the feeling section](#feelings)) but also almost no published reviews so far.

Use it in your project by adding this starter config with the CLI set up
```bash
curl -L https://github.com/lue-bird/elm-review-mini-cli-starter/tarball/master review-mini | tar xz
```
The created `review-mini/` is a self-contained elm application which means you can add new reviews with `elm install` just like any other elm project dependency.
Then add the review in your `review-mini/src/ReviewConfiguration.elm`:

```elm
module ReviewConfiguration exposing (configuration)

import SomeConvention
import Review


configuration : { reviews : List Review.Review, extraPaths : List String }
configuration =
    { extraPaths = [ "README.md" ]
    , reviews =
        [ SomeConvention.review SomeConvention.configurationOption
        -- , ...
        ]
    }
```

Discuss added reviews with your team, see ["when to enable a review"](#when-to-create-or-enable-a-review).

You can also [create custom reviews](https://package.elm-lang.org/packages/lue-bird/elm-review-mini/1.0.0/Review#create). An example of a review that prevents a typo in a string that was made too often at your company:
```elm
module StringWithMisspelledCompanyNameForbid exposing (review)

import Elm.Syntax.Expression
import Elm.Syntax.Node
import Review

review : Review.Review
review =
    Review.create
        { inspect =
            [ Review.inspectModule
                (\moduleData ->
                    moduleData.syntax.declarations
                        |> List.concatMap
                            (\(Elm.Syntax.Node.Node _ declaration) ->
                                case Elm.Syntax.Node.value declaration of
                                    Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                                        functionDeclaration.declaration
                                            |> Elm.Syntax.Node.value
                                            |> .expression
                                            |> List.concatMap Review.expressionAllSubs

                                    _ ->
                                        []
                            )
                        |> List.filterMap
                            (\expressionNode ->
                                case expressionNode of
                                    Elm.Syntax.Node.Node range (Elm.Syntax.Expression.Literal string) ->
                                        if string |> String.contains "frits.com" then
                                            { modulePath = moduleData.path, range = range } |> Just

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )
                )
            ]
        , knowledgeMerge = \a b -> a ++ b
        , report =
            \stringsWithTypos ->
                stringsWithTypos
                    |> List.map
                        (\misspelledName ->
                            { target = Review.FileTargetModule misspelledName.modulePath
                            , message = "Replace `frits.com` by `fruits.com`"
                            , details = [ "This typo has been made and noticed by users too many times. Our company is `fruits.com`, not `frits.com`." ]
                            , range = misspelledName.range
                            , fix = []
                            }
                        )
        }
```

## when to create or enable a review

The bar to write or enable a review should be pretty high.
A new review can often turn out to be a nuisance to someone, sometimes in ways you didn't predict, so making sure the review solves a real problem, and that your team is on board with it, is important.
If a developer disagrees with a review, they may try to circumvent it, resulting in code that is even more error prone than the pattern that was originally forbidden.
So the value provided by the review should be much greater than the trouble it causes, and if you find that a review doesn't live up to this, consider disabling it.

Review reviews are most useful when some pattern must never appear in the code.
It gets less useful when a pattern is allowed to appear in certain cases, as there is [no good solution for handling exceptions to reviews](#what-if-i-disagree-with-a-review-on-a-specific-case-in-my-code).
If you really need to make exceptions, they must be written in the review itself, or the review should be configurable.

Raise the bar for inclusion even higher for reviews that enforce a certain **coding style** or suggest simplifications to your code.
For example: If a record contains only one field, then I could suggest not using a record
and use the field value directly, which would make things simpler. But using a
record can have the advantage of being more explicit: `Dict String Range` is
harder to understand than `{ moduleInfosByPath : Dict String { headerNameRange : Range } }`.

Some reviews might suggest using advanced techniques to avoid pitfalls, which can make it harder for newcomers to get something done.
When enabling this kind of review, make sure that the message it gives is helpful enough to unblock users.

When wondering whether to enable a review, here's a checklist

  - [ ] I have had problems with the pattern I want to forbid
  - [ ] I could not find a way to solve the problem by changing the API of the problematic code or introducing a new API
  - [ ] If the review exists, I have read its documentation and the section about when not to enable the review, and it doesn't apply to my situation
  - [ ] I have thought very hard about what the corner cases could be and what kind of patterns this would forbid that are actually okay, and they are acceptable
  - [ ] I think the review explains well enough how to solve the issue, to make sure beginners are not blocked by it
  - [ ] I have communicated with my teammates and they all agree to enforce the review
  - [ ] I am ready to disable the review if it turns out to be more disturbing than helpful

## What if I disagree with a review on a specific case in my code?

`elm-review-mini` does not provide a way to disable errors on a case-by-case basis like a lot of static analysis tools do, see ["How disable comments make static analysis tools worse" by Jeroen Engels](https://jfmengels.net/disable-comments/).
Similarly, it does not come with a system to suppress legacy issues as lower-priority because in my opinion even these should always be visible as a (longer term) project checklist.

Since you can't ignore errors, the burden is on the reviews to be of higher quality, **avoiding those with inherent exceptions or false positives**.

However! You can [mark specific kinds of files (e.g. from vendored packages) as not relevant to a review, preventing errors to be reported for those](https://package.elm-lang.org/packages/lue-bird/elm-review-mini/1.0.0/Review#ignoreErrorsForPathsWhere).

It is a good fit if you wish for `elm-review-mini` to not report errors in vendored or generated code,
or in files and directories that by the nature of the review should be exempted.


--------


## feelings

I'm _scared_ of big projects, especially those with a lot of underlying covered area/complexity
because code structures become static:
  - You start to almost work around your code when you add new features – which again makes code harder to understand and harder to get into
  - Trying to make all those changes backwards-compatible introduces even more layers and wires everywhere
  - Keeping all the examples and tests up to date becomes daunting and somewhat draining, unexciting work

I know I could neither maintain them well nor would I be happy doing so.
And neither do I have hopes that anyone could.
For big projects run by a single person, the bus factor becomes increasingly scary as well
and I'd be horribly sad for a tool as great as `elm-review` to become stale/abandoned.

I'll try hard to find any simplification or anything that reduces the covered area/responsibilities of the project, even if it makes things more rough for users (performance, convenience, even beginner friendliness!).
You have been warned: all your favorite APIs and tool interactions might get canned.
Most things that seem rather simple and super cool will sneakily demand "just this edge case as well". Wait a few years and we have elm-review all over again.
