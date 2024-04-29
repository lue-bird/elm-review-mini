# Why a fork?
## goals and ideas

  - much simpler API and internals 
      - enabling docs that cover the basics in a very concise manner,
        then guiding through tons of examples if you feel like it.
  - composable inspections
      - much of the complexity and perf choke points in `elm-review` come from the dependency between inspections, see e.g. https://github.com/jfmengels/elm-review/issues/168
      - multiple reviews can feed off the same bunch of collected contexts, published as packages by users. Examples: "data to determine all bindings in scope", "data to determine a given reference's full origin", "data to determine the reference's minimum qualification", "type information"
  - all the nice helpers: `Type/Pattern/Expression.map/subs/fold` etc

> Status: Basic API is promising, implementation is still brewing (CLI especially will take a while)

Directly back-porting these changes to `elm-review` is not an explicit goal for me
and breaking changes etc are explicitly not avoided.

## feelings

I'm _scared_ of big projects, especially those with a lot of underlying covered area/complexity
because code structures become static:
  - You start to almost work around your code when you add new features – which again makes code harder to understand and harder to get into
  - Trying to make all those changes backwards-compatible introduces even more layers and wires everywhere
  - Keeping all the examples and tests up to date becomes daunting and somewhat draining, unexciting work

I know I could neither maintain them well nor would I be happy doing so.
And neither do I have hopes that anyone could.
For big projects run by a single person, the bus factor becomes increasingly scary as well
and I'd be horribly sad for a great tool such as `elm-review` to become stale/abandoned.

I'll try hard to find any simplification or anything that reduces the covered area/responsibilities of the project, even if it makes things a bit more rough for users (performance, convenience).

# elm-review-mini

`elm-review-mini` scans your [elm](https://elm-lang.org/) project to find bugs and enforce conventions.
Each review is written in elm and is [published as a package](https://dark.elm.dmy.fr/?q=elm-review-mini-). There are no built-in reviews.

You can run `elm-review-mini` from the command line (requires `node.js` and `npm` to be installed).

Clone a starter config:
```bash
curl -L https://github.com/lue-bird/elm-review-mini-cli-starter/tarball/master review-mini | tar xz
```
The created `review-mini/` is a self-contained elm application which means you can add new reviews with `elm install` (e.g. [search for packages elm-review-mini-...](https://dark.elm.dmy.fr/?q=elm-review-mini-)), just like any other elm project dependency.
And don't forget to actually put it in the list in `src/Reviews.elm` and configure it :)

Beware how and why you introduce reviews in your project though.
If a review seems like the best solution, remember to discuss it with your team.
It's easy to mix up patterns that are objectively bad, with patterns that you personally find problematic, and forbidding patterns that other people find useful can be very disruptive.
Read also: [when to enable a review](#when-to-write-or-enable-a-review).

## bring your own reviews

I encourage you to write custom reviews yourself (and if it makes sense publish them for everyone to benefit)

  - enforce that e.g. image paths only live in an `Images` module, which other modules can reference
  - make everyone use a common `Button` ui module, instead of creating their own

Check out the [`Review`](https://package.elm-lang.org/packages/lue-bird/elm-review-mini/1.0.0/Review/) documentation for how to get started.

Here's an example of a review that prevents a typo in a string that was made too often at your company.

```elm
module StringWithMisspelledCompanyNameForbid exposing (review)

import Elm.Syntax.Expression
import Elm.Syntax.Node
import Review
import Serialize

review : Review.Review
review =
    Review.create
        { name = "StringWithMisspelledCompanyNameForbid" -- same as the module
        , inspect =
            [ Review.inspectModule
                (\module ->
                    module.ast.declarations
                        |> List.filterMap Review.declarationToValueOrFunction
                        |> List.concatMap Review.expressionAllSubs
                        |> List.filterMap
                            (\expressionNode ->
                                case expressionNode of
                                    Elm.Syntax.Node.Node range (Elm.Syntax.Expression.Literal string) ->
                                        if string |> String.contains "frits.com" then
                                            { modulePath = module.path, range = range } |> Just

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )
                )
            ]
        , report =
            \stringsWithTypos ->
                stringsWithTypos
                    |> List.map
                        (\error ->
                            { target = Review.FileTargetModule error.modulePath
                            , message = "Replace `frits.com` by `fruits.com`"
                            , details = [ "This typo has been made and noticed by users too many times. Our company is `fruits.com`, not `frits.com`." ]
                            , range = error.range
                            }
                        )
        , contextMerge = \a b -> a ++ b
        , contextSerialize =
            Serialize.list
                (Serialize.record (\modulePath range -> { modulePath = modulePath, range = range })
                    |> Serialize.field .modulePath Serialize.string
                    |> Serialize.field .range Serialize.string
                    |> Serialize.finishRecord
                )
        }

serializeRange : Serialize.Codec Elm.Syntax.Range.Range
serializeRange =
    Serialize.record (\start end -> { start = start, end = end })
        |> Serialize.field .start serializeLocation
        |> Serialize.field .end serializeLocation
        |> Serialize.finishRecord

serializeLocation : Serialize.Codec Elm.Syntax.Location.Location
serializeLocation =
    Serialize.record (\row column -> { row = row, column = column })
        |> Serialize.field .row Serialize.int
        |> Serialize.field .column Serialize.int
        |> Serialize.finishRecord
```

Then add the review in your runner:

```elm
module Reviews exposing (reviews)

import NoStringWithMisspelledCompanyName
import Review


reviews : List Review.Review
reviews =
    [ NoStringWithMisspelledCompanyName.review
    -- , ...
    ]
```

## when to write or enable a review

The bar to write or enable a review should be pretty high.
A new review can often turn out to be a nuisance to someone, sometimes in ways you didn't predict, so making sure the review solves a real problem, and that your team is on board with it, is important.
If a developer disagrees with a review, they may try to circumvent it, resulting in code that is even more error prone than the pattern that was originally forbidden.
So the value provided by the review should be much greater than the trouble it causes, and if you find that a review doesn't live up to this, consider disabling it.

Review reviews are most useful when some pattern must never appear in the code.
It gets less useful when a pattern is allowed to appear in certain cases, as there is [no good solution for handling exceptions to reviews](#is-there-a-way-to-ignore-errors-).
If you really need to make exceptions, they must be written in the review itself, or the review should be configurable.

For reviews that enforce a certain **coding style**, or suggest simplifications to your code, I would ask you to raise the bar for inclusion even higher.
A few examples:

  - I much prefer using `|>` over `<|`, and I think using the latter to pipe
  functions over several lines is harder to read. Even if using `|>` was indeed
  better for most situations and even if my teammates agree, this would prevent
  me from writing tests [the suggested way](https://github.com/elm-explorations/test#quick-start)
  for instance.
  - If a record contains only one field, then I could suggest not using a record
  and use the field directly, which would make things simpler. But using a
  record can have the advantage of being more explicit: `findFiles [] folder` is
  harder to understand than `findFiles { exceptions = [] } folder`.

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

`elm-review-mini` does not provide a way to disable errors on a case-by-case basis like a lot of static analysis tools do, see _[How disable comments make static analysis tools worse](https://jfmengels.net/disable-comments/)_.
Similarly, it does not come with a system to suppress legacy issues as lower-priority because in my opinion even these should always be visible as a (longer term) project checklist.

Since you can't ignore errors, the burden is on the reviews to be of higher quality, avoiding those with inherent exceptions or false positives.

However! You can [mark specific kinds of files as not relevant to a review, preventing errors to be reported for those](https://package.elm-lang.org/packages/lue-bird/elm-review-mini/1.0.0/Review#ignoreErrorsForFilesWhere).

It is a good fit if you wish for `elm-review-mini` to not report errors in vendored or generated code,
or in files and directories that by the nature of the review should be exempted.
