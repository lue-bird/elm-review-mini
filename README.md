## What is the point of this fork? goals

  - much simpler API and internals 
      - enabling docs that cover the basics in a very concise manner,
        then guiding through tons of examples if you feel like it.
  - composable inspections so that multiple reviews can feed off the same bunch of collected contexts, published as packages by users. Examples: "data to determine all bindings in scope", "data to determine a given reference's full origin", "data to determine the reference's minimum qualification", "type information"
  - all the nice helpers: `Type/Pattern/Expression.map/subs/fold` etc

> Status: Basic API seems promising, implementation is still brewing

Directly back-porting these changes to `elm-review` is not an explicit goal
and breaking changes etc are explicitly not avoided.

## What is the point of this fork? feelings

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

[![elm-review reporter output](https://github.com/lue-bird/elm-review-mini/blob/1.0.0/documentation/images/elm-review-report.png?raw=true)](https://github.com/lue-bird/elm-review-mini/blob/1.0.0/documentation/images/elm-review-report.png?raw=true)

Each review is written in elm and is [published as a package](https://dark.elm.dmy.fr/?q=elm-review-mini-). There are [no built-in rules]((https://github.com/lue-bird/elm-review-mini/blob/master/documentation/design/no-built-in-rules.md)).

You can run `elm-review-mini` from the command line (requires `node.js` and `npm` to be installed).

Clone a starter config:
```bash
curl -L https://github.com/lue-bird/elm-review-mini-cli-starter/tarball/master review-mini | tar xz
```
The created `review-mini/` is a self-contained elm application which means you can add new reviews with `elm install` (e.g. [search for packages elm-review-mini-...](https://dark.elm.dmy.fr/?q=elm-review-mini-)), just like any other elm project dependency.
And don't forget to actually put it in the list in `src/Main.elm` and configure it :)

Beware how and why you introduce rules in your project though.
If a rule seems like the best solution, remember to discuss it with your team.
It's easy to mix up patterns that are objectively bad, with patterns that you personally find problematic, and forbidding patterns that other people find useful can be very disruptive.
Read also: [when to enable a rule](#when-to-write-or-enable-a-rule).

## bring your own reviews

I encourage you to write custom reviews yourself (and if it makes sense publish them for everyone to benefit)

  - enforce that e.g. image paths only live in an `Images` module, which other modules can reference
  - make everyone use a common `Button` ui module, instead of creating their own

Check out the [`Review`](https://package.elm-lang.org/packages/lue-bird/elm-review-mini/1.0.0/Review/) documentation for how to get started.

Here's an example of a rule that prevents a typo in a string that was made too often at your company.

```elm
module NoStringWithMisspelledCompanyName exposing (rule)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)

-- Create a new rule
rule : Rule
rule =
    -- Define the rule with the same name as the module it is defined in
    Review.create
        { name = "StringWithMisspelledCompanyNameForbid"
        , inspect =
            [ Review.inspectModule
                (\module ->
                    module.ast.declarations
                        |> List.filterMap Review.declarationToValueOrFunction
                        |> List.concatMap Review.expressionAllSubs
                        |> List.filterMap
                            (\expressionNode ->
                                case expressionNode of
                                    Elm.Syntax.Node.Node strRange (Elm.Syntax.Expression.Literal str) ->
                                        if String.contains "frits.com" str then
                                            { modulePath = module.path, range = strRange } |> Just

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

Then add the rule in your configuration:

```elm
module Main exposing (main)

import NoStringWithMisspelledCompanyName
import Review


main : Review.Program
main =
    Review.program
        [ NoStringWithMisspelledCompanyName.rule
        -- other rules...
        ]
```

## When to write or enable a rule

The bar to write or enable a rule should be pretty high.
A new rule can often turn out to be a nuisance to someone, sometimes in ways you didn't predict, so making sure the rule solves a real problem, and that your team is on board with it, is important.
If a developer disagrees with a rule, they may try to circumvent it, resulting in code that is even more error prone than the pattern that was originally forbidden.
So the value provided by the rule should be much greater than the trouble it causes, and if you find that a rule doesn't live up to this, consider disabling it.

Review rules are most useful when some pattern must never appear in the code.
It gets less useful when a pattern is allowed to appear in certain cases, as there is [no good solution for handling exceptions to rules](#is-there-a-way-to-ignore-errors-).
If you really need to make exceptions, they must be written in the rule itself, or the rule should be configurable.

For rules that enforce a certain **coding style**, or suggest simplifications to your code, I would ask you to raise the bar for inclusion even higher.
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

Some rules might suggest using advanced techniques to avoid pitfalls, which can make it harder for newcomers to get something done.
When enabling this kind of rule, make sure that the message it gives is helpful enough to unblock users.

When wondering whether to enable a rule, I suggest using this checklist:
  - [ ] I have had problems with the pattern I want to forbid.
  - [ ] I could not find a way to solve the problem by changing the API of the problematic code or introducing a new API.
  - [ ] If the rule exists, I have read its documentation and the section about when not to enable the rule, and it doesn't apply to my situation.
  - [ ] I have thought very hard about what the corner cases could be and what kind of patterns this would forbid that are actually okay, and they are acceptable.
  - [ ] I think the rule explains well enough how to solve the issue, to make sure beginners are not blocked by it.
  - [ ] I have communicated with my teammates and they all agree to enforce the rule.
  - [ ] I am ready to disable the rule if it turns out to be more disturbing than helpful.

## What if I disagree with a review on a specific case in my code?

`elm-review-mini` does not provide a way to disable errors on a case-by-case basis — by line or sections of code like a lot of static analysis tools do, see _[How disable comments make static analysis tools worse](https://jfmengels.net/disable-comments/)_.

Because you can't ignore errors easily, `elm-review` puts more burden on the rules, requiring them to be of higher quality
— less false positives — and better designed — avoiding rules that will inherently have lots of exceptions or false positives.

However! You can [configure exceptions](https://package.elm-lang.org/packages/lue-bird/elm-review-mini/1.0.0/Review-Rule/#configuring-exceptions),
which consists of marking specific directories or files as not relevant to a review, preventing errors to be reported for those.

It is a good fit if you wish for `elm-review` to not report errors in vendored or generated code,
or in files and directories that by the nature of the rule should be exempted.
