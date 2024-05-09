import * as ReviewCli from "@lue-bird/elm-review-mini-cli"

const elmApp = ReviewCli.compileElm(import.meta.dirname).init()
ReviewCli.programStart(elmApp.ports)
