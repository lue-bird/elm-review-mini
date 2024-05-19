import * as ReviewCli from "@lue-bird/elm-review-mini-cli"

const elmApp = ReviewCli.compileElm(import.meta.dirname, "src/Cli.elm").init()
ReviewCli.startWatching(elmApp.ports)
