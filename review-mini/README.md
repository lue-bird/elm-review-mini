[`elm-review-mini`](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-mini/latest/) configuration and CLI setup (requires the latest `node.js` and `npm` to be installed).
Before the first run,
```bash
npm install --prefix review-mini
```

To watch for changes to your files and reports errors for you with fixes to accept/reject:
```bash
node review-mini/cli
```

Since the CLI is a regular elm application, you can add new reviews with `elm install` ([search for packages elm-review-mini-...](https://dark.elm.dmy.fr/?q=elm-review-mini-) for example), then put them in the list in `src/ReviewConfiguration.elm` and configure them.
