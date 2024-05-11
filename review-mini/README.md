Minimal setup for an [`elm-review-mini`](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-mini/latest/) CLI program (requires `node.js` and `npm` to be installed).
Before the first run,
```bash
npm install --prefix review-mini
```

Launch
```bash
node review-mini/cli
```
which watches for changes to your files and reports errors for you with fixes to accept/reject. 

Since the CLI is a self-contained elm application, you can add new reviews with `elm install` just like any other elm project dependency ([search for packages elm-review-mini-...](https://dark.elm.dmy.fr/?q=elm-review-mini-) for example).
And don't forget to actually put it in the list in `src/ReviewConfiguration.elm` and configure it :)
