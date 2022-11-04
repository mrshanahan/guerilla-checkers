# Guerilla Checkers

An asymmetric board game combining Go and Checkers. Developed by Brian Train, written in the [Elm language](https://guide.elm-lang.org/) by me.

More details on the game: https://boardgamegeek.com/boardgame/71035/guerrilla-checkers

## Build

Setup &amp; build in the way you normally build Elm. 

- Make sure Elm is installed: https://guide.elm-lang.org/install/elm.html
- To build into a readily-distributable package, run `elm make` in the repository root on the `GuerillaCheckers.elm` file &amp; copy the resulting `index.html`:

    $ elm make src/GuerillaCheckers.elm

- To simply inspect it locally, run `elm reactor` in the repository root &amp; navigate to `src/GuerillaCheckers.elm`:

    $ elm reactor
    Go to http://localhost:8000 to see your project dashboard.

