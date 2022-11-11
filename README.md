# Guerilla Checkers

An asymmetric board game combining Go and Checkers. Developed by Brian Train, written in the [Elm language](https://guide.elm-lang.org/) by me.

More details on the game: https://boardgamegeek.com/boardgame/71035/guerrilla-checkers

## Build

Setup &amp; build in the way you normally build Elm. 

- Make sure Elm is installed: https://guide.elm-lang.org/install/elm.html
- To build into a readily-distributable package, run the `make.sh` in the repository root:

      $ ./make.sh

  The complete package will be copied to `./dist`. You can load `./dist/index.html` in a browser to see the full application.
- To inspect or modify the Elm code locally, run `elm reactor` in the repository root &amp; navigate to `src/GuerillaCheckers.elm`:

      $ elm reactor
      Go to http://localhost:8000 to see your project dashboard.

  Note that you will be missing some CSS styling when running the application this way.
