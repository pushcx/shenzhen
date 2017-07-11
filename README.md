# shenzhen

Solves games of Shenzhen Solitaire,
a minigame [available standalone](http://store.steampowered.com/app/570490/SHENZHEN_SOLITAIRE/)
or as part of [Shenzhen I/O](http://www.zachtronics.com/shenzhen-io/)
from [Zachtronics](http://www.zachtronics.com/).

![gameplay screenshot](screenshot.jpg)

Written for practice with Haskell and HaskellStack.

## TODO

  - [x] create Stack project
  - [x] types for cards
  - [x] create standard deck
  - [x] Layout type
  - [x] custom Show instances for Card, Layout
  - [ ] enter game Layout
  - [x] shuffle deck to create Layout
  - [x] Game type
  - [x] Use [standard terms](https://en.wikipedia.org/wiki/Glossary_of_patience_terms)
  - [ ] Move type for card moves, collecting dragons
  - [ ] move :: Game -> Move -> Game
  - [ ] replicate automatic build of released `Card`s
  - [ ] Automatic moves of available cards
  - [ ] Detect game win
  - [ ] Detect game loss
  - [ ] Generate list of possible Moves for a position
  - [ ] Filter possible Moves against Game history to avoid loops
  - [ ] Take moves until game win/loss

## Cleanups:
  - [ ] Encode that `Game` has exactly one `Foundation` per `Suit`
  - [ ] Look at `Bound` or `Enum` for `Rank`
  - [ ] `mayTakeTo` and `mkRunTo` want some kind of help
