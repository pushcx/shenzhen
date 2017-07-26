# shenzhen

Solves games of Shenzhen Solitaire,
a minigame [available standalone](http://store.steampowered.com/app/570490/SHENZHEN_SOLITAIRE/)
or as part of [Shenzhen I/O](http://www.zachtronics.com/shenzhen-io/)
from [Zachtronics](http://www.zachtronics.com/).
Here's an [video intro to the gameplay](https://www.youtube.com/watch?v=kT7bEJQsVYk).

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
  - [ ] Game type
  - [x] Use [standard terms](https://en.wikipedia.org/wiki/Glossary_of_patience_terms)
  - [x] Move type for card moves, collecting dragons
  - [ ] `move :: Game -> Move -> Game`
    - [ ] `move` with `MoveFromColumnToCell`
    - [ ] `move` with `MoveFromCellToColumn`
    - [ ] `move` with `BuildFromColumn`
    - [ ] `move` with `BuildFromCell`
    - [x] `move` with `Pack`
    - [x] `move` with `CollectDragons`
  - [ ] replicate automatic build of released `Card`s
  - [ ] Automatic moves of available cards
  - [ ] Detect game win
  - [ ] Detect game loss
  - [ ] Generate list of possible Moves for a position
  - [ ] Filter possible Moves against Game history to avoid loops
  - [ ] Take moves until game win/loss
  - [ ] QuickCheck that the number + distribution of cards in the game is constant

## Cleanups and open questions:

  - [ ] Encode that `Tableau` has exactly one `Foundation` per `Suit`
  - [ ] Encode that `Tableau` has exactly one `DragonCell` per `Suit`
  - [ ] Look at `Bound` or `Enum` for `Rank`
  - [ ] `mayTakeTo` and `mkRunTo` want some kind of help
  - [ ] The `Move` constructors must enforce validity to avoid passing around broken `Move` data, but then `move` has none. Does this make `move` clear or unsafe? What if this was more mature with `Move` in its own module not exporting the default constructor?
  - [ ] This ties into `mayTakeTo` and `unsafeTakeTo`. `mkMove` must use the former but `move` really wants unsafe to avoid unwrapping `Maybe`. I can't even see how to unwrap it, really.
