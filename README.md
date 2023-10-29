# Tetris solver

A tetris solver on top of [samtay/tetris](https://github.com/samtay/tetris/).
It got me a `10_000_000` score until I was no longer interested in waiting.

Bruteforces (see more: [Solve.hs](https://github.com/siers/tetris/blob/solver/src/Solve.hs)) all two next moves with the scoring function that

* penalizes each occupied block on the screen,
* penalizes each free block with occupied blocks above it,
* weighs each block according to its height in the game (high is bad),
* scores by (scoreAfterTwoMoves, scoreAfterFirstMove), which is important.

<p align="center">
  <img width="600" src="/docs/img/solve.svg">
</p>

## UI features on top of samtay's tetris

* Left/right rotation buttons
* Save-game implemented (`~/.local/share/tetris/saved.json`)
* Has an autosuggest feature (and solve)
* Pause has the board visible behind "Paused"

## Installation

#### Nix
```bash
nix build github:siers/tetris/solver
```

## Usage

The default game is run by simply executing the `tetris` command.
If the unicode characters look a bit
wonky in your terminal, you can also run
```shell
tetris --ascii-only         # uses [] as preview cell
# or
tetris --preview-chars 'XX' # uses custom characters as preview cell
```
If you want to skip the level prompt, you can start the game immediately via
```shell
tetris --level n
```
Lastly, to see the current high score, you can run `tetris --high-score`.
And of course, see `tetris --help` for help.

## Development

Enter the development shell with `nix develop`.

Format the source with `fourmolu --indentation 2 **/*.hs -i`.
