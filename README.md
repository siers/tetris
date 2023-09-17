# tetris solver

A tetris solver on top of [samtay/tetris](https://github.com/samtay/tetris/).
It got me a `1_000_000` score until I was no longer interested in waiting.

Bruteforces all two next moves with the scoring function that

* penalizes each occupied block on the screen
* penalizes each free block with occupied blocks above it.
* weighs each block according to its height in the game (high is bad)
* scores by (scoreAfterTwoMoves, scoreAfterFirstMove), which is important

See more: [siers/tetris/blob/solver/src/Solve.hs](https://github.com/siers/tetris/blob/solver/src/Solve.hs).

<p align="center">
  <img width="600" src="/docs/img/solve.svg">
</p>

## installation

#### Nix
```bash
nix shell --run 'cabal run tetris'
```
#### install from source
First [get stack](https://docs.haskellstack.org/en/stable/README/#how-to-install). Then
```bash
git clone https://github.com/samtay/tetris.git
cd tetris
stack install tetris
```

## usage

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

## tips

#### troubleshooting
People seem to have varying levels of success with the linux binary. Please note that it is compiled dynamically and hence should not be expected to work on most distros. If you have other problems, feel free to open an issue.

#### roll your own
If you like games in your terminal and have an interest in functional programming, write your own! This code is built on top of [brick](https://github.com/jtdaugherty/brick) which makes building terminal user interfaces very accessible. I also have a [tutorial](https://samtay.github.io/posts/introduction-to-brick) that can help you get started.
