# Sudoku OCaml

A Sudoku game implementation in OCaml with a GTK3-based graphical user interface using LablGtk3 and Cairo2 for rendering. This project demonstrates functional programming principles and OCaml's capabilities in creating interactive applications with modern GUI libraries.

<figure>
  <img src="pics/no_hints.png" alt="Sudoku Game Board With No Hints">
  <figcaption>Figure 1: Initial game board with empty cells, fixed numbers, valid numbers</figcaption>
</figure>

<figure>
  <img src="pics/with_hints.png" alt="Sudoku Game Board With Hints">
  <figcaption>Figure 2: Game board with hints showing possible numbers for each empty cell</figcaption>
</figure>

<figure>
  <img src="pics/wrong.png" alt="Sudoku Game Board With Wrong Move">
  <figcaption>Figure 3: Game board showing an invalid move highlighted in red</figcaption>
</figure>

<figure>
  <img src="pics/finished.png" alt="Sudoku Game Board Finished">
  <figcaption>Figure 4: Completed game board with all cells correctly filled</figcaption>
</figure>

## Features

- **Graphical User Interface**: Built with LablGtk3 and Cairo2 for smooth rendering
- **Multiple Difficulty Levels**: Easy, Medium, and Hard puzzles
- **Smart Hints System**: Shows possible valid numbers for each cell
- **Input Validation**: Real-time validation of moves
- **Game State Tracking**: Tracks invalid moves and game completion
- **Responsive Design**: Adapts to window resizing
- **Cross-Platform**: Works on Linux, macOS, and Windows

## Prerequisites

- OCaml (version 4.14.0 or later)
- OPAM (OCaml Package Manager)
- GTK3 development libraries
- Cairo2 graphics library
- LablGtk3 OCaml bindings for GTK3
- Dune build system (version 2.9.0 or later)

## Installation

1. Install OCaml and OPAM:
```bash
# Ubuntu/Debian
sudo apt-get install opam

# macOS
brew install opam
```

2. Initialize OPAM:
```bash
opam init
eval $(opam env)
```

3. Install required packages:
```bash
opam install dune lablgtk3 cairo2
```

4. Clone and navigate to the project:
```bash
git clone <repository-url>
cd Sudoku_OCaml
```

5. Build the project:
```bash
dune build
# or using Makefile
make
```

## Running the Game

To start the game:
```bash
dune exec bin/main.exe
# or using Makefile
make sudoku
```

## Testing

The project includes comprehensive tests using Alcotest. To run the tests:

```bash
dune runtest
```

## Development Dependencies

For development and testing, you'll also need:
```bash
opam install alcotest qcheck
```

## Debug
```bash
dune build && dune exec _build/default/bin/main.exe
```

## Game Controls

- **Mouse**: Left-click to select cells
- **Arrow Keys**: Navigate between cells (Up, Down, Left, Right)
- **Number Keys (1-9)**: Enter numbers in selected cells (supports both regular and numpad keys)
- **Backspace/Delete/Numpad 0**: Clear selected cell
- **Menu Options**:
  - **Game → New Game**: Choose difficulty (Easy, Medium, Hard) and start a new puzzle
  - **Game → Show Hints**: Toggle display of possible numbers for each cell
  - **Game → Quit**: Exit the game

## Game Features

- **Invalid Move Detection**: Invalid moves are highlighted in red
- **Auto Game Completion**: When you complete a puzzle, a dialog will ask if you want to start a new game
- **Hint System**: Shows all possible valid numbers for empty cells when enabled