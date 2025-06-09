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
- **Professional Puzzle Generation**: Guarantees unique solutions using strategic removal algorithm
- **Multiple Difficulty Levels**: Easy, Medium, and Hard puzzles with authentic difficulty calibration
- **Smart Hints System**: Shows possible valid numbers for each cell
- **Input Validation**: Real-time validation of moves with constraint satisfaction
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
opam install alcotest
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

## Puzzle Generation Algorithm

This implementation features a **professional-grade puzzle generator** that guarantees high-quality Sudoku puzzles:

### Unique Solution Guarantee
- Every generated puzzle has **exactly one solution** (no ambiguous puzzles)
- Uses strategic cell removal with solution validation
- Prevents unsolvable or multiple-solution puzzles

### Three-Phase Generation Process

1. **Foundation Phase**: Fill diagonal 3×3 boxes independently
   - Reduces search space from O(9^81) to O(9^54)
   - Boxes at (0,0), (3,3), (6,6) have no overlapping constraints

2. **Completion Phase**: Complete board using backtracking
   - Guarantees valid, complete Sudoku solution
   - Randomization ensures variety between puzzles

3. **Strategic Removal Phase**: Create puzzle by removing cells
   - Tests each removal to maintain solution uniqueness
   - Stops when target difficulty reached or no more safe removals possible

### Difficulty Calibration
- **Easy**: ~35 cells removed (~57% filled) - Gentle introduction
- **Medium**: ~45 cells removed (~44% filled) - Moderate challenge
- **Hard**: ~55 cells removed (~32% filled) - Significant challenge

**Note**: Actual difficulty may vary based on puzzle structure. The algorithm prioritizes puzzle quality over exact cell count targets.