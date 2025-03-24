# Architecture Overview

## Layers

### UI Layer
Uses GTK (via LablGTK) and Cairo for rendering and user interaction:
- Render the Sudoku grid and numbers.
- Highlight selected cells and invalid inputs.
- Display menu (new game, difficulty, hints, quit)
- Handles keyboard and mouse input events.

### Game State and Validation Layers
Core logic to handle:
- Fixed: immutable cell.
- Mutable: user inputs.
- Empty: empty cell.
- Hint calculation for empty cells.
- Sudoku rule validation (row, column, 3x3 box).
- Win condition checking.
- Tracking invalid user inputs.

## Board Generation Logic
1. Fill Diagonal 3x3 Boxes:
- Each diagonal box is filled independently using Fisher-Yates shuffle to randomize values 1-9.

2. Solve Full Board With Backtracking:
Recursive backtracking.
- Fills cells left-to-right, top-to-bottom.
- Skips filled cells.
- Triese values 1-9 in shuffled order.
- Validates each placement using Sudoku constraints.
- Backtracks on conflicts until the board is complete.

3. Remove Cells to Set Puzzle Difficulty

## Game Hints
- Calculates all possible valid numbers for each empty cells.

## Invalid Cell Tracking
- Uses a hash table to track cells that contain invalid user inputs.
- When a move is illegal (duplicate in row, colu;n, or box) the cell is marked red.
- Valid input automatically clear previous invalid markers.

## UI System (GTK + Cairo)
Rendering:
- Grid drawn with Cairo, using:
    + Thin and thick lines (every 3 rows/columns).
    + Color-coded text and cell backgrounds.
- Use different colors for: background, fixed value (black), etc.
- Input Handling:
    + Keyboard:
        ++ Arrow keys to navigate cells.
        ++ 1-9 input values
        ++ Delete/Backspace: to clear a cell.
    + Mouse:
        ++ Click to select a cell.

## Game Completion Logic
- After each valid move, the game checks if:
    + All cells are filled.
    + All rows, columns, and boxes are valid.
- On success:
    + Displays a dialog with congratulatory message.
    + Offers to start a new game.