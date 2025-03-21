# Game Overview

This module implements the core logic of the game. It includes:
- Board creation and internal representation.
- Puzzle generation (with difficulty levels).
- Move validation and board mutation.
- Solving and solution checking.
- Game status tracking and completion handling.

## Board creation
- Fixed: immutable cell
- Mutable: user inputs
- Empty: empty cell

## Puzzle generation
- Fill diaonal 3x3 randomly (Fisher-Yates shuffle):
    + Used to randomize 1-9 values in place
- Solve the board using backtracking:
    + Randomizes number trial order.
    + Skips already-filled cells.
    + Validates each value if it is a valid number.
- Remove values to match difficulty level

## Game Hints
