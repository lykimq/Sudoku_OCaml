# Demostrate Board Generate Example

## Why ShufflePool?

### Without ShufflePool

```ocaml
(* Native approach - expensive *)
let get_random_numbers () =
 let num = Array.init 9 (fun i -> i + 1) in
 Fisher_yates.shuffle nums (* O(n) operation *)
```

- Create new array allocation every time
- Performs expensive shuffle operation repeatedly
- Higher memory pressure during generation
- O(n) cost for ech randomization request

## With ShufflePool

```ocaml
let pool = ShufflePool.create () (* Pre-compute 10 shuffled arrays *)
let nums = ShufflePool.get_shuffle pool ~index:0 ~rgn (* Reuse + re-shuffle *)
```

- Pre-compute shuffle operations
- Reuse pre-allocated arrays
- Still maintains randomness through re-shuffling
- Reduces allocation pressure


## Example Board Generation Pipeline

### Step 1: Initialize ShufflePool

```ocaml
(* Create pool with 10 pre-shuffled arrays *)
let pool = ShufflePool.create ()

(* Pool contains 10 different shuffled versions of [1;2;3;4;5;6;7;8;9] *)
pool[0] = [|3; 7; 1; 9; 2; 5; 8; 4; 6|]
pool[1] = [|8; 2; 6; 4; 1; 9; 3; 7; 5|]
pool[2] = [|5; 9; 4; 7; 6; 3; 1; 2; 8|]
pool[3] = [|2; 1; 8; 3; 5; 7; 9; 6; 4|]
...
pool[9] = [|6; 4; 9; 1; 8; 2; 7; 5; 3|]
```

### Step 2: Create Empty Board

```
Initial Board (all Empty):
┌───────────────────────┐
│ _ _ _ │ _ _ _ │ _ _ _ │
│ _ _ _ │ _ _ _ │ _ _ _ │
│ _ _ _ │ _ _ _ │ _ _ _ │
├───────────────────────┤
│ _ _ _ │ _ _ _ │ _ _ _ │
│ _ _ _ │ _ _ _ │ _ _ _ │
│ _ _ _ │ _ _ _ │ _ _ _ │
├───────────────────────┤
│ _ _ _ │ _ _ _ │ _ _ _ │
│ _ _ _ │ _ _ _ │ _ _ _ │
│ _ _ _ │ _ _ _ │ _ _ _ │
└───────────────────────┘
```

3-phase pipeline:
- Diagonal Box Filling
- Board Completion (Backtracking)
- Cell Removal


### Step 3: Phase 1 - Fill Diagonal Boxes
```
┌─────────────────────────────────┐
│  Why Diagonals First?           │
│                                 │
│  ┌───┬───┬───┐ ┌───┬───┬───┐    │
│  │ 1 │ 2 │ 3 │ │   │   │   │    │
│  ├───┼───┼───┤ ├───┼───┼───┤    │
│  │ 4 │ 5 │ 6 │ │   │   │   │    │
│  ├───┼───┼───┤ ├───┼───┼───┤    │
│  │ 7 │ 8 │ 9 │ │   │   │   │    │
│  └───┴───┴───┘ └───┴───┴───┘    │
│                                 │
│  These boxes DON'T share:       │
│  - Rows with each other         │
│  - Columns with each other      │
│  - Box constraints              │
│                                 │
│  = NO CONFLICTS!                │
└─────────────────────────────────┘
```

Diagonal positions: (0, 0); (3,3); (6,6)

#### Fill Box (0,0) - Top-Left Diagonal:
```ocaml
(* Get shuffled numbers from pool[0] and re-shuffle *)
let nums = ShufflePool.get_shuffled pool ~index:0 ~rng
(* After re-shuffle, nums might be: [|7; 3; 9; 1; 5; 2; 4; 8; 6|] *)

(* Fill 3x3 box at (0,0) *)
```

Board after filling (0,0):
```
┌───────────────────────┐
│ 7 3 9 │ _ _ _ │ _ _ _ │
│ 1 5 2 │ _ _ _ │ _ _ _ │
│ 4 8 6 │ _ _ _ │ _ _ _ │
├───────────────────────┤
│ _ _ _ │ _ _ _ │ _ _ _ │
│ _ _ _ │ _ _ _ │ _ _ _ │
│ _ _ _ │ _ _ _ │ _ _ _ │
├───────────────────────┤
│ _ _ _ │ _ _ _ │ _ _ _ │
│ _ _ _ │ _ _ _ │ _ _ _ │
│ _ _ _ │ _ _ _ │ _ _ _ │
└───────────────────────┘
```

#### Fill Box (3,3) - Center Diagonal:
```ocaml
(* Get shuffled numbers from pool[1] and re-shuffle *)
let nums = ShufflePool.get_shuffled pool ~index:1 ~rng
(* After re-shuffle, nums might be: [|2; 8; 6; 9; 4; 1; 7; 3; 5|] *)
```

Board after filling (3,3):
```
┌───────────────────────┐
│ 7 3 9 │ _ _ _ │ _ _ _ │
│ 1 5 2 │ _ _ _ │ _ _ _ │
│ 4 8 6 │ _ _ _ │ _ _ _ │
├───────────────────────┤
│ _ _ _ │ 2 8 6 │ _ _ _ │
│ _ _ _ │ 9 4 1 │ _ _ _ │
│ _ _ _ │ 7 3 5 │ _ _ _ │
├───────────────────────┤
│ _ _ _ │ _ _ _ │ _ _ _ │
│ _ _ _ │ _ _ _ │ _ _ _ │
│ _ _ _ │ _ _ _ │ _ _ _ │
└───────────────────────┘
```

#### Fill Box (6,6) - Bottom-Right Diagonal:
```ocaml
(* Get shuffled numbers from pool[2] and re-shuffle *)
let nums = ShufflePool.get_shuffled pool ~index:2 ~rng
(* After re-shuffle, nums might be: [|3; 1; 7; 8; 5; 9; 6; 2; 4|] *)
```

Board after all diagonals filled:
```
┌───────────────────────┐
│ 7 3 9 │ _ _ _ │ _ _ _ │
│ 1 5 2 │ _ _ _ │ _ _ _ │
│ 4 8 6 │ _ _ _ │ _ _ _ │
├───────────────────────┤
│ _ _ _ │ 2 8 6 │ _ _ _ │
│ _ _ _ │ 9 4 1 │ _ _ _ │
│ _ _ _ │ 7 3 5 │ _ _ _ │
├───────────────────────┤
│ _ _ _ │ _ _ _ │ 3 1 7 │
│ _ _ _ │ _ _ _ │ 8 5 9 │
│ _ _ _ │ _ _ _ │ 6 2 4 │
└───────────────────────┘
```

### Step 3: Phase 2 - Complete Board (Backtracking)

Now the algorithm fills the remaining 54 empty cells using backtracking:

#### First Empty Cell (0,3):
```ocaml
(* Get shuffled numbers: [|4; 1; 8; 6; 2; 9; 3; 5; 7|] *)
(* Try 4: Check if valid at (0,3) *)
(* Row 0 has: [7,3,9,_,_,_,_,_,_] - 4 not in row ✓ *)
(* Col 3 has: [_,_,_,2,9,7,_,_,_] - 4 not in col ✓ *)
(* Box (0,3) is top-middle box - 4 not in box ✓ *)
(* Place 4 at (0,3) *)
```

```
┌───────────────────────┐
│ 7 3 9 │ x _ _ │ _ _ _ │
│ 1 5 2 │ _ _ _ │ _ _ _ │
│ 4 8 6 │ _ _ _ │ _ _ _ │
├───────────────────────┤
│ _ _ _ │ 2 8 6 │ _ _ _ │
│ _ _ _ │ 9 4 1 │ _ _ _ │
│ _ _ _ │ 7 3 5 │ _ _ _ │
├───────────────────────┤
│ _ _ _ │ _ _ _ │ 3 1 7 │
│ _ _ _ │ _ _ _ │ 8 5 9 │
│ _ _ _ │ _ _ _ │ 6 2 4 │
└───────────────────────┘
```

#### Continue Recursively...
After the backtracking algorithm completes, we get a full valid board:

```
Complete Board:
┌───────────────────────┐
│ 7 3 9 │ 4 1 8 │ 2 6 5 │
│ 1 5 2 │ 6 7 9 │ 3 8 4 │
│ 4 8 6 │ 2 3 5 │ 1 9 7 │
├───────────────────────┤
│ 3 1 4 │ 2 8 6 │ 5 7 9 │
│ 6 2 5 │ 9 4 1 │ 7 3 8 │
│ 8 9 7 │ 7 3 5 │ 4 1 6 │
├───────────────────────┤
│ 9 4 1 │ 5 6 2 │ 3 1 7 │
│ 2 7 3 │ 1 9 4 │ 8 5 9 │
│ 5 6 8 │ 3 5 7 │ 6 2 4 │
└───────────────────────┘
```

### Step 5: Phase 3 - Strategic Cell Removal

Target: Remove 35 cells for "Easy" difficulty.

#### Why Some Cells Are "Critical"

Let's look at a simplified 4x4 Sudoku to understand this better:

```
Original Complete 4x4 Board:
┌───────────┐
│ 1 2 │ 3 4 │
│ 3 4 │ 1 2 │
├───────────┤
│ 2 1 │ 4 3 │
│ 4 3 │ 2 1 │
└───────────┘
```

**Case 1: Removing a "Safe" Cell**
```
Remove (0,0) = 1:
┌───────────┐
│ _ 2 │ 3 4 │
│ 3 4 │ 1 2 │
├───────────┤
│ 2 1 │ 4 3 │
│ 4 3 │ 2 1 │
└───────────┘

Analysis:
- Row 0: needs [1] (only 1 is missing)
- Col 0: has [3,2,4], needs [1] (only 1 is missing)
- Box top-left: has [2,3,4], needs [1] (only 1 is missing)

Result: Only ONE way to fill (0,0) → count_solutions() = 1 ✅
```

**Case 2: Removing a "Critical" Cell** - Should not be Remove (Backtracking)
```
Remove (0,0) = 1 AND (0,1) = 2:
┌───────────┐
│ _ _ │ 3 4 │
│ 3 4 │ 1 2 │
├───────────┤
│ 2 1 │ 4 3 │
│ 4 3 │ 2 1 │
└───────────┘

Analysis:
- (0,0) could be 1: then (0,1) must be 2
- (0,0) could be 2: then (0,1) must be 1

Result: TWO ways to solve → count_solutions() = 2 ❌
```

#### Get All Filled Positions:
```ocaml
let positions = [(0,0); (0,1); (0,2); ...; (8,8)]  (* 81 positions *)
(* Shuffle randomly: *)
let shuffled = [(4,7); (2,1); (7,3); (0,5); (6,9); ...]
```

**Attempt 1: Remove (4,7) = 3**
```
Complete Board:
┌───────────────────────┐
│ 7 3 9 │ 4 1 8 │ 2 6 5 │
│ 1 5 2 │ 6 7 9 │ 3 8 4 │
│ 4 8 6 │ 2 3 5 │ 1 9 7 │
├────────────────── ────┤
│ 3 1 4 │ 2 8 6 │ 5 7 9 │
│ 6 2 5 │ 9 4 1 │ 7 3x 8 │
│ 8 9 7 │ 7 3 5 │ 4 1 6 │
├─────────────────── ───┤
│ 9 4 1 │ 5 6 2 │ 3 1 7 │
│ 2 7 3 │ 1 9 4 │ 8 5 9 │
│ 5 6 8 │ 3 5 7 │ 6 2 4 │
└───────────────────────┘
```

```
Before:                     After:
│ 6 2 5 │ 9 4 1 │ 7 3 8 │  →  │ 6 2 5 │ 9 4 1 │ 7 _ 8 │
```
Check uniqueness: `count_solutions() = 1` ✓ **Keep removal**

**Attempt 2: Remove (2,1) = 8**
```
Before:                     After:
│ 4 8 6 │ 2 3 5 │ 1 9 7 │  →  │ 4 _ 6 │ 2 3 5 │ 1 9 7 │
```
Check uniqueness: `count_solutions() = 1` ✓ **Keep removal**

**Attempt 3: Remove (7,3) = 1**
```
Before:                     After:
│ 2 7 3 │ 1 9 4 │ 8 5 9 │  →  │ 2 7 3 │ _ 9 4 │ 8 5 9 │
```
Check uniqueness: `count_solutions() = 3` ✗ **Restore value**

After Removal (simplified view):
Row 7: [2, 7, 3, _, 9, 4, 8, 5, 9]  ← Missing: 1, 6
Col 3: [4, 6, 2, 2, 9, 7, 5, _, 3]  ← Missing: 1, 8
Box:   [5,6,2 | 1,9,4 | 3,5,7]      ← Missing: 8

Now position (7,3) could be:
- 1 (satisfies row, col, box constraints)
- 8 (satisfies row, col, box constraints)  ← This creates ambiguity!

When (7,3) = 1: Rest of puzzle solves in one specific way
When (7,3) = 8: Rest of puzzle solves in two different ways
Total: 3 different solutions!

**Continue until 35 cells removed...**


### Step 6: Final Puzzle

After successful removal of 35 cells:

```
Final Easy Puzzle (35 cells removed, 46 remaining):
┌───────────────────────┐
│ _ 3 _ │ 4 _ _ │ 2 6 _ │
│ 1 _ 2 │ _ 7 9 │ _ _ 4 │
│ _ 8 6 │ 2 _ 5 │ 1 _ _ │
├───────────────────────┤
│ 3 _ _ │ _ 8 6 │ 5 7 _ │
│ 6 2 5 │ 9 _ 1 │ 7 _ 8 │
│ _ 9 7 │ 7 3 _ │ _ 1 6 │
├───────────────────────┤
│ 9 4 _ │ 5 _ 2 │ 3 1 _ │
│ _ 7 3 │ _ 9 4 │ _ 5 9 │
│ 5 _ 8 │ 3 5 _ │ 6 _ 4 │
└───────────────────────┘

Statistics:
- Difficulty: Easy
- Cells removed: 35/35 target
- Cells remaining: 46/81 (56.8% filled)
- Unique solution: ✓ Guaranteed
```


## Key Insights from This Example:

1. **ShufflePool Efficiency**: Notice how we reused pool arrays with different indices, avoiding 81+ shuffle operations
2. **Diagonal Strategy**: The three diagonal boxes were filled independently without conflicts
3. **Randomization Impact**: Even with the same pool, re-shuffling creates different boards each time
4. **Strategic Removal**: Not every removal attempt succeeds - some would break uniqueness
5. **Quality Assurance**: Every removal is validated to maintain exactly one solution

This concrete example shows why each optimization matters and how they work together to create high-quality puzzles efficiently!