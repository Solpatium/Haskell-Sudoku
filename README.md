### Haskel sudoku
----
Project for functional programming classes.

#### About
Sudoku is represented by SudokuBoard, SudokuSquare and SudokuValue. We implemented different types implementing those classes:
- Boards
-- ListBoard
-- VectorBoard
- Squares:
-- MaybeSquare
-- SimpleSquare
- Values:
-- IntValue
-- DataBalue
-- CharValue
-- IntegerValue

In order to create a sudoku use function `readBoard`:
```haskell
readBoard string :: Board ( Square Value )
```
replacing Board, Square and Value with wanted implementation (listed above).
