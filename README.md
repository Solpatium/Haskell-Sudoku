## Haskel sudoku
----
Project for functional programming classes.
### Quick start
Before you preceed make sure you have Stack installed and configured.
```bash
git clone https://github.com/Solpatium/Haskell-Sudoku.git
cd Haskell-Sudoku
stack build
stack exec sudoku-exe # This launches all benchmarks and can take a few minutes :)
```
**Warning: ** benchmarks might not work on Windows, there issues with file coding. Anything else should work on Windows.
#### Generating documentation:
```bash
stack haddock
```
Please note that documentation is already generated - it is in the `doc` directory.
#### Launching tests:
```bash
stack test
```

### About
Sudoku is represented by SudokuBoard, SudokuSquare and SudokuValue. We implemented different types implementing those classes:
* Boards
  * ListBoard
  * VectorBoard
* Squares:
  * MaybeSquare
  * SimpleSquare
* Values:
  * IntValue
  * DataBalue
  * CharValue
  * IntegerValue

In order to create a sudoku use function `readBoard`. It returns board wrapped in `Maybe`, `Nothing` is returned in case of invalid board.
```haskell
readBoard string :: Board ( Square Value )
```
replacing Board, Square and Value with wanted implementation (listed above).
