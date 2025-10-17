# Tic-Tac-Toe Game - PureScript with Canvas

A canvas-based tic-tac-toe game demonstrating PureScript's Canvas API integration and Halogen component model.

## What Makes This Interesting

**1. Canvas Graphics:**
```purescript
drawX :: Context2D -> Number -> Number -> Effect Unit
drawO :: Context2D -> Number -> Number -> Effect Unit
```
Direct 2D drawing with the HTML5 Canvas API - X's and O's drawn programmatically.

**2. Mouse Coordinate Handling:**
```purescript
canvasX = mouseX - canvasOffsetX
canvasY = mouseY - canvasOffsetY
cellIndex = getCellIndex canvasX canvasY
```
Converting viewport mouse clicks to canvas-relative coordinates and grid positions.

**3. Programmatic Win Detection:**
```purescript
checkWinner board = checkAllLines (allRows <> allColumns <> allDiagonals)
  where
    allRows = map makeRow (range 0 2)
      where makeRow r = map (\c -> r * 3 + c) (range 0 2)
```
No hardcoded patterns - rows, columns, and diagonals generated mathematically.

**4. Clean Functional Pattern:**
```purescript
checkAllLines lines = findMap checkLine lines
```
Using `findMap` idiomatically - find the first winning line, short-circuit on success.

## Architecture

**State Management:**
- Board: `Array (Maybe Player)` - 9 cells, each `Nothing` or `Just X/O`
- Game state: current player, winner, game over flag
- Canvas context stored in component state for drawing

**Actions:**
- `HandleClick` - converts mouse to grid position, updates board
- `HandleKeyPress` - 'R' key resets the game
- `Reset` - clears board back to initial state

**Pure Functions:**
- `checkWinner` - examines board, returns `Maybe Player`
- `getCellIndex` - converts coordinates to cell index
- All drawing is in `Effect`, game logic is pure

## Compare to Typical Implementations

**JavaScript Canvas Game:**
- Manual DOM manipulation
- Global state or messy closures
- Runtime errors from typos
- Unclear separation of concerns

**PureScript:**
- ~300 lines including comments
- Type-checked at compile time
- Clear separation: pure logic vs effects
- Canvas code isolated in `Effect`
- If it compiles, coordinates work, clicks register correctly

## Key Techniques

**Halogen + Canvas:**
Halogen manages component lifecycle, Canvas API does the rendering. Best of both worlds.

**Effect Isolation:**
```purescript
drawBoard :: forall output m. MonadEffect m => 
  H.HalogenM State Action () output m Unit
```
Drawing is clearly effectful, game logic stays pure.

**Type Safety:**
The compiler ensures we handle all player states, all actions, never access invalid array indices (returns `Maybe`).

The result: a working game with no runtime surprises! 🎮

