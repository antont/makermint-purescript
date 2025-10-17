# Games

This folder contains game examples built with canvas graphics.

## Tic-Tac-Toe

A classic tic-tac-toe game using HTML5 Canvas and Halogen.

**Features:**
- ✅ Canvas-based rendering with X's and O's
- ✅ Click to place pieces
- ✅ Win detection (rows, columns, diagonals)
- ✅ Draw detection
- ✅ Reset functionality
- ✅ Beautiful animations and colors

**Run it:**
```bash
npm run game:tictactoe
```

This will build the game and start a local server at http://localhost:9091

**Build only:**
```bash
npm run game:tictactoe:build
```

## Architecture

The Tic-Tac-Toe game demonstrates:
- **Canvas Graphics**: Direct 2D drawing with `purescript-canvas`
- **Halogen Component**: Type-safe UI with state management
- **Event Handling**: Mouse clicks mapped to canvas coordinates
- **Game Logic**: Win detection, turn management, board state
- **Algebraic Data Types**: `Player` type (X | O), board representation
- **Pure Functions**: All game logic is pure, effects isolated

