# PureScript Example Apps

This project contains multiple example applications to test LLM capabilities with PureScript.

## Setup

1. Install dependencies:
```bash
npm install
```

2. Install PureScript packages:
```bash
npx spago install
```

## Building and Running

Build the project:
```bash
npm run build
```

Run all CLI examples:
```bash
npm start
# or
npm run cli
```

Run individual CLI examples:
```bash
npm run cli:hello            # HelloWorld examples
npm run cli:data             # DataStructures examples
npm run cli:functional       # FunctionalPatterns examples
```

Run tests:
```bash
npm test
```

Start a REPL:
```bash
npm run repl
```

## Project Structure

```
makermint-purescript/
├── src/
│   ├── Main.purs                        # Main entry point
│   └── Examples/
│       ├── CLI/                         # Command-line examples
│       │   ├── HelloWorld.purs          # Basic examples
│       │   ├── DataStructures.purs      # Arrays, Maybe, custom types
│       │   └── FunctionalPatterns.purs  # Functors, Monads, Either
│       ├── Apps/                        # Web application examples
│       └── Games/                       # Game examples
├── test/
│   └── Main.purs                        # Test suite
├── spago.yaml                           # Spago configuration
└── package.json                         # npm configuration
```

## Example Categories

### CLI Examples
Command-line examples demonstrating core PureScript concepts:
- **HelloWorld**: Basic console output and function composition
- **DataStructures**: Arrays, Maybe, and custom algebraic data types
- **FunctionalPatterns**: Functors, Applicatives, Monads, and Either for error handling

### Apps
Web applications with interactive UIs:
- **TodoApp**: A fully functional todo list application with Halogen
  - Features: Add, complete, delete todos with a beautiful UI
  - Run: `npm run app:todo`

### Games
Interactive canvas-based games:
- **TicTacToe**: Classic tic-tac-toe with canvas rendering
  - Features: Canvas graphics, win detection, animations
  - Run: `npm run game:tictactoe`

## Running Examples

### CLI Examples
```bash
npm run cli                  # Run all CLI examples
npm run cli:hello           # HelloWorld example
npm run cli:data            # DataStructures example
npm run cli:functional      # FunctionalPatterns example
```

### Web Apps
```bash
npm run app:todo            # Run Todo app (builds & opens in browser at :9090)
npm run app:todo:build      # Just build the Todo app
```

### Games
```bash
npm run game:tictactoe      # Run Tic-Tac-Toe game (builds & opens in browser at :9091)
npm run game:tictactoe:build # Just build the Tic-Tac-Toe game
```

### In the REPL
```purescript
import Examples.CLI.HelloWorld
helloWorld
greet "PureScript"

import Examples.CLI.DataStructures
runExamples

import Examples.CLI.FunctionalPatterns
runExamples
```

## Adding New Examples

### CLI Examples
1. Create a new module in `src/Examples/CLI/`
2. Follow the pattern of existing examples
3. Export functions that can be called from REPL or Main

### Apps
1. Create a new module in `src/Examples/Apps/`
2. Set up web UI with Halogen, React Basic, or other framework
3. Add run script to `package.json`

### Games
1. Create a new module in `src/Examples/Games/`
2. Set up game engine and canvas/WebGL rendering
3. Add run script to `package.json`

## Resources

- [PureScript Documentation](https://github.com/purescript/documentation)
- [PureScript by Example](https://book.purescript.org/)
- [Pursuit (Package Documentation)](https://pursuit.purescript.org/)

