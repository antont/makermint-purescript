# Web Applications

This folder contains web application examples.

## Todo App

A fully functional Todo application built with Halogen (PureScript's UI framework).

**Features:**
- ✅ Add new todos
- ✅ Mark todos as complete
- ✅ Delete todos
- ✅ Count active items
- ✅ Beautiful, responsive UI

**Run it:**
```bash
npm run app:todo
```

This will build the app and start a local server at http://localhost:9090

**Build only:**
```bash
npm run app:todo:build
```

## Architecture

The Todo app demonstrates:
- **Halogen Component**: Type-safe UI component with state management
- **Algebraic Data Types**: `Todo` type with id, text, and completed status
- **Actions**: `AddTodo`, `ToggleTodo`, `DeleteTodo`, `UpdateInput`
- **Pure Functions**: Business logic separated from effects
- **Web bundling**: esbuild bundles PureScript to optimized JavaScript

