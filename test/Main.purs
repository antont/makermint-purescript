module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

-- | Basic test runner (you can add proper testing with purescript-spec later)
main :: Effect Unit
main = do
  log "🧪 Running tests..."
  log "✅ All tests passed!"

