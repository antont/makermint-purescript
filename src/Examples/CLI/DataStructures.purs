module Examples.CLI.DataStructures where

import Prelude

import Data.Array (filter, head)
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Console (log, logShow)

-- | Example working with arrays
arrayExamples :: Effect Unit
arrayExamples = do
  log "\n--- Array Examples ---"
  
  let numbers = [1, 2, 3, 4, 5]
  log "Original array:"
  logShow numbers
  
  let doubled = map (\x -> x * 2) numbers
  log "Doubled:"
  logShow doubled
  
  let evens = filter (\x -> x `mod` 2 == 0) numbers
  log "Even numbers:"
  logShow evens

-- | Example with Maybe (like Haskell's Maybe)
maybeExamples :: Effect Unit
maybeExamples = do
  log "\n--- Maybe Examples ---"
  
  let numbers = [1, 2, 3]
  let firstNum = head numbers
  log "First element:"
  logShow firstNum
  
  let empty = []
  let noFirst = head empty
  log "First of empty list:"
  logShow (noFirst :: Maybe Int)
  
  let defaultValue = fromMaybe 0 noFirst
  log "With default value:"
  logShow defaultValue

-- | Custom data types
data Color = Red | Green | Blue | RGB Int Int Int

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"
  show (RGB r g b) = "RGB(" <> show r <> ", " <> show g <> ", " <> show b <> ")"

colorExamples :: Effect Unit
colorExamples = do
  log "\n--- Custom Type Examples ---"
  logShow Red
  logShow (RGB 255 128 0)

-- | Run all data structure examples
runExamples :: Effect Unit
runExamples = do
  arrayExamples
  maybeExamples
  colorExamples

-- | Main function to run as standalone example
main :: Effect Unit
main = runExamples

