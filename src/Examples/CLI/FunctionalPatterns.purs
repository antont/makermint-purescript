module Examples.CLI.FunctionalPatterns where

import Prelude

import Data.Array (foldl)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log, logShow)

-- | Map and Functor examples
functorExamples :: Effect Unit
functorExamples = do
  log "\n--- Functor Examples ---"
  
  let justFive = Just 5
  let mapped = map (_ * 2) justFive
  log "Mapping over Maybe:"
  logShow mapped
  
  let nothing = Nothing :: Maybe Int
  let mappedNothing = map (_ * 2) nothing
  logShow mappedNothing

-- | Applicative examples
applicativeExamples :: Effect Unit
applicativeExamples = do
  log "\n--- Applicative Examples ---"
  
  let addTwo = Just (\x y -> x + y)
  let result = addTwo <*> Just 3 <*> Just 4
  log "Applicative computation:"
  logShow result

-- | Monad examples (flatMap / bind)
monadExamples :: Effect Unit
monadExamples = do
  log "\n--- Monad Examples ---"
  
  let result = do
        x <- Just 5
        y <- Just 3
        pure (x + y)
  log "Monadic computation:"
  logShow result
  
  let withNothing = do
        x <- Just 5
        y <- Nothing
        pure (x + y)
  log "With Nothing:"
  logShow (withNothing :: Maybe Int)

-- | Either for error handling
type Error = String

safeDivide :: Int -> Int -> Either Error Int
safeDivide _ 0 = Left "Division by zero!"
safeDivide x y = Right (x / y)

eitherExamples :: Effect Unit
eitherExamples = do
  log "\n--- Either (Error Handling) Examples ---"
  
  let good = safeDivide 10 2
  log "10 / 2:"
  logShow good
  
  let bad = safeDivide 10 0
  log "10 / 0:"
  logShow bad

-- | Higher-order functions
sumArray :: Array Int -> Int
sumArray = foldl (+) 0

productArray :: Array Int -> Int
productArray = foldl (*) 1

higherOrderExamples :: Effect Unit
higherOrderExamples = do
  log "\n--- Higher-Order Function Examples ---"
  
  let numbers = [1, 2, 3, 4, 5]
  log "Sum of array:"
  logShow (sumArray numbers)
  
  log "Product of array:"
  logShow (productArray numbers)

-- | Run all examples
runExamples :: Effect Unit
runExamples = do
  functorExamples
  applicativeExamples
  monadExamples
  eitherExamples
  higherOrderExamples

-- | Main function to run as standalone example
main :: Effect Unit
main = runExamples

