module Examples.CLI.HelloWorld where

import Prelude

import Effect (Effect)
import Effect.Console (log)

-- | A simple Hello World example
helloWorld :: Effect Unit
helloWorld = do
  log "Hello, World!"
  log "This is PureScript!"

-- | Greet someone by name
greet :: String -> Effect Unit
greet name = do
  log ("Hello, " <> name <> "!")

-- | Example of pure function composition
exclaim :: String -> String
exclaim s = s <> "!"

shout :: String -> String
shout s = s <> "!!!"

-- | Composing pure functions
enthusiasticGreeting :: String -> String
enthusiasticGreeting = exclaim <<< shout <<< (_ <> " is awesome")

-- | Main function to run as standalone example
main :: Effect Unit
main = do
  log "=== Hello World Example ==="
  helloWorld
  greet "CLI User"
  log ("Composition example: " <> enthusiasticGreeting "Functional Programming")

