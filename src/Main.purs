module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Examples.CLI.HelloWorld as HelloWorld
import Examples.CLI.DataStructures as DataStructures
import Examples.CLI.FunctionalPatterns as FunctionalPatterns

main :: Effect Unit
main = do
  log "🚀 Welcome to PureScript Examples!"
  log "================================================"
  log ""
  
  -- HelloWorld examples
  log "=== HELLO WORLD EXAMPLES ==="
  HelloWorld.helloWorld
  HelloWorld.greet "PureScript"
  log ("Enthusiastic: " <> HelloWorld.enthusiasticGreeting "PureScript")
  
  -- DataStructures examples
  log "\n=== DATA STRUCTURES EXAMPLES ==="
  DataStructures.runExamples
  
  -- FunctionalPatterns examples
  log "\n=== FUNCTIONAL PATTERNS EXAMPLES ==="
  FunctionalPatterns.runExamples
  
  log "\n================================================"
  log "✅ All examples completed!"

