module Main where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect (Effect)
import Effect.Console (log)
import Prim.Row as Row

data Param (name :: Symbol) ty = Param

data Thing (params :: # Type) = Thing String

mkLiteral :: String -> Thing ()
mkLiteral = Thing

mkParam :: forall name ty r
   . Row.Cons name ty () r
  => IsSymbol name
  => Param name ty -> Thing r
mkParam _ = Thing name
  where
    nameS = SProxy :: SProxy name
    name = "$" <> reflectSymbol nameS

joinThings :: forall r1 r2 r
   . Row.Union r1 r2 r
  => Thing r1 -> Thing r2 -> Thing r
joinThings (Thing s1) (Thing s2) = Thing $ s1 <> " " <> s2

infixl 1 joinThings as <<>>

runThing :: forall r
   . Show { | r }
  => Thing r -> { | r } -> Effect Unit
runThing (Thing query) params = do
  log "Running query:"
  log query
  log "with params:"
  log $ show params

main :: Effect Unit
main = do
  runThing thing { apple: "i am apple" }
  -- select * from things where id = $apple
  -- with params:
  -- { apple: "i am apple" }
  where
    -- optional, inferred type annotation:
    -- thing :: Thing ( apple :: String )
    thing = lit <<>> apple

    -- optional, inferred type annotation:
    -- lit :: Thing ()
    lit = mkLiteral "select * from things where id ="

    -- optional, inferred type annotation:
    -- apple :: Thing ( apple :: String )
    apple = mkParam (Param :: Param "apple" String)

