module Main where

import TokenAirdrop qualified
import Prelude


main :: IO ()
main = do
  results <- TokenAirdrop.tokenAirdrop TokenAirdrop.defaultConfig
  print results
