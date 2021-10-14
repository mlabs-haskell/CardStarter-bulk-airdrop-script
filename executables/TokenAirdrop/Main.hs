module Main where

import CommandLine qualified
import TokenAirdrop qualified
import Prelude

main :: IO ()
main = do
  config <- CommandLine.execCommand
  results <- TokenAirdrop.tokenAirdrop config
  print results
