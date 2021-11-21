module Main where

import CommandLine qualified
import Data.Text (unpack)
import TokenAirdrop qualified
import Prelude

main :: IO ()
main = do
  config <- CommandLine.execCommand
  result <- TokenAirdrop.tokenAirdrop config
  putStrLn $ either unpack (const "Token drop complete!") result
