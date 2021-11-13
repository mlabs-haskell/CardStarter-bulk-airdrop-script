module Main where

import CommandLine qualified
import TokenAirdrop qualified
import Prelude

main :: IO ()
main = do
  config <- CommandLine.execCommand
  result <- TokenAirdrop.tokenAirdrop config
  case result of
    Left err -> putStrLn err
    Right _ -> putStrLn "Token drop complete!"
