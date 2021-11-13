module Main where

import CommandLine qualified
import Data.Text (unpack)
import TokenAirdrop qualified
import Prelude

main :: IO ()
main = do
  config <- CommandLine.execCommand
  result <- TokenAirdrop.tokenAirdrop config
  case result of
    Left err -> putStrLn $ unpack err
    Right _ -> putStrLn "Token drop complete!"
