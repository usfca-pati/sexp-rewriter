{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Rewriting
import Parser
import Data.List (intercalate)
import Options.Applicative
import qualified Text.Parsec as P
import System.Exit (die)

-- Argument handling
data CLIOptions =
  CLIOptions { ruleFile :: FilePath
             , inputFile :: FilePath
             , outputFile :: FilePath
             }

-- The arguments to CLIOptions must appear in the same order as the declaration
-- order (this parser treats CLIOptions as a list).
cliArgParser = CLIOptions <$> rules <*> input <*> output
  where rules = strOption
                ( short 'r'
                <> long "rules"
                <> help "The file containing the rewrite rules."
                <> metavar "FILE")
        input = strOption
                ( short 'i'
                <> long "input"
                <> help "The file to read the input s-expression from."
                <> metavar "FILE")
        output = strOption
                 ( short 'o'
                 <> long "output"
                 <> help "The file to write the output s-expression to."
                 <> metavar "FILE")

opts = info (cliArgParser <**> helper)
       (fullDesc <>
         header "sexp-rewriter")

type ParseFn a = FilePath -> String -> Either P.ParseError a

-- | Read and parse a value using given parser.  Exit the program on failure.
readFromFile :: (ParseFn a) -> FilePath -> IO a
readFromFile parser path = do
  a <- parser path <$> readFile path
  case a of
    Left err -> die $ "Parse error: " ++ show err
    Right a -> return a

applyAllRules rules sexp = foldl applyTillFixpoint sexp rules
  where
    applyTillFixpoint s rule = let s' = applyRule rule s in
      if s == s' then s' else applyTillFixpoint s' rule

main :: IO ()
main = do
  cliOptions <- execParser opts
  rules <- readFromFile parseRules cliOptions.ruleFile
  sexps <- readFromFile parseSExps cliOptions.inputFile
  writeFile cliOptions.outputFile $ intercalate "\n" $
    map (show . applyAllRules rules) sexps
