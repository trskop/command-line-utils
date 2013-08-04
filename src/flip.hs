-- |
-- Module:       Main
-- Description:  Change the order of command line arguments.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
--
-- Change the order of command line arguments.
module Main (main)
    where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (Handle, hPutStr, stderr, stdout)

import System.Posix.Process (executeFile)


printHelp :: Handle -> IO ()
printHelp handle = do
    progName <- getProgName
    hPutStr handle $ unlines
        [ "Usage:"
        , "  " ++ progName ++ " [--] COMMAND FIRST_ARG ARGS..."
        , "  " ++ progName ++ " {-h|-?|-help|--help}"
        ]

main' :: [String] -> IO ()
main' (cmd : arg : args) = executeFile cmd True (args ++ [arg]) Nothing
main' _                  = printHelp stderr >> exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> printHelp stderr >> exitFailure
        arg@('-' : _) : _
          | isHelpArg arg -> printHelp stdout
          | arg == "--" -> main' $ tail args
          | otherwise -> printHelp stderr >> exitFailure
        _ -> main' args
  where
    isHelpArg ('-' : arg) = case arg of
        "?" -> True
        "h" -> True
        "help" -> True
        "-help" -> True
        _ -> False
    isHelpArg _ = False
