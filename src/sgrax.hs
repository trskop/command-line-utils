-- |
-- Module:       Main
-- Description:  Concatenate command line argument in various ways.
-- Copyright:    (c) 2011, 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
--
-- Concatenate command line argument in various ways.
module Main (main)
    where

import System.Environment (getArgs, getProgName)
import System.IO (Handle, hPutStr, stderr, stdout)


delimChar :: Char -> [String] -> String
delimChar _ []     = ""
delimChar _ [w]    = w
delimChar d (w:ws) = w ++ d : delimChar d ws

delimStr :: String -> [String] -> String
delimStr _   []     = ""
delimStr _   [w]    = w
delimStr [d] ws     = delimChar d ws
delimStr d   (w:ws) = w ++ d ++ delimStr d ws

data Cfg = Cfg
    { cfgDelimiter :: String
    , cfgDelimiterAtTheEnd :: Bool
    , cfgDelimiterAtTheBeginning :: Bool
    , cfgHeader :: String
    , cfgFooter :: String
    , cfgOutputHandle :: Handle
    }

emptyCfg :: Cfg
emptyCfg = Cfg
    { cfgDelimiter = ""
    , cfgDelimiterAtTheEnd = False
    , cfgDelimiterAtTheBeginning = False
    , cfgHeader = ""
    , cfgFooter = ""
    , cfgOutputHandle = stdout
    }

defaultCfg :: Cfg
defaultCfg = emptyCfg
    { cfgDelimiter = "\n"
    , cfgDelimiterAtTheEnd = True
    }

parseOptions :: (Cfg, [String], [String]) -> (Cfg, [String], [String])
parseOptions x = case x of
    (cfg, arg:args, strs) -> parseOptions
        $ case arg of
            "--" -> (cfg, [], strs ++ args)
            "-0" -> (nulSeparatedCfg cfg, args, strs)
            "-c" -> (csvCfg cfg, args, strs)
            "-e" -> (setHandle cfg stderr, args, strs)
            "-n" -> (setHandle defaultCfg $ cfgOutputHandle cfg, args, strs)
            "-o" -> (setHandle cfg stdout, args, strs)
            "-t" -> (tabSeparatedCfg cfg, args, strs)
            s -> (cfg, args, strs ++ [s])
    x' -> x'
  where
    nulSeparatedCfg cfg = emptyCfg
        { cfgDelimiter = "\0"
        , cfgOutputHandle = cfgOutputHandle cfg
        }
    tabSeparatedCfg cfg = emptyCfg
        { cfgDelimiter = "\t"
        , cfgFooter = "\n"
        , cfgOutputHandle = cfgOutputHandle cfg
        }
    csvCfg cfg = emptyCfg
        { cfgHeader = "\""
        , cfgDelimiter = "\",\""
        , cfgFooter = "\"\n"
        , cfgOutputHandle = cfgOutputHandle cfg
        }
    setHandle cfg h = cfg{cfgOutputHandle = h}

printHelp :: IO ()
printHelp = do
    progName <- getProgName
    putStr $ unlines
        [ "Usage:"
        , ' ' : ' ' : (progName ++ " [-0cenot] [--] STR [...]")
        , ""
        , "Options:"
        , "  -0    Print STRs by delimiting them with '\\0' character, similar\
            \ to GNU find's '-print0' option."
        , "  -c    Print STRs in CSV format, all options are quoted."
        , "  -e    Use stderr for output."
        , "  -n    Print STRs by delimiting them with '\\n' character\
            \ (default)."
        , "  -o    Use stdout for output (default)."
        , "  -t    Print STRs by delimiting them with '\\t' character."
        ]

main :: IO ()
main = do
    args <- getArgs
    let (cfg, [], strs) = parseOptions (defaultCfg, args, [])
        putS = hPutStr (cfgOutputHandle cfg)
        delim = cfgDelimiter cfg

    if null strs
        then printHelp
        else do
            putS $ cfgHeader cfg
            putS . delimStr delim
                $ if cfgDelimiterAtTheBeginning cfg
                    then "" : strs
                    else strs
            if cfgDelimiterAtTheEnd cfg
                then putS delim
                else return ()
            putS $ cfgFooter cfg
