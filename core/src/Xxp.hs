{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import System.Console.CmdArgs
import System.Environment ( getArgs, withArgs )

import System.Cmd
import System.Exit
import System.Directory
import System.FilePath

import Control.Monad

import System.Log.Logger
import System.Log.Handler.Simple
import System.IO

import qualified HsShellScript as SH

import Data.Maybe

_PROGRAM_NAME = "xxp"
_PROGRAM_VERSION = "0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "Haskell based framework for running simulations and numerical\
\ experiments in a modular fashion and collects logged data."
_COPYRIGHT = "(C) 2012 Malte Harder"

data Commands = Run { experiment::String
                    , tag::String
                    , customConfig::String
                    , forceConfig::String
                    , debugMode::Bool
                    , gdb::Bool
                    }
              | Clean
              | Gdb { binary::String }
              deriving (Data, Typeable, Show, Eq)

data CompilationResult = CompilationSuccess 
                       | CompilationFailure 
                       deriving (Eq)

commandGdb = Gdb { binary = def &= argPos 0 &= typ "BINARY"
                 }
             &= details [ "Examples:", "xxp gdb test1"]

commandRun = Run { experiment = def &= argPos 0 &= typ "EXPERIMENT"
                 , tag = def &= typ "LABEL"
                 , customConfig = def &= typ "JSON"
                 , forceConfig = def &= typFile
                 , debugMode = def &= help "Run in debug mode"
                 , gdb = def &= help "Run the binary using gdbserver"
                 }
             &= details [ "Examples:", "xxp run test1"]
      
commandClean = Clean

commands :: Mode (CmdArgs Commands)
commands = cmdArgsMode $ modes [commandRun, commandGdb, commandClean]
  &= verbosityArgs [explicit, name "Verbose", name "V"] []
  &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
  &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
  &= help _PROGRAM_ABOUT
  &= helpArg [explicit, name "help", name "h"]
  &= program _PROGRAM_NAME

main = do
  updateGlobalLogger rootLoggerName (setLevel NOTICE)
  args <- getArgs
  opts <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun commands
  whenLoud $ do 
    verboseHandler <- verboseStreamHandler stderr DEBUG
    updateGlobalLogger rootLoggerName 
      (setLevel DEBUG . setHandlers [verboseHandler])
  debugM "xxp.log" (_PROGRAM_INFO ++ " started")
  optionHandler opts
  exitSuccess

optionHandler :: Commands -> IO ()
optionHandler opts = exec opts

exec :: Commands -> IO ()                      
exec opts@Gdb{..} = SH.execp "gdb" [ "-q"
                                   , "-ex"
                                   , "target remote localhost:2486"
                                   , "build" </> binary]
exec opts@Run{..} = run experiment tag customConfig
                      forceConfig debugMode gdb

run :: String -> String -> String -> String -> Bool -> Bool -> IO ()
run exp t cc fc dbg gdb = do 
  let binary = "xp_" ++ exp
      source = binary ++ ".hs"
  debugM "xxp.log" ("Preparing experiment: " ++ exp)
  result <- compileFile source
  when (result == CompilationSuccess) $ do
    noticeM "xxp.log" $ "Running experiment: " ++ exp
    pwd <- getCurrentDirectory
    -- Pass the logging level for the console
    lvl <- liftM ((fromMaybe NOTICE) . getLevel) getRootLogger
    exitCode <- rawSystem (pwd </> "build" </> binary) [ (show lvl)
                                                       , t
                                                       , cc
                                                       , fc
                                                       , (show dbg)
                                                       , (show gdb)
                                                       ]
    case exitCode of 
      ExitSuccess -> noticeM "xxp.log" $ 
                     "Experiment finished: " ++ exp
      ExitFailure i -> errorM "xxp.log" $ 
                       "Experiment failed with error code: " ++ (show i)
      


compileFile :: String -> IO (CompilationResult)
compileFile f = do
  let exp = (dropExtension f)
  pwd <- getCurrentDirectory
  -- TODO Check for file first
  makeBuild <- getDirectoryContents pwd >>= return . notElem "build"
  when makeBuild (do debugM "xxp.log" "Create build directory"
                     createDirectory (pwd </> "build"))
  -- Always create the run directory
  createDirectoryIfMissing True (pwd </> "run")
  debugM "xxp.log" ("Compiling experiment source " ++ f)
  exitCode <- rawSystem "ghc" [ "--make", f
                              , "-o", "build" </> exp
                              , "-hidir", "build"
                              , "-odir", "build"
                              , "-osuf", exp ++ ".o"
                              , "-hisuf", exp ++ ".hi"]
  case exitCode of 
    ExitSuccess -> return CompilationSuccess
    ExitFailure i -> do
      errorM "xxp.log" $ "GHC returned with error code " ++ (show i)
      return CompilationFailure

