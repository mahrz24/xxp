{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import System.Console.CmdArgs
import System.Environment ( getArgs, withArgs )

import System.Cmd
import System.Exit
import System.Directory
import System.FilePath

import Control.Monad

-- import MonadUtils
-- import GHC
-- import GHC.Paths ( libdir )
-- import DynFlags ( defaultFatalMessager, defaultFlushOut )

_PROGRAM_NAME = "xxp"
_PROGRAM_VERSION = "0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "Haskell based framework for running simulations and numerical\
\ experiments in a modular fashion and collects logged data."
_COPYRIGHT = "(C) 2012 Malte Harder"

data Commands = Run { experiment::String
                    , tag::String
                    , custom_config::String
                    , force_config::String
                    }
              | Clean
              deriving (Data, Typeable, Show, Eq)

commandRun = Run { experiment = def &= argPos 0 &= typ "EXPERIMENT"
                 , tag = def &= typ "LABEL"
                 , custom_config = def &= typ "JSON"
                 , force_config = def &= typFile
                 }
             &= details [ "Examples:", "xxp run test1"]
      
commandClean = Clean

commands :: Mode (CmdArgs Commands)
commands = cmdArgsMode $ modes [commandRun, commandClean]
  &= verbosityArgs [explicit, name "Verbose", name "V"] []
  &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
  &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
  &= help _PROGRAM_ABOUT
  &= helpArg [explicit, name "help", name "h"]
  &= program _PROGRAM_NAME

main = do
  args <- getArgs
  opts <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun commands
  optionHandler opts

optionHandler :: Commands -> IO ()
optionHandler opts@Run{..} = exec opts

exec :: Commands -> IO ()
exec opts@Run{..} = run experiment

run :: String -> IO ()
run exp = compileFile ("xp_" ++ exp ++ ".hs" )

compileFile :: String -> IO ()
compileFile f = do
  let exp = (dropExtension f)
  pwd <- getCurrentDirectory
  makeBuild <- getDirectoryContents pwd >>= return . notElem "build"
  when makeBuild (createDirectory (pwd </> "build"))
  exitCode <- rawSystem "ghc" [ "--make", f
                              , "-o", "build" </> exp
                              , "-hidir", "build"
                              , "-odir", "build"
                              , "-osuf", exp ++ ".o"
                              , "-hisuf", exp ++ ".hi"]
  case exitCode of 
    ExitSuccess -> return ()
    ExitFailure i -> putStrLn $ "ghc returned with error code " ++ (show i)
  -- defaultErrorHandler defaultFatalMessager defaultFlushOut  $ do
  --   runGhc (Just libdir) $ do
  --     dflags <- getSessionDynFlags
  --     setSessionDynFlags dflags
  --     target <- guessTarget f Nothing
  --     setTargets [target]
  --     r <- load LoadAllTargets
  --     case r of
  --       Failed -> error "Compilation failed"
  --       Succeeded -> liftIO $ putStrLn "Compilation successfull"
