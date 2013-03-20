{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import qualified XXP.PostProcessing.Logs as Logs

import System.Console.CmdArgs
import System.Environment ( getArgs, withArgs )

import System.Cmd
import System.Exit
import System.Directory
import System.FilePath

import Control.Monad

import Data.Maybe
import Data.List
import Data.Time.Clock
import Data.Time.Format

import System.Locale
import System.Log.Logger
import System.Log.Handler.Simple
import System.IO

import qualified HsShellScript as SH

import TermSize

import qualified XXP.Experiment as XP

_PROGRAM_NAME = "xxp"
_PROGRAM_VERSION = "0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "Haskell based framework for running simulations and numerical\
\ experiments in a modular fashion and collects logged data."
_COPYRIGHT = "(C) 2012 Malte Harder"

data Column = Static String
            | Flexible String

data Commands = Run { experiment::String
                    , tag::String
                    , customConfig::String
                    , forceConfig::String
                    , debugMode::Bool
                    , gdb::Bool
                    }
              | Gdb { binary::String }
              | Rm { match :: Maybe String
                   , all :: Bool
                   , running :: Bool
                   , marked :: Bool
                   , last :: Bool
                   }
              | Clean
              | Mark { match :: Maybe String
                     , latest :: Maybe Int
                     }
              | Unmark
              | List
              deriving (Data, Typeable, Show, Eq)

data CompilationResult = CompilationSuccess 
                       | CompilationFailure 
                       deriving (Eq)

commandRun = Run { experiment = def &= argPos 0 &= typ "<experiment>"
                 , tag = def &= typ "<label>"
                 , customConfig = def &= typ "<json>"
                 , forceConfig = def &= typ "<filename>"
                 , debugMode = def &= help "Run in debug mode"
                 , gdb = def &= help "Run the binary using gdbserver"
                 }
             &= details [ "Examples:", "xxp run test1"]

commandGdb = Gdb { binary = def &= argPos 0 &= typ "<binary>"
                 }
             &= details [ "Examples:", "xxp gdb test1"]

commandClean = Clean

commandRemove = Rm { match = def &= args &= typ "<pattern>"
                   , all = def &= help "Remove all logs not only failed runs"
                   , running = def &= help "Also logs of running experiments"
                   , marked = def &= help "Delete all marked logs"
                   , last = def &= help "Delete last log"
                   }
                &= details [ "Examples:", "xxp rm tag1"]

commandMark = Mark { match = def &= args &= typ "<pattern>"
                   , latest = def &= help "Mark the latest n logs"}
             &= details [ "Examples:", "xxp mark 201304"]

commandUnmark = Unmark

commandList = List

commands :: Mode (CmdArgs Commands)
commands = cmdArgsMode $ modes [ commandRun
                               , commandGdb
                               , commandClean
                               , commandRemove
                               , commandMark
                               , commandUnmark
                               , commandList
                               ]
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

exec opts@Run{..} = do
  let binary = "xp_" ++ experiment
      source = binary ++ ".hs"
  debugM "xxp.log" ("Preparing experiment: " ++ experiment)
  result <- compileFile source
  when (result == CompilationSuccess) $ do
    noticeM "xxp.log" $ "Running experiment: " ++ experiment
    pwd <- getCurrentDirectory
    -- Pass the logging level for the console
    lvl <- liftM ((fromMaybe NOTICE) . getLevel) getRootLogger
    exitCode <- rawSystem (pwd </> "build" </> binary) [ (show lvl)
                                                       , tag
                                                       , customConfig
                                                       , forceConfig
                                                       , (show debugMode)
                                                       , (show gdb)
                                                       ]
    case exitCode of 
      ExitSuccess -> noticeM "xxp.log" $ 
                     "Experiment finished: " ++ experiment
      ExitFailure i -> errorM "xxp.log" $ 
                       "Experiment failed with error code: " ++ (show i)

exec opts@Gdb{..} = SH.execp "gdb" [ "-q"
                                   , "-ex"
                                   , "target remote localhost:2486"
                                   , "build" </> binary]

exec opts@Clean{..} = do
  removeDirectoryRecursive "run"
  removeDirectoryRecursive "build"

exec opts@Rm{..} = do
  logs <- Logs.loadAllLogs
  let filtered = if last then
                   (\x -> [x]) . head . Logs.sortLogs $
                     filterC (not running) (not . Logs.running) logs
                   else filterC (not running) (not . Logs.running)
                        (if marked then
                           filter Logs.marked logs
                         else 
                           filterC (not all) (not . Logs.success) logs)
  let matched = case match of
        Just p -> filter (Logs.anyMatches p) filtered
        Nothing -> filtered
  mapM_ Logs.removeLog matched

exec opts@Mark{..} = do
  logs <- Logs.loadAllLogs
  let matched = case match of
        Just p -> filter (Logs.anyMatches p) logs
        Nothing -> logs
      filtered = case latest of
        Just n -> take n $ Logs.sortLogs matched
        Nothing -> matched
  mapM_ Logs.markLog filtered

exec opts@Unmark{..} = do
  logs <- Logs.loadAllLogs
  mapM_ Logs.unmarkLog logs

exec opts@List{..} = do
  logs <- liftM Logs.sortLogs $ Logs.loadAllLogs
  (_,termWidth) <- getTermSize
  putStr $ renderTable termWidth headers (map toRow logs)
    where headers = [ "Name"
                    , "UUID"
                    , "Tag"
                    , "M"
                    , "Time"
                    , "State"
                    , "R"
                    , "Size"
                    ]
          toRow lg = [ XP.experimentName $ Logs.identifier lg
                     , XP.uuid $ Logs.identifier lg
                     , XP.tag $ Logs.identifier lg
                     , if Logs.marked lg then "*" else " "
                     , formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" $
                       XP.timestamp $ Logs.identifier lg
                     , show $ Logs.experimentExit lg
                     , case (Logs.experimentDataLocation lg) of
                         Logs.Remote _ -> "*"
                         _ -> " "
                     , Logs.printSize (Logs.dataSize lg) 5
                     ]
          renderTable width hs rows =
            let columns = transpose (hs : rows)
                columnWidths = map (maximum . (map length)) columns
                aws = (width-6-3*((length columns)-1)-
                       (sum $ tail columnWidths)
                      ):(tail columnWidths)
                r = (renderRow aws) 
            in (sep width) ++ r hs ++ (sep width)
               ++ concatMap r rows ++ (sep width)
          renderRow ws row =
            " | " ++ (intercalate " | " $ map (uncurry align) (zip ws row))
            ++ " | \n"
          align w s =
            let l = length s
            in if l <= w then
                 s ++ ((take $ w-l) $ repeat ' ')
               else (take $ w-3) s ++ "..."
          sep w = " " ++  (take  (w-2) $ repeat '-') ++ " \n"
filterC cond f xs = if cond then
                      filter f xs
                    else xs
  
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

