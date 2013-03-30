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
import Control.Proxy

import Data.Maybe
import Data.List
import Data.List.Split
import Data.Time.Clock
import Data.Time.Format
import qualified Data.ByteString.Lazy as BS

import System.Locale
import System.Log.Logger
import System.Log.Handler.Simple
import System.IO

import Text.Hastache
import Text.Hastache.Context

import qualified HsShellScript as SH
import TermSize
import Paths_xxp

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
                    , pipeData::Maybe String
                    , debugMode::Bool
                    , gdb::Bool
                    }
              | Gdb { binary::String }
              | Check { match :: Maybe String
                      , marked :: Bool
                      , last :: Bool
                      }
              | Fetch { match :: Maybe String
                      , marked :: Bool
                      , last :: Bool
                      }
              | Info  { match :: Maybe String
                      , marked :: Bool
                      , last :: Bool
                      }
              | Data  { match :: Maybe String
                      , identifier :: String
                      , last :: Bool
                      }
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
              | Unmark { match :: Maybe String }
              | List
              | Create { adaptor :: String
                       , experiment :: String
                       }
              | Path { path :: String }
              deriving (Data, Typeable, Show, Eq)

data CompilationResult = CompilationSuccess
                       | CompilationFailure
                       deriving (Eq)

commandRun = Run { experiment = def &= argPos 0 &= typ "<experiment>"
                 , tag = def &= typ "<label>"
                 , customConfig = def &= typ "<json>"
                 , forceConfig = def &= typ "<filename>"
                 , pipeData = def &= typ "<pattern>"
                 , debugMode = def &= help "Run in debug mode"
                 , gdb = def &= help "Run the binary using gdbserver"
                 }
             &= details [ "Examples:", "xxp run test1"]

commandFetch = Fetch { match = def &= args &= typ "<pattern>"
                     , marked = def &= help "Fetch all marked logs"
                     , last = def &= help "Fetch last log"
                     }
                &= details [ "Examples:", "xxp fetch -l"]

commandCheck = Check { match = def &= args &= typ "<pattern>"
                     , marked = def &= help "Check all marked logs"
                     , last = def &= help "Check last log"
                     }
                &= details [ "Examples:", "xxp check -l"]

commandData = Data { match = def &= args &= typ "<pattern>"
                   , identifier = def &= help "Data file identifier"
                   , last = def &= help "Data of last log"
                   }
                &= details [ "Examples:", "xxp data -l -i main"]

commandInfo = Info { match = def &= args &= typ "<pattern>"
                   , marked = def &= help "Info about all marked logs"
                   , last = def &= help "Info about last log"
                   }
                &= details [ "Examples:", "xxp info -l"]

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

commandUnmark = Unmark { match = def &= args &= typ "<pattern>" }
             &= details [ "Examples:", "xxp mark 201304"]

commandList = List

commandCreate = Create { adaptor = def &= argPos 0 &= typ "<adaptor>"
                       , experiment = def &= argPos 1 &= typ "<experiment>"
                       }
             &= details [ "Examples:", "xxp create cpp test1"]

commandPath = Path { path = def &= args &= typ "<path>" }

commands :: Mode (CmdArgs Commands)
commands = cmdArgsMode $ modes [ commandRun
                               , commandCheck
                               , commandFetch
                               , commandGdb
                               , commandClean
                               , commandRemove
                               , commandMark
                               , commandUnmark
                               , commandList
                               , commandInfo
                               , commandData
                               , commandCreate
                               , commandPath
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
    -- Get the pipe argument
    pipes <- maybe (return [])
              (\pipes -> do logs <- Logs.loadAllLogs
                            let pPipes = map (splitOn ":") $
                                           splitOn "," pipes
                            return $ [show $
                              map (\(t:[p]) -> (t, XP.uniqueID' $
                                                 Logs.identifier $
                                                 head $
                                                 filter (Logs.anyMatches p) $
                                                 filter (not . Logs.running) $
                                                 filter (Logs.success) logs)
                                  ) pPipes])
              pipeData
    exitCode <- rawSystem (pwd </> "build" </> binary) $ [ show lvl
                                                         , tag
                                                         , customConfig
                                                         , forceConfig
                                                         , show debugMode
                                                         , show gdb
                                                         ] ++ pipes
    case exitCode of
      ExitSuccess -> noticeM "xxp.log" $
                     "Experiment finished: " ++ experiment
      ExitFailure i -> errorM "xxp.log" $
                       "Experiment failed with error code: " ++ (show i)

exec opts@Fetch{..} = do
  logs <- liftM ((filter (not . Logs.fetched)) .
                 (filter (Logs.remote)) .
                 (filter (not . Logs.running)))
          Logs.loadAllLogs
  let filtered = if last then
                   (\x -> [x]) . head . Logs.sortLogs $ logs
                   else if marked then
                           filter Logs.marked logs
                         else
                           logs

  let matched = case match of
        Just p -> filter (Logs.anyMatches p) filtered
        Nothing -> filtered
  mapM_ Logs.fetchLog matched

exec opts@Check{..} = do
  logs <- liftM ((filter (not . Logs.fetched)) .
                 (filter (Logs.remote)) .
                 (filter (Logs.running)))
          Logs.loadAllLogs
  let filtered = if last then
                   (\x -> [x]) . head . Logs.sortLogs $ logs
                   else if marked then
                           filter Logs.marked logs
                         else
                           logs

  let matched = case match of
        Just p -> filter (Logs.anyMatches p) filtered
        Nothing -> filtered
  mapM_ Logs.checkLog matched

exec opts@Data{..} = do
  logs <- liftM (filter (Logs.success))
          Logs.loadAllLogs
  let filtered = if last then
                   (\x -> [x]) . head . Logs.sortLogs $ logs
                   else logs

  let matched = case match of
        Just p -> filter (Logs.anyMatches p) filtered
        Nothing -> filtered
  when (not $ null matched) $ do
    let dataDir = Logs.dataDir $ head matched
    contents <- getDirectoryContents dataDir
    let relevant = filter (\x -> identifier == (head $ splitOn "." x))
                   $ filter (`notElem` [".", ".."]) contents
    mapM_ (\f -> do h <- openFile (dataDir </> f) ReadMode
                    runProxy $ hGetLineS h >-> putStrLnD
                    hClose h) relevant

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
  let matched = case match of
        Just p -> filter (Logs.anyMatches p) logs
        Nothing -> logs
  mapM_ Logs.unmarkLog matched

exec opts@Info{..} = do
  logs <- liftM (filter (Logs.success))
          Logs.loadAllLogs
  let filtered = if last then
                   (\x -> [x]) . head . Logs.sortLogs $ logs
                   else if marked then
                           filter Logs.marked logs
                         else
                           logs

  let matched = case match of
        Just p -> filter (Logs.anyMatches p) filtered
        Nothing -> filtered
  mapM_ logInfo matched
    where logInfo lg = do cfg <- Logs.loadLogConfig lg
                          logs <- Logs.loadLogLogs lg
                          dataTags <- Logs.loadLogDataTags lg
                          putStrLn "Experiment identifier:"
                          putStrLn $ unwords $ 
                            [ XP.uuid $ Logs.identifier lg
                            , XP.experimentName $ Logs.identifier lg
                            , XP.tag $ Logs.identifier lg
                            ]
                          putStrLn "Experiment logs:"
                          putStrLn logs
                          putStrLn "Experiment data:"
                          putStrLn $ unwords $ dataTags
                          putStrLn "Experiment config:"
                          putStrLn cfg

exec opts@List{..} = do
  logs <- liftM Logs.sortLogs $ Logs.loadAllLogs
  (_,termWidth) <- getTermSize
  putStr $ renderTable termWidth headers (map toRow logs)
    where headers = [ "UUID"
                    , "Name"
                    , "Tag"
                    , "M"
                    , "Time"
                    , "State"
                    , "R"
                    , "D"
                    , "Size"
                    ]
          toRow lg = [ XP.uuid $ Logs.identifier lg
                     , XP.experimentName $ Logs.identifier lg
                     , XP.tag $ Logs.identifier lg
                     , if Logs.marked lg then "*" else " "
                     , formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" $
                       XP.timestamp $ Logs.identifier lg
                     , show $ Logs.experimentExit lg
                     , if Logs.remote lg then
                         if Logs.fetched lg then "*"
                         else "+"
                       else " "
                     , if Logs.external lg then
                         "@"
                       else " "
                     , Logs.printSize (Logs.dataSize lg) 5
                     ]
          renderTable width hs rows =
            let columns = transpose (hs : rows)
                colWidths = map (maximum . (map length)) columns
                allocWidths = balanceWidths width colWidths
                width' = (sum allocWidths) + (length columns)*3 + 2
                r = (renderRow allocWidths)
            in (sep width') ++ r hs ++ (sep width')
               ++ concatMap r rows ++ (sep width')
          renderRow ws row =
            " | " ++ (intercalate " | " $ map (uncurry align) (zip ws row))
            ++ " | \n"
          align w s =
            let l = length s
            in if l <= w then
                 s ++ ((take $ w-l) $ repeat ' ')
               else (take $ w-3) s ++ "..."
          sep w = " " ++  (take  (w-2) $ repeat '-') ++ " \n"
          balanceWidths width colWidths =
            let numCols = (length colWidths)
                colTail = tail colWidths
                headWidth = width-6-3*(numCols-1)-(sum colTail)
                (headWidth',excessWidth) = if headWidth < 5 then
                                             (5,-headWidth+5)
                                           else (headWidth,0)
            in headWidth':(balance excessWidth $ tail colWidths)
          balance x colWidths | x <= 0 = colWidths
          balance x colWidths = let newWidths = map (\x -> if x > 5 then
                                                             x-1 else x)
                                                colWidths
                                in balance (x-(sum colWidths)+(sum newWidths))
                                     newWidths


exec opts@Create{..} = do
  if adaptor `notElem` ["cpp"] then
    errorM "xxp.log" ("Error adaptor not supported")
    else do createDirectoryIfMissing True experiment
            logConfig <- getDataFileName "scaffold/xxp/log.json"
            copyFile logConfig (experiment </> "log.json")
            config <- getDataFileName "scaffold/xxp/config.json"
            copyFile config (experiment </> "config.json")
            xp <- getDataFileName $ "scaffold/xxp/xp_" ++ adaptor ++ ".hs"
            template experiment (const $ "xp_" ++ experiment ++ ".hs") xp
            createDirectoryIfMissing True (experiment </> "src")
            adaptorDir <- getDataFileName $ "scaffold/" ++ adaptor
            contents <- getDirectoryContents adaptorDir
            let visibles = map ((</>) adaptorDir) $ Logs.getVisible contents
            mapM_ (template (experiment </> "src") takeFileName) visibles
  where template d g f = do res <- hastacheFile
                                   defaultConfig
                                   f
                                   (mkGenericContext opts)
                            BS.writeFile (d </> (g f)) res
exec opts@Path{..} = do
  fn <- getDataFileName path
  putStrLn fn

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

