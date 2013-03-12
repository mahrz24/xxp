module XXP.Modules.Build (cmake) where

import Control.Monad

import XXP.State
import XXP.Logging
import XXP.Experiment
import XXP.Process

cmake :: String -> XXP ()
cmake target = do
  debug <- liftM (debugMode . identifier) get
  -- Run cmake in build directory "i.e cmake ../src"
  let buildMode = if debug then
                    "-DCMAKE_BUILD_TYPE=Debug"
                  else "-DCMAKE_BUILD_TYPE=Release"
  exitCode <- customProc "./build" "cmake" ["../src", buildMode]
  when (exitCode /= ExitSuccess) (throwXXP $
                                    ErrorCall $ "cmake "
                                      ++ show exitCode)
  -- Run make target in build directory
  exitCode <- customProc "./build" "make" [target]
  when (exitCode /= ExitSuccess) (throwXXP $
                                    ErrorCall $ "make "
                                      ++ show exitCode)
  return ()
