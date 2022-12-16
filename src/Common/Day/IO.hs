module Common.Day.IO (runDays) where

import Control.StopWatch (stopWatch)
import System.Clock (nsec, sec)
import Text.Printf (printf)
import Common.Day

runDays :: [Day] -> [String] -> IO ()
runDays allDays args
  | null args = runDefault allDays
  | args == ["all"] = runAll allDays
  | otherwise = mapM_ runOrNotFound args
  where
    runOrNotFound :: String -> IO ()
    runOrNotFound input
      | null foundDays = do
        printf "%s  not found.\n"
      | otherwise = runDay . head $ foundDays
      where
        foundDays = filter ((== input) . name) allDays

runAll :: [Day] -> IO ()
runAll allDays = do
  (_, time) <- stopWatch (mapM_ runDay allDays)
  printf "Completed in %d.%0.9d seconds.\n" (sec time) (nsec time)

runDefault :: [Day] -> IO ()
runDefault allDays = do
  (_, time) <- stopWatch (mapM_ runOrOmit allDays)
  printf "Completed in %d.%0.9d seconds.\n" (sec time) (nsec time)
  where
    runOrOmit :: Day -> IO ()
    runOrOmit day
      | isDefault day = runDay day
      | otherwise = omitDay day

runDay :: Day -> IO ()
runDay day = do
  printf "Running %s (%s) ...\n" (friendlyName day) (name day)
  putStr " --> "
  (_, time) <- stopWatch (run day)
  printf "Finished. (%d.%0.9d seconds)\n" (sec time) (nsec time)

omitDay :: Day -> IO ()
omitDay day = do
  printf "%s (%s) omitted.\n" (friendlyName day) (name day)