import System.Environment ( getArgs )
import System.IO ( hSetBinaryMode, stdin, stdout )

import Darcs.Flags ( DarcsFlag(Verbose) )
import Darcs.RunCommand ( runTheCommand )

import Version ( version, context )
import Help
    ( commandControlList,
      helpCmd,
      listAvailableCommands,
      printVersion )

main = do
  argv <- getArgs
  case argv of
    []                  -> printVersion >> helpCmd [] []
    ["-h"]              -> helpCmd [] []
    ["--help"]          -> helpCmd [] []
    ["--overview"]      -> helpCmd [Verbose] []
    ["--commands"]      -> listAvailableCommands
    ["-v"]              -> putStrLn version
    ["--version"]       -> putStrLn version
    ["--exact-version"] -> do
              --putStrLn $ "darcs compiled on "++__DATE__++", at "++__TIME__
              putStrLn context
    _ -> do
      hSetBinaryMode stdin True
      hSetBinaryMode stdout True
      runTheCommand commandControlList (head argv) (tail argv)
