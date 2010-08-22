{-
 - Copyright (C) 2010 Joachim Breitner
 - 
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation; either version 2, or (at your option)
 - any later version.
 - 
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 - 
 - You should have received a copy of the GNU General Public License
 - along with this program; see the file COPYING.  If not, write to
 - the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 - Boston, MA 02110-1301, USA.
 -}

import System.Environment ( getArgs )
import System.IO ( hSetBinaryMode, stdin, stdout )

import Darcs.Flags ( DarcsFlag(Verbose) )
import Darcs.RunCommand ( runTheCommand )

import IPatch.Version ( version )
import IPatch.Help
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
    _ -> do
      hSetBinaryMode stdin True
      hSetBinaryMode stdout True
      runTheCommand commandControlList (head argv) (tail argv)
