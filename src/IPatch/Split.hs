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
{-# LANGUAGE Rank2Types #-}
module IPatch.Split where

import qualified Data.ByteString as B ( writeFile )
import Control.Applicative ( (<$>) )
import System.Directory ( createDirectory )
import Control.Monad ( when )
import Control.Monad.Fix ( fix )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import System.FilePath ( (</>) )

import Darcs.Commands
    ( DarcsCommand(DarcsCommand, commandAdvancedOptions,
                   commandArgdefaults, commandBasicOptions, commandCommand,
                   commandDescription, commandExtraArgHelp, commandExtraArgs,
                   commandGetArgPossibilities, commandHelp, commandName,
                   commandPrereq) )
import Darcs.Arguments ( DarcsFlag, fixFilePathOrStd, listFiles )
import Darcs.Repository ( amNotInRepository )
import Darcs.External ( execPipeIgnoreError )
import Darcs.Lock ( withTempDir )
import Darcs.Patch ( Prim, apply )
import Printer ( empty, renderPS )
import Workaround ( getCurrentDirectory )
import Darcs.Global ( debugMessage )
import Darcs.Utils ( askUser, promptYorn )
import Darcs.Utils ( withCurrentDirectory )
import Darcs.SelectChanges
    ( WhichChanges(First),
      runSelection,
      selectChanges,
      selectionContextPrim )
import Darcs.Patch.Split ( primSplitter )
import Darcs.Witnesses.Ordered ( FL, (:>)(..), nullFL )

import IPatch.Common
    ( withTempRepository,
      initializeBaseState,
      diffToPrims,
      stdindefault )
import IPatch.DiffFile ( readDiffFile, filesTouchedByDiff )

splitHelp :: String
splitHelp =
    "The `ipatch split file.patch' lets the user select different parts (hunks) of the\n" ++
    "given patch file. After making a choice for each hunk, the user has to provide a\n" ++
    "file name where the selected changes are stored. This procedure is repeated until\n" ++
    "each change in the original file has been selected for one output file.\n" ++
    "\n"++
    "No files are modified by this command. The output patch files are all written at the\n" ++
    "of the process.\n"


splitDescription :: String
splitDescription = "Split a diff file interactively."

split :: DarcsCommand
split = DarcsCommand {commandName = "split",
                      commandHelp = splitHelp,
                      commandDescription = splitDescription,
                      commandExtraArgs = 1,
                      commandExtraArgHelp = ["<PATCHFILE>"],
                      commandCommand = splitCmd,
                      commandPrereq = amNotInRepository,
                      commandGetArgPossibilities = listFiles,
                      commandArgdefaults = const stdindefault,
                      commandAdvancedOptions = [],
                      commandBasicOptions = []}

splitCmd :: [DarcsFlag] -> [String] -> IO ()
splitCmd _ [""] = fail "Empty filename argument given to split!"
splitCmd opts [unfixed_patchesfile] = do
    maindir <- getCurrentDirectory
    patchesfile <- fixFilePathOrStd opts unfixed_patchesfile
    diffPS <- readDiffFile patchesfile
    files <- filesTouchedByDiff diffPS
    if null files
      then putStrLn "Patch seems to be empty"
      else withTempRepository "work" $ \rdir -> do
        init_ps <- initializeBaseState rdir maindir files
        patch_ps <- diffToPrims diffPS

        let run :: (FL Prim -> Int -> IO [(FL Prim,String)]) ->
                   (FL Prim -> Int -> IO [(FL Prim,String)])
            run repeat remaining_ps n = if nullFL remaining_ps then return [] else do
                putStrLn $ "Please select the changes for the " ++ ordinal n ++ " patch"
                --putStrLn $ "To choose " ++ show remaining_ps
                let context = selectionContextPrim "split" [] (Just primSplitter) []
                let selector = selectChanges First remaining_ps
                (chosen_ps :> remaining_ps') <- runSelection selector context
                {- we need to force chosen_ps before accessing remaining_ps',
                 - see pull_only_firsts in ./Darcs/Patch/Choices.hs. There is a reason
                 - why unsafeReadIO is called unsafe...-}
                --length (show chosen_ps) `seq` return ()
                --length (show remaining_ps') `seq` return ()
                if (nullFL chosen_ps) 
                  then do
                    yorn <- promptYorn "You selected nothing. Do you want to abort?"
                    when (yorn == 'y') $ do
                        exitWith ExitSuccess
                    repeat remaining_ps n
                  else do
                    filename <- askUser $ "Please enter filename for the " ++ ordinal n ++ " patch: "
                    --putStrLn $ "Chosen " ++ show chosen_ps
                    --putStrLn $ "Left " ++ show remaining_ps'
                    ((chosen_ps,filename) :) <$> repeat remaining_ps' (succ n)

        chunks <- fix run patch_ps 1

        when (null chunks) $ do
            putStrLn "No patched splitted, exiting."
            exitWith ExitSuccess

        withTempDir "ipatch-diff-area" $ \bpath -> do
            debugMessage "Setting up old and new staging areas"
            createDirectory "old" -- Find nicer names based on original directory name
            createDirectory "new"

            withCurrentDirectory "new" $ apply [] init_ps 
            let applyAndDiff last next name = do
                withCurrentDirectory "old" $ apply [] last
                withCurrentDirectory "new" $ apply [] next
                output <- renderPS <$> execPipeIgnoreError "diff" ["-Nur","old","new"] empty
                putStrLn $ "Writing File " ++ name ++ " .."
                B.writeFile (maindir </> name) output
            sequence_ $ zipWith3 applyAndDiff (init_ps : map fst chunks) (map fst chunks) (map snd chunks)


ordinal 1 = "first"
ordinal 2 = "second"
ordinal 3 = "third"
ordinal n = show n ++ "th"
