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
{-# LANGUAGE Rank2Types, ScopedTypeVariables, GADTs #-}
module IPatch.Split where

import qualified Data.ByteString as B ( writeFile )
import Control.Applicative ( (<$>) )
import System.Directory ( createDirectory )
import Control.Monad ( when )
import Control.Monad.Fix ( fix )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import System.FilePath ( (</>) )

import Darcs.Commands ( DarcsCommand(..) )
import Darcs.Arguments ( DarcsFlag(..), fixFilePathOrStd, listFiles )
import Darcs.Repository ( amNotInRepository, withRepoLock, RepoJob(..), Repository )
import Darcs.External ( execPipeIgnoreError )
import Darcs.Lock ( withTempDir )
import Darcs.Patch ( apply, PrimOf )
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
import Darcs.Witnesses.Ordered ( FL(..), (:>)(..), nullFL, mapFL )
import Darcs.Witnesses.Sealed ( Sealed(Sealed), Sealed2(..) )

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
split = DarcsCommand {commandProgramName = "ipatch",
                      commandName = "split",
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
      else withTempRepository "work" $ \rdir -> withRepoLock [LookForAdds] $ RepoJob $ \(repo :: Repository p r u r) -> do

        (init_ps, (repo :: Repository p r r r)) <- initializeBaseState rdir maindir files repo
        Sealed (patch_ps :: FL (PrimOf p) r d) <- diffToPrims diffPS repo

        when (nullFL patch_ps) $ do
            putStrLn "Patch seems to be empty"
            exitWith ExitSuccess

        let run :: forall x y. 
                (forall x y . (FL (PrimOf p) x y) -> Int -> IO (FL (Annotated (FL (PrimOf p)) String) x y)) ->
                (FL (PrimOf p) x y) -> Int -> IO (FL (Annotated (FL (PrimOf p)) String) x y)
            run repeat remaining_ps n = case remaining_ps of
                NilFL -> return NilFL; _ -> do
                putStrLn $ "Please select the changes for the " ++ ordinal n ++ " patch"
                let context = selectionContextPrim "split" [] (Just primSplitter) Nothing Nothing
                let selector = selectChanges First remaining_ps
                (chosen_ps :> remaining_ps') <- runSelection selector context
                if (nullFL chosen_ps) 
                  then do
                    yorn <- promptYorn "You selected nothing. Do you want to abort?"
                    when yorn $ do
                        exitWith ExitSuccess
                    repeat remaining_ps n
                  else do
                    filename <- askUser $ "Please enter filename for the " ++ ordinal n ++ " patch: "
                    ((Annotated chosen_ps filename) :>:) <$> repeat remaining_ps' (succ n)

        (chunks :: FL (Annotated (FL (PrimOf p)) String) r d) <- run undefined patch_ps 1
        let sealeadChunks = mapFL (\(Annotated a b) -> (Sealed2 a, b)) chunks

        when (nullFL chunks) $ do
            putStrLn "No patched splitted, exiting."
            exitWith ExitSuccess

        withTempDir "ipatch-diff-area" $ \bpath -> do
            debugMessage "Setting up old and new staging areas"
            createDirectory "old" -- Find nicer names based on original directory name
            createDirectory "new"

            withCurrentDirectory "new" $ apply init_ps 
            let applyAndDiff (Sealed2 last) (Sealed2 next) name = do
                withCurrentDirectory "old" $ apply last
                withCurrentDirectory "new" $ apply next
                output <- renderPS <$> execPipeIgnoreError "diff" ["-Nur","old","new"] empty
                putStrLn $ "Writing File " ++ name ++ " .."
                B.writeFile (maindir </> name) output
            sequence_ $ zipWith3 applyAndDiff (Sealed2 init_ps : map fst sealeadChunks) (map fst sealeadChunks) (map snd sealeadChunks)

ordinal 1 = "first"
ordinal 2 = "second"
ordinal 3 = "third"
ordinal n = show n ++ "th"

data Annotated a b x y = Annotated (a x y) b
