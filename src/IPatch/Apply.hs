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
{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module IPatch.Apply where

import Control.Monad ( when )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Data.Functor

import Darcs.Commands ( DarcsCommand(..) )
import Darcs.Arguments ( DarcsFlag(..), fixFilePathOrStd, listFiles )
import Darcs.Repository
    ( amNotInRepository, applyToWorking, withRepoLock, RepoJob(..), Repository )
import Darcs.RepoPath ( FilePathLike(..) )
import Darcs.Patch ( Effect(effect) )
import Workaround ( getCurrentDirectory )
import Darcs.Global ( debugMessage )
import Darcs.Utils ( promptYorn )
import Darcs.SelectChanges
    ( WhichChanges(First),
      runSelection,
      selectChanges,
      selectionContextPrim )
import Darcs.Patch.Split ( primSplitter )
import Darcs.Witnesses.Ordered ( (:>)(..), nullFL, FL )
import Darcs.Witnesses.Sealed ( Sealed(Sealed), Sealed2(..), unsafeUnseal, unseal)

import IPatch.Common
    ( diffToPrims,
      initializeBaseState,
      withTempRepository,
      stdindefault,
      clonePathsWithDeletion )
import IPatch.DiffFile ( filesTouchedByDiff, readDiffFile )

applyHelp :: String
applyHelp =
    "The `ipatch apply file.patch' command works similar to a `patch file.patch' command.\n" ++
    "It will, however, prompt the user about each part of the patch, whether it should\n" ++
    "be applied or not. Using the integrated hunk editor, the user has full control over\n" ++
    "the chosen changes.\n"++
    "\n"++
    "No files are touched until the end, when the user is asked for a final confirmation.\n"

applyDescription :: String
applyDescription = "Apply a diff file interactively."

apply :: DarcsCommand
apply = DarcsCommand {commandProgramName = "ipatch",
                      commandName = "apply",
                      commandHelp = applyHelp,
                      commandDescription = applyDescription,
                      commandExtraArgs = 1,
                      commandExtraArgHelp = ["<PATCHFILE>"],
                      commandCommand = applyCmd,
                      commandPrereq = amNotInRepository,
                      commandGetArgPossibilities = listFiles,
                      commandArgdefaults = const stdindefault,
                      commandAdvancedOptions = [],
                      commandBasicOptions = []}

applyCmd :: [DarcsFlag] -> [String] -> IO ()
applyCmd _ [""] = fail "Empty filename argument given to apply!"
applyCmd opts [unfixed_patchesfile] = do
    maindir <- getCurrentDirectory
    patchesfile <- fixFilePathOrStd opts unfixed_patchesfile
    diffPS <- readDiffFile patchesfile
    files <- filesTouchedByDiff diffPS
    if null files
      then putStrLn "Patch seems to be empty"
      else withTempRepository "work" $ \rdir -> withRepoLock [LookForAdds] $ RepoJob $ \(repo :: Repository p r u t) -> do

        (_, repo) <- initializeBaseState rdir maindir files repo

        Sealed patch_ps <- diffToPrims diffPS repo

        -- Ask the user which parts of the patch to apply
        let context = selectionContextPrim "apply" [] (Just primSplitter) Nothing Nothing
        let selector = selectChanges First patch_ps
        (wanted_ps :> _) <- runSelection selector context

        when (nullFL patch_ps) $ do
            putStrLn "You selected nothing, so I'm exiting!"
            exitWith ExitSuccess
        debugMessage $ "Applying selected patches"

        applyToWorking repo opts wanted_ps `catch` \e ->
                fail ("Error applying patch to working dir:\n" ++ show e)
  
        yorn <- promptYorn "Really apply the selected changes?"
        when yorn $ do
            clonePathsWithDeletion (toFilePath rdir) maindir files

        {-
        debugMessage $ "Printing selected parts"
        withTempDir "ipatch-repo-old" $ \opath -> do
          withTempDir "ipatch-repo-new" $ \npath -> do
            setCurrentDirectory (toFilePath rdir)

            debugMessage $ "Write out patched state"
            withRepoLock (testByDefault []) $ \repo -> do
                n <- slurp_recorded repo
                withCurrentDirectory npath $ writeSlurpy n "."

            withRepoLock (testByDefault []) $ \repo -> do
                debugMessage $ "Return to unpatched state"
                withGutsOf repo $ do
                    -- How to wrap wanted_patch in FL wanted_patch?
                    (_ :> top_patch) <- splitAtFL 1 . patchSetToPatches  <$> read_repo repo
                    tentativelyRemovePatches repo [] top_patch
                    tentativelyAddToPending repo [] $ invert $ effect top_patch
                    finalizeRepositoryChanges repo
                    applyToWorking repo [] (invert wanted_ps) `catch`
                        \e -> fail ("Couldn't undo patch in working dir.\n" ++ show e)

            debugMessage $ "Write out unpatched state"
            withRepoLock (testByDefault []) $ \repo -> do
                o <- slurp_recorded repo
                withCurrentDirectory opath $ writeSlurpy o "."

            output <- execPipeIgnoreError "diff" ["-u","-r",toFilePath opath,toFilePath npath] empty
            putDoc output
        -}

        return ()
