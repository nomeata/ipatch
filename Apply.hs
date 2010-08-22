{-# LANGUAGE Rank2Types #-}
module Apply where

import Control.Monad ( when )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )

import Darcs.Commands
    ( DarcsCommand(DarcsCommand, commandAdvancedOptions,
                   commandArgdefaults, commandBasicOptions, commandCommand,
                   commandDescription, commandExtraArgHelp, commandExtraArgs,
                   commandGetArgPossibilities, commandHelp, commandName,
                   commandPrereq) )
import Darcs.Arguments ( DarcsFlag, fixFilePathOrStd, listFiles )
import Darcs.Repository
    ( amNotInRepository, applyToWorking, withRepoLock )
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
import Darcs.Witnesses.Ordered ( (:>)(..), nullFL )

import Common
    ( diffToPrims,
      initializeBaseState,
      withTempRepository,
      stdindefault,
      clonePathsWithDeletion )
import DiffFile ( filesTouchedByDiff, readDiffFile )

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
apply = DarcsCommand {commandName = "apply",
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
      else withTempRepository "work" $ \rdir -> do
        initializeBaseState rdir maindir files

        patch_ps <- diffToPrims diffPS

        -- Ask the user which parts of the patch to apply
        let context = selectionContextPrim "apply" [] (Just primSplitter) []
        let selector = selectChanges First patch_ps
        (wanted_ps :> _) <- runSelection selector context

        when (nullFL patch_ps) $ do
            putStrLn "You selected nothing, so I'm exiting!"
            exitWith ExitSuccess
        debugMessage $ "Applying selected patches"
        withRepoLock [] $ \repo -> do
            {- wanted_patch <- namepatch "NODATE" "Chosen Patch" "NOAUTHOR" [] (fromPrims wanted_ps)
            tentativelyAddPatch repo [] $ n2pia wanted_patch
            invalidateIndex repo
            withGutsOf repo (finalizeRepositoryChanges repo)
                        `clarifyErrors` "Failed to apply inital patch"
            -}
            applyToWorking repo opts (effect wanted_ps) `catch` \e ->
                    fail ("Error applying patch to working dir:\n" ++ show e)
      
            yorn <- promptYorn "Really apply the selected changes?"
            when (yorn == 'y') $ do
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
