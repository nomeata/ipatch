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
{-# LANGUAGE Rank2Types, ScopedTypeVariables, TypeFamilies, GADTs #-}
module IPatch.Common where

import Control.Applicative ( (<$>) )
import Control.Monad ( when, unless )
import System.Posix.Files ( getSymbolicLinkStatus, isRegularFile, isDirectory )
import System.Directory ( createDirectoryIfMissing, doesFileExist, removeFile )
import System.FilePath.Posix ( (</>), takeDirectory, normalise )
import System.Exit ( exitWith, ExitCode(..) )

import Darcs.Arguments ( DarcsFlag(LookForAdds) )
import Darcs.Repository
    ( RepoJob(..),
      Repository,
      createRepository,
      applyToWorking,
      finalizeRepositoryChanges,
      tentativelyAddPatch,
      withGutsOf,
      withRepoLock,
      invalidateIndex,
      unrecordedChanges )
import Darcs.Flags ( Compression(..), UseIndex(..), ScanKnown(..) )
import Darcs.RepoPath ( AbsolutePath, FilePathLike(..) )
import Darcs.External ( cloneFile )
import Darcs.Lock ( withTempDir )
import Darcs.Patch ( invert, fromPrims, namepatch, PrimOf, RepoPatch )
import Darcs.Global ( debugMessage )
import Darcs.Patch.PatchInfoAnd ( n2pia )
import Darcs.Utils ( clarifyErrors, promptYorn )
import Darcs.Witnesses.Ordered ( nullFL, FL(..) )
import Darcs.Witnesses.Sealed ( Sealed(Sealed), Sealed2(..) )
import Darcs.Witnesses.Sealed ( Sealed(Sealed), Sealed2(..), unsafeUnseal2 )
import Darcs.Patch.Apply ( ApplyState )
import Storage.Hashed.Tree ( Tree )

import IPatch.DiffFile ( applyDiff, DiffFile )

clonePathWithDeletion :: FilePath -> FilePath -> FilePath -> IO ()
clonePathWithDeletion source dest path = do
    let source' = source </> path
        dest' = dest </> path
    ex <- doesFileExist source'
    if ex
     then do
        fs <- getSymbolicLinkStatus source'
        if isDirectory fs
         then do
            createDirectoryIfMissing True dest'
         else
            if isRegularFile fs
             then do
                createDirectoryIfMissing True (dest </> takeDirectory path)
                cloneFile source' dest'
             else
                fail ("clonePathWithDeletion: Bad file " ++ source')
     else do
        exT <- doesFileExist dest'
        when exT $ removeFile dest'
   `catch` fail ("clonePathWithDeletion: Bad file " ++ source </> path)

clonePathsWithDeletion source dest = mapM_ (clonePathWithDeletion source dest)


withTempRepository :: String -> (AbsolutePath -> IO a) -> IO a
withTempRepository name job =
    withTempDir ("ipatch-repo-" ++ name) $ \rdir -> do
        debugMessage $ "Creating temporary repository in directory " ++ show rdir
        createRepository []
        job rdir

initializeBaseState
  :: (FilePathLike a, RepoPatch p, ApplyState p ~ Tree) =>
     a -> FilePath -> [FilePath] -> Repository p r u r ->
         IO (FL (PrimOf p) r r', Repository p r r' r')
initializeBaseState rdir sdir files repo = do

        -- There should be no local changes:
        pend <- unrecordedChanges (IgnoreIndex, ScanAll) repo Nothing
        (repo :: Repository p r r r) <- case pend of
            NilFL -> return repo
            _ -> fail $ "Repo passed to initializeBaseState has unrecord changes"
    
        debugMessage $ "Copying " ++ show (length files) ++ " files to temporary repository."  
        clonePathsWithDeletion sdir (toFilePath rdir) files

        -- Change the phantom type here.
        (repo :: Repository p r r' r) <- return $ unsafeUnseal2 (Sealed2 repo)

        -- Create a patch from the newly added files
        debugMessage $ "Creating initial check in patch"
        init_ps <- unrecordedChanges (IgnoreIndex, ScanAll) repo Nothing -- Correct flags?
        init_patch <- n2pia <$> namepatch "NODATE" "Initial state" "NOAUTHOR" [] (fromPrims init_ps)
        repo <- tentativelyAddPatch repo NoCompression init_patch
        invalidateIndex repo
        withGutsOf repo (finalizeRepositoryChanges repo)
            `clarifyErrors` "Failed to apply inital patch"
        return (init_ps, repo)


diffToPrims
  :: (RepoPatch p, ApplyState p ~ Tree, ApplyState (PrimOf p) ~ Tree) =>
     DiffFile -> Repository p r t t -> IO (Sealed (FL (PrimOf p) t))
diffToPrims diff repo = do

    debugMessage $ "Applying the user provided diff"
    -- Now apply the patch
    ok <- applyDiff diff

    -- Change the phantom type here.
    (repo :: Repository p r u' t) <- return $ unsafeUnseal2 (Sealed2 repo)

    debugMessage $ "Creating a patch from the user changes"
    -- Create another patch from the changed files
    patch_ps <- unrecordedChanges (IgnoreIndex, ScanAll) repo Nothing

    unless ok $
        if nullFL patch_ps
        then do
            putStrLn $ "The patch did not apply cleanly. Aborting."
            exitWith (ExitFailure 1)
        else do
            putStrLn $ "The patch did fully apply cleanly. "
            yorn <- promptYorn "Do you want to continue with the partially applied patch?"
            unless yorn $ do
                exitWith $ ExitFailure 1

    -- Now we obliterate the patch, undoing its effects
    applyToWorking repo [] (invert patch_ps) `catch` \e ->
        fail ("Couldn't undo diff effect in working dir.\n" ++ show e)

    return (Sealed patch_ps)
      
stdindefault :: a -> [String] -> IO [String]
stdindefault _ [] = return ["-"]
stdindefault _ x = return x
