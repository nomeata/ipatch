{-# LANGUAGE Rank2Types #-}
module Common where

import Control.Applicative ( (<$>) )
import Control.Monad (when)
import System.Posix.Files ( getSymbolicLinkStatus, isRegularFile, isDirectory )
import System.Directory ( createDirectoryIfMissing, doesFileExist, removeFile )
import System.FilePath.Posix ( (</>), takeDirectory, normalise )

import Darcs.Arguments ( DarcsFlag(LookForAdds) )
import Darcs.Repository
    ( createRepository,
      applyToWorking,
      finalizeRepositoryChanges,
      tentativelyAddPatch,
      withGutsOf,
      withRepoLock,
      invalidateIndex,
      unrecordedChanges )
import Darcs.Flags ( Compression(..) )
import Darcs.RepoPath ( AbsolutePath, FilePathLike(..) )
import Darcs.External ( cloneFile )
import Darcs.Lock ( withTempDir )
import Darcs.Patch ( invert, fromPrims, namepatch )
import Darcs.Global ( debugMessage )
import Darcs.Hopefully ( n2pia )
import Darcs.Utils ( clarifyErrors )

import DiffFile ( applyDiff )

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

initializeBaseState rdir sdir files = do
    debugMessage $ "Copying " ++ show (length files) ++ " files to temporary repository."  
    clonePathsWithDeletion sdir (toFilePath rdir) files
    -- Create a patch from the newly added files
    debugMessage $ "Creating initial check  in patch"
    withRepoLock [LookForAdds] $ \repo -> do
        init_ps <- unrecordedChanges [LookForAdds] repo [] -- Correct flags?
        init_patch <- n2pia <$> namepatch "NODATE" "Initial state" "NOAUTHOR" [] (fromPrims init_ps)
        tentativelyAddPatch repo [] init_patch
        invalidateIndex repo
        withGutsOf repo (finalizeRepositoryChanges repo)
            `clarifyErrors` "Failed to apply inital patch"
        return init_ps


diffToPrims diff = do
    debugMessage $ "Applying the user provided diff"
    -- Now apply the patch
    applyDiff diff

    debugMessage $ "Creating a patch from the user changes"
    withRepoLock [LookForAdds] $ \repo -> do
        -- Create another patch from the changed files
        patch_ps <- unrecordedChanges [LookForAdds] repo []
        -- patch_patch <- n2pia <$> namepatch date "Patch effect" author [] (fromPrims patch_ps)
        -- tentativelyAddPatch repo [] patch_patch
        -- Now we obliterate the patch, undoing its effects
        applyToWorking repo [] (invert patch_ps) `catch` \e ->
            fail ("Couldn't undo diff effect in working dir.\n" ++ show e)
        return patch_ps
      
stdindefault :: a -> [String] -> IO [String]
stdindefault _ [] = return ["-"]
stdindefault _ x = return x
