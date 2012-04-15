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
module IPatch.DiffFile where


import qualified Data.ByteString as B
    ( ByteString, null, hGetContents, readFile, hPutStr )
import qualified Data.ByteString.Char8 as BC ( unpack )
import System.IO ( stdin, openBinaryFile, IOMode(..), hClose )
import System.Process ( createProcess, proc, CreateProcess(..), StdStream(..), waitForProcess )
import System.Exit ( ExitCode(..) )
import Control.Applicative ( (<$>) )
import Darcs.RepoPath ( FilePathLike(..), AbsolutePathOrStd, useAbsoluteOrStd )
import Darcs.External ( execDocPipe )
import Printer ( packedString, renderPS )
import ByteStringUtils ( linesPS )

newtype DiffFile = DiffFile B.ByteString

readDiffFile :: AbsolutePathOrStd -> IO DiffFile
readDiffFile = fmap DiffFile . useAbsoluteOrStd (B.readFile . toFilePath) (B.hGetContents stdin) 

filesTouchedByDiff :: DiffFile -> IO [FilePath]
filesTouchedByDiff (DiffFile bs) = map BC.unpack . filter (not . B.null) . linesPS <$> execPSPipe "diffstat" ["-l","-p1"] bs

applyDiff :: DiffFile -> IO Bool
applyDiff (DiffFile bs) = do
    devNull <- openBinaryFile "/dev/null" WriteMode
    (Just i,_,_,pid) <- createProcess (proc "patch" ["--reject-file","-","--strip","1"])
        { std_out = UseHandle devNull
        , std_err = Inherit
        , std_in = CreatePipe }
    B.hPutStr i bs
    hClose i
    rval <- waitForProcess pid
    case rval of
        ExitSuccess     -> return True
        ExitFailure 1   -> return False
        ExitFailure 127 -> fail $ "patch not found"
        _               -> fail $ "patch failed"


execPSPipe :: String -> [String] -> B.ByteString -> IO B.ByteString
execPSPipe c args ps = fmap renderPS
                     $ execDocPipe c args
                     $ packedString ps

