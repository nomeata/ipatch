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
    ( ByteString, null, hGetContents, readFile )
import qualified Data.ByteString.Char8 as BC ( unpack )
import System.IO ( stdin )
import Control.Applicative ( (<$>) )
import Darcs.RepoPath ( FilePathLike(..), useAbsoluteOrStd )
import Darcs.External ( execDocPipe )
import Printer ( packedString, renderPS )
import ByteStringUtils ( linesPS )

newtype DiffFile = DiffFile B.ByteString

readDiffFile = fmap DiffFile . useAbsoluteOrStd (B.readFile . toFilePath) (B.hGetContents stdin) 

filesTouchedByDiff (DiffFile bs) = map BC.unpack . filter (not . B.null) . linesPS <$> execPSPipe "diffstat" ["-l","-p1"] bs

applyDiff (DiffFile bs) = execPSPipe "patch" ["-r","-","-p1"] bs


execPSPipe :: String -> [String] -> B.ByteString -> IO B.ByteString
execPSPipe c args ps = fmap renderPS
                     $ execDocPipe c args
                     $ packedString ps

