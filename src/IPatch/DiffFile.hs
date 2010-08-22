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

