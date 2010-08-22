module IPatch.Version (version) where

import Data.Version

import qualified Paths_ipatch

version = showVersion Paths_ipatch.version
