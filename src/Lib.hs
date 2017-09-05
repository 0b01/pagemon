module Lib where

import qualified Network.Wreq as Wreq
import Control.Lens
import Data.IORef
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Crypto.Hash as H
import qualified Data.Algorithm.Diff
import qualified Data.Map.Strict as Map
import qualified Text.HTML.TagSoup as TS

type PageHash = String
type PageURL = String
type PageSource = BSL.ByteString

data Task = Task { pageSource :: PageSource
                 , pageHash :: PageHash
                 , pageOpts :: Opts
                 } deriving (Show, Eq)

data Diff = Diff { pageUrl :: PageURL
                 , pageDiff :: (String, String)
                 }

data TagType = Open
             | Close
             | TextRegex
             deriving (Show, Eq, Ord)

data Opt = BlackList TagType PageSource
         | WhiteList TagType PageSource
         deriving (Show, Eq, Ord)

type Opts = [Opt]

data URL = URL {
  url :: PageURL,
  opts :: Opts
} deriving (Show, Eq, Ord)

type TaskMap = Map.Map URL Task


fetchPage :: String -> IO (Maybe PageSource)
fetchPage url = do
  r <- Wreq.get url
  return $ r ^? Wreq.responseBody


hexSha3_512 :: BS.ByteString -> PageHash
hexSha3_512 bs = show (H.hash bs :: H.Digest H.SHA1)

-- fetchPage page url then hash
pageToHash :: BSL.ByteString -> PageHash
pageToHash page = do
  let strictBS = BSL.toStrict page
  hexSha3_512 strictBS

urlToTask :: URL -> IO Task
urlToTask URL {url=url, opts=opts}= do
  pageSource <- fetchPage url
  case pageSource of
    Just source -> return Task {pageSource = source, pageHash = pageToHash source, pageOpts = opts}
