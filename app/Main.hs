{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module Main where

import Lib
import Diff
import Data.IORef
import Control.Monad
import Control.Concurrent(forkIO, threadDelay)
import Network.HTTP.Types.Status
import Data.Text.Lazy.Encoding
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as TL
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W
import qualified Data.Map.Strict as Map
-- import Text.HTML.TagSoup as TS

import Debug.Trace


data GlobalState = GlobalState { tasks :: IORef (Map.Map URL Task)
                               , diffs :: IORef (Map.Map URL Diff)
                               }

initialize :: IO GlobalState
initialize = do
  titles <- getPages testURLs
  tasksRef <- newIORef titles
  diffsRef <- newIORef Map.empty
  return GlobalState { tasks = tasksRef, diffs = diffsRef }

startTimer :: GlobalState -> IO ()
startTimer (tasks -> ref') = do
  threadId <- forkIO loop
  return ()
  where
    loop = do
      threadDelay $ seconds 2
      oldPages <- readIORef ref'
      newPages <- updatePages oldPages
      atomicWriteIORef ref' newPages
      print $ getDiffs oldPages newPages
      loop

seconds :: Num a => a -> a
seconds = (*) 1000000

updatePages :: TaskMap -> IO TaskMap
updatePages = getPages . Map.keys

getPages :: [URL] -> IO TaskMap
getPages urls = do
  tasks <- mapM urlToTask urls
  return $ Map.fromList $ zip urls tasks

testURLs :: [URL]
testURLs = [
             URL { url = "https://github.com" --"http://www.supremenewyork.com/shop/accessories/nizey7qdx/y6r07ij1u"
                 , opts = [ BlackList Open "meta"
                          , BlackList Text "*"
                          ]
                 }
           ]

main :: IO ()
main = do
  st <- initialize
  startTimer st
  W.run 2000 $ \request respond -> do
    txt <- renderState st
    respond $ W.responseLBS status200 [] txt

renderState :: GlobalState -> IO BS.ByteString
renderState (tasks -> ref) = do
  watches <- readIORef ref
  return $ encodeUtf8 $ TL.pack $ show watches
