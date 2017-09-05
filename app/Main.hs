{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module Main where

import Lib
import Data.IORef
import Control.Monad
import Control.Concurrent(forkIO, threadDelay)
import Network.HTTP.Types.Status
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W
import qualified Data.Map.Strict as Map
import Debug.Trace
import qualified Data.Algorithm.Diff as D
import qualified Data.Algorithm.DiffOutput as D
import qualified Text.HTML.TagSoup as TS

data GlobalState = GlobalState { tasks :: IORef (Map.Map String Task)
                               , diffs :: IORef (Map.Map String Diff)
                               }

type TaskMap = Map.Map String Task


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

getDiffs :: TaskMap -> TaskMap -> Map.Map String (Maybe Bool)
getDiffs olds news = Map.mapWithKey diff olds
  where
    diff key oldTask = do
      newTask <- Map.lookup key news
      let hash  = pageHash oldTask
      let hash' = pageHash newTask
      traceM $ "Old: " ++ hash ++ " New: " ++ hash'
      let changed = hash /= hash'
      if changed
        then traceM $ ppDiff $ D.getDiff (parsedSource oldTask) (parsedSource newTask)
        else traceM "Nothing changed"
      return changed

diffPairs :: [D.Diff (TS.Tag PageSource)] -> [String]
diffPairs diffs = zipWith
      (\(D.First first) (D.Second second) ->
         "<<<<<<\n"
      ++ show first
      ++ "\n======\n"
      ++ show second
      ++ "\n>>>>>>\n"
      )
    (onlyFirsts diffs) (onlySeconds diffs)

onlySeconds = filter (\diff ->
  case diff of
    D.Second a -> True
    _         -> False)

onlyFirsts = filter (\diff ->
  case diff of
    D.First a -> True
    _         -> False)

ppDiff :: [D.Diff (TS.Tag PageSource)] -> String
ppDiff = unlines . diffPairs

parsedSource :: Task -> [TS.Tag PageSource]
parsedSource = TS.parseTags . pageSource

seconds :: Num a => a -> a
seconds = (*) 1000000

diff old@Task{pageHash = hash} new@Task{pageHash = hash'} =
  if hash == hash'
    then putStr "OK"
    else putStr "Changes detected!!!"

updatePages :: TaskMap -> IO TaskMap
updatePages = getPages . Map.keys

getPages :: [String] -> IO TaskMap
getPages urls = do
  tasks <- mapM urlToTask urls
  return $ Map.fromList $ zip urls tasks

testURLs :: [String]
testURLs = ["http://www.supremenewyork.com/shop/accessories/nizey7qdx/y6r07ij1u"
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
