module Diff where

import Lib
import qualified Data.Algorithm.Diff as D
import qualified Data.Algorithm.DiffOutput as D
import qualified Data.Map.Strict as Map
import qualified Text.HTML.TagSoup as TS
import Debug.Trace

getDiffs :: TaskMap -> TaskMap -> Map.Map URL (Maybe Bool)
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

parsedSource :: Task -> [TS.Tag PageSource]
parsedSource = TS.parseTags . pageSource

ppDiffPairs :: [D.Diff (TS.Tag PageSource)] -> [String]
ppDiffPairs diffs = zipWith
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
ppDiff = unlines . ppDiffPairs
