{-# LANGUAGE OverloadedStrings #-}

module Diff where

import Lib
import Control.Monad
import qualified Data.Algorithm.Diff as D
import qualified Data.Algorithm.DiffOutput as D
import qualified Data.Map.Strict as Map
import qualified Text.HTML.TagSoup as TS
import Debug.Trace
import Text.Regex.PCRE

getDiffs :: TaskMap -> TaskMap -> Map.Map URL (Maybe Bool)
getDiffs olds news = Map.mapWithKey diff olds
  where
    diff key oldTask = do
      newTask <- Map.lookup key news
      let hash  = pageHash oldTask
      let hash' = pageHash newTask
      traceM $ "Old: " ++ hash ++ "\nNew: " ++ hash'
      let diffs = D.getDiff (parsedSource oldTask) (parsedSource newTask)
      let opts = pageOpts oldTask
      let filteredDiffs = filtered opts diffs
      let changed = (hash /= hash') && (not.null $ filteredDiffs)
      if changed
        then traceM $ ppDiff filteredDiffs
        else traceM "Nothing changed"
      return changed

parsedSource :: Task -> [TS.Tag PageSource]
parsedSource = TS.parseTags . pageSource

ppDiff :: [D.Diff (TS.Tag PageSource)] -> String
ppDiff = unlines . ppDiffPairs

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

onlySeconds :: [D.Diff t] -> [D.Diff t]
onlySeconds = filter (\diff ->
  case diff of
    D.Second _ -> True
    _         -> False)

onlyFirsts :: [D.Diff t] -> [D.Diff t]
onlyFirsts = filter (\diff ->
  case diff of
    D.First _ -> True
    _         -> False)
--
-- filtered :: [D.Diff (TS.Tag PageSource)] -> Opts -> [D.Diff (TS.Tag PageSource)]
-- filtered diffs opts' = filter (\opt -> opt `checkBlackWhiteList` diffs) opts'
--   where
--     checkBlackWhiteList :: Opt -> [D.Diff (TS.Tag PageSource)] -> Bool
--     checkBlackWhiteList opts (D.First d) = all (\opt -> d `withinBoundary` opt) diffs
--     checkBlackWhiteList opts (D.Second d) = all (\opt -> d `withinBoundary` opt) diffs
--     checkBlackWhiteList _    (D.Both _ _) = False
--
--     withinBoundary :: TS.Tag PageSource -> Opt -> Bool
--     withinBoundary d opt = case opt of BlackList Open name  -> not $ TS.isTagOpenName name d
--                                        WhiteList Open name  -> TS.isTagOpenName name d
--                                        BlackList Close name -> not $ TS.isTagCloseName name d
--                                        WhiteList Close name -> TS.isTagCloseName name d
--                                        _                    -> False

filtered :: Opts -> [D.Diff (TS.Tag PageSource)] -> [D.Diff (TS.Tag PageSource)]
filtered os dfs = filter(\df -> all (\o -> ok o df) os ) dfs
  where ok o df = case df of D.Both _ _ -> False
                             _          -> case o of BlackList Open name -> not $ TS.isTagOpenName name d
                                                     BlackList Close name -> not $ TS.isTagCloseName name d
                                                     BlackList TextRegex regex -> TS.isTagText d && (TS.fromTagText d =~ regex)
                                                     WhiteList Open name -> name == "*" || TS.isTagOpenName name d
                                                     where d = fromDiff df

-- filtered :: Opts -> [D.Diff (TS.Tag PageSource)] -> [D.Diff (TS.Tag PageSource)]
-- filtered os dfs = do
--   df <- dfs
--   o <- os
--   guard $ case df of D.Both _ _ -> False
--                      _          -> True
--   let d = fromDiff df
--   guard $ case o of BlackList Open name -> not $ TS.isTagOpenName name d
--                     BlackList Close name -> not $ TS.isTagCloseName name d
--                     BlackList TextRegex regex -> TS.isTagText d && (TS.fromTagText d =~ regex)
--                     WhiteList Open name -> TS.isTagOpenName name d
--   return df

fromDiff :: D.Diff (TS.Tag PageSource) -> TS.Tag PageSource
fromDiff (D.First a) = a
fromDiff (D.Second a) = a


-- for each diff:
--   all (diff /= b) blacklist
