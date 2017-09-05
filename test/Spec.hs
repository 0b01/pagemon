import Diff
import Lib
import qualified Data.Algorithm.Diff as D
import qualified Data.Algorithm.DiffOutput as D
import qualified Text.HTML.TagSoup as TS
import qualified Data.ByteString.Lazy.Char8 as BSL

os = [BlackList Open (BSL.pack "meta")]
regex = [BlackList TextRegex (BSL.pack "test")]

dfs = D.getDiff
      (TS.parseTags $ BSL.pack "<meta class='test'>")
      (TS.parseTags $ BSL.pack "<meta class='blah'>")

dfs' = D.getDiff
      (TS.parseTags $ BSL.pack "<test class='test'>")
      (TS.parseTags $ BSL.pack "<test class='blah'>")
dfsRegex = D.getDiff
      (TS.parseTags $ BSL.pack "text")
      (TS.parseTags $ BSL.pack "test")

test = filtered os dfs
test' = filtered os dfs'

main :: IO ()
main = do
  print test
  print test'
