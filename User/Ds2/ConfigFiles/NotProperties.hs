module User.Ds2.ConfigFiles.NotProperties (Property) where
-- a simple string key-value text format,
-- one entry per line, separated by a single space:
-- foo 1
-- bar 2
-- baz some other key
-- # this is a comment, note that the values may have further spaces in them.
--
-- # comments and blank lines are ignored.
-- XXX: suppport multiple spaces/tabs?

import qualified User.Ds2.SplitOnce as Sp
-- (splitOnce, Split, SplitResult(Success, NoMatch))

-- A single property.
data Property = Property String String
  deriving Show

-- decision of what a single line should be.
data LineResult = Success Property | Ignore | NoSeparator
  deriving Show

handleLine :: String -> LineResult
-- empty lines ignored
handleLine [] = Ignore
-- comment lines ignored
handleLine ('#':cs) = Ignore
-- otherwise try to split
handleLine str =
  let result = Sp.splitOnce (== ' ') str
  in case result of
    Sp.Success (Sp.Split before after) -> Success (Property before after)
    Sp.NoMatch -> NoSeparator



-- process a list of lines to produce a list of properties.
-- note this stage does not handle duplicates.
-- we can't use a simple fmap,
-- as a line marked Ignore will produce zero entries.
linesToProperties_ :: Int -> [String] -> [Property]
linesToProperties_ _ [] = []
linesToProperties_ linenum (l:ls) =
  let
    parsed = handleLine l
    continue = linesToProperties_ (linenum + 1) ls
  in case parsed of
    (Success p) -> p:continue
    Ignore -> continue
    NoSeparator -> error ("line " ++ (show linenum) ++ 
      ": no separator found: " ++ show l)

linesToProperties = linesToProperties_ 1

fileToProperties :: String -> [Property]
fileToProperties file = linesToProperties (lines file)

