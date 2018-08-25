module User.Ds2.ConfigFiles.NotProperties (Property) where

import qualified Data.Map.Strict as Map
import Data.Set (Set, member, insert, empty)
import User.Ds2.FoldEither

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
propKey :: Property -> String
propKey (Property k v) = k

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



-- Populate a client-defined data structure using a list of properties.
-- The data structure is updated by a map of functions,
-- which take the current structure and produce an updated copy,
-- given the input property.
-- Property names are defined in a map of strings to functions;
-- the string key is determined by the property key,
-- and an invalid key causes the load to fail.
-- The operation enforces that each property may only appear once,
-- however they may appear in any order.

-- type of a config updater:
-- either returns the successfully updated config structure,
-- or an error object describing some error with the property's contents.
data ConfigUpdater cfg err = ConfigUpdater (cfg -> Property -> Either err cfg)
fn :: ConfigUpdater cfg err -> (cfg -> Property -> Either err cfg)
fn (ConfigUpdater f) = f

-- property updater map
type UpdaterMap cfg err = Map.Map String (ConfigUpdater cfg err)

data SimplePropertiesResult cfg err = Config cfg | ValueError err | UnknownKey String | Duplicate String



loadSimpleProperties :: UpdaterMap cfg err -> cfg -> [Property] -> SimplePropertiesResult cfg err
-- we use FoldEither here to do the error handling for us.
-- Firstly, we map by extracting handler functions from the updater map;
-- if we don't find any, that's a mapping error.
-- otherwise, produce both the input property and the corresponding function;
-- this is to allow the folding step to catch duplicate entries.



type FoundHandler cfg err = (Property, ConfigUpdater cfg err)
type MissingKey = String
lookupHandler :: UpdaterMap cfg err -> Property -> Either MissingKey (FoundHandler cfg err)
lookupHandler map prop =
  let key = propKey prop
  in case (Map.lookup key map) of
    Just cfgu -> Right (prop, cfgu)
    Nothing -> Left key

-- next up is the fold stage.
-- the folded state maintains a Set of property names seen from previous Properties in the list,
-- as well as the current state of the config structure.
-- the input, being a tuple of property and config updater function,
-- is checked against this set - if it's already there, then reject it.
-- this way we can enforce the constraint that a given property only appears once in the input.
data SimpleFoldState cfg = SimpleFoldState cfg (Set String)
data FoldError err = DuplicateKey String | ParseError err

foldHandler :: SimpleFoldState cfg -> FoundHandler cfg err -> Either (FoldError err) (SimpleFoldState cfg)
foldHandler (SimpleFoldState current seenkeys) (prop, updater) =
  let key = propKey prop
  in if member key seenkeys
    then Left (DuplicateKey key)  -- duplicate entry, raise error
    else  -- try to apply the updater
      case (fn updater current prop) of
        Left err -> Left (ParseError err)
        Right newcfg ->
          -- configuration successfully updated.
          -- make a note of this property key so it can't be inserted again.
          let newset = insert key seenkeys
          in Right (SimpleFoldState newcfg newset)



-- lift the result of FoldEither into our externally visible result type,
-- so that FoldResult isn't part of our API.
type PropertiesFoldResult cfg err = FoldResult (SimpleFoldState cfg) MissingKey (FoldError err)
liftToSimpleResult :: PropertiesFoldResult cfg err -> SimplePropertiesResult cfg err
liftToSimpleResult fr = case fr of
  Result (SimpleFoldState cfg seenset) -> Config cfg
  MapFail missing -> UnknownKey missing
  FoldFail o -> case o of
    DuplicateKey key -> Duplicate key
    ParseError err -> ValueError err



-- with all that done, we can now bind loadSimpleProperties
loadSimpleProperties keymap initial inputprops =
  -- the initial seen set for an input will be empty.
  let initialState = SimpleFoldState initial empty
  in liftToSimpleResult (foldEitherL foldHandler (lookupHandler keymap) initialState inputprops)

