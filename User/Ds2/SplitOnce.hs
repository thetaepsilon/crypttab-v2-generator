-- split a list once the first time a predicate returns true.
-- returns Nothing if no such match was found.
-- returned list fragments does not include the indicated element.

module User.Ds2.SplitOnce (splitOnce, Split(Split), SplitResult(Success, NoMatch)) where

-- tuple of parts before and after the matched element.
data Split a = Split [a] [a]
  deriving Show 
data SplitResult a = Success (Split a) | NoMatch
  deriving Show

-- inner function helper to append peeled char to inner invocation.
-- here mainly because trying to fit the proper type constraint inline was horrible.
unpeel :: a -> Split a -> Split a
unpeel e (Split before after) =
  Split (e:before) after

splitOnce :: (a -> Bool) -> [a] -> SplitResult a
-- passed a zero length list to begin with, evidently there can be no match.
splitOnce _ [] = NoMatch
-- passed a list with at least some elements.
-- if the first element is the match, then the rest of the list is the remainder.
-- otherwise, continue down the list.
splitOnce f (a:as) =
  if (f a)
    then Success (Split [] as)
    else
      -- process the rest of the list,
      -- then re-cons the character we peeled off here if needed.
      let
        inner = splitOnce f as
        l = (\x -> unpeel a x)
      in case inner of
        Success s -> Success (l s)
        NoMatch -> NoMatch

