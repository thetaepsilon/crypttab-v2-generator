module User.Ds2.FoldEither where

-- a combination of fmap and fold that allows signalling error conditions.
-- for each item in a list,
-- first a map function is applied, returning an Either,
-- and a Left (error) will cause the entire fold to return a MapFail.
-- Right's then get passed to the folding function with state as usual;
-- this also returns an Either and a Left causes the fold to return FoldFail.
-- otherwise, the fold continues, repeating these steps for each element;
-- if no errors result, returns Result.

data FoldResult d a o = Result d | MapFail a | FoldFail o
  deriving (Show, Eq)

-- d: fold state
-- i: input element type before map
-- e: element type after map
-- a: map error type
-- o: fold error type
foldEitherL :: (d -> e -> Either o d) -> (i -> Either a e) -> d -> [i] -> FoldResult d a o

-- list empty base case - as usual, can't do anything here.
foldEitherL _ _ initial [] = Result initial
-- got elements?
foldEitherL foldf mapf initial (i:is) =
  -- firstly try to map the input - if that fails, short circuit return
  let mapr = mapf i
  in case mapr of
    Left mfail -> MapFail mfail
    -- next, try to fold into the accumulated value
    Right mapped ->
      let foldres = foldf initial mapped
      in case foldres of
        -- again, if it fails, short circuit
        Left ffail -> FoldFail ffail
        -- otherwise, pass on to rest of processing of list.
        Right folded -> foldEitherL foldf mapf folded is

