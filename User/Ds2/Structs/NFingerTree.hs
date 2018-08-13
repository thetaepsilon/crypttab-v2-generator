{-# LANGUAGE MultiParamTypeClasses #-}

module User.Ds2.Structs.NFingerTree where

-- an n-ary finger tree, where branches can have zero or more elements;
-- a branch having at least two children is not enforced.
-- for the current purposes of StupidTests, this is not required.



-- A typeclass to adapt leaf nodes to the monoid used in the finger tree.
-- Whereas branches derive their values from their children
-- using the associative concat/combine operation,
-- leaves get their value directly from the leaf elements that they store.
class Monoid m => MonoidLeaf m l where
  measure :: l -> m


-- Define the N-ary tree on it's own.
-- we allow zero-length or one-length branch lists for now,
-- as for our purposes it matters little.
-- tree elements possess a tag and either children nodes or leaf value.
-- branches additionally have a metadata value -
-- this value doesn't take part in the monoidal operation.
data NTree t e l = NBranch t [NTree t e l] e | NLeaf t l
  deriving Show

-- a helper function to extract just the tag value.
tag :: NTree t e l -> t
tag (NBranch tag _ _) = tag
tag (NLeaf tag _) = tag



-- next, define some constructors for trees which only work when
-- the leaf type is a MonoidLeaf of a certain type.
-- note the tag type is the monoid; we will combine this later.
-- these smart constructors also take care of calculating the monoidal tag value.
fingerLeaf :: MonoidLeaf m l => l -> NTree m e l
fingerLeaf leaf =
  -- leaf nodes: "measure" the tag value from the leaf value.
  let tag = measure leaf
  in NLeaf tag leaf

fingerBranch :: MonoidLeaf m l => [NTree m e l] -> e -> NTree m e l
fingerBranch children extradata =
  -- branch nodes: use the associative op
  -- to combine all elements of the list.
  let
    -- collect the monoid tags and combine them.
    tagged = fmap tag children
    folded = mconcat tagged
  in NBranch folded children extradata



