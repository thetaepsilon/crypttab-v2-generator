module User.Ds2.Types.BoolMonoids where

-- various possible monoids for Bool,
-- using the various binary boolean operators for the associative operation;
-- e.g. and, or and others.

-- firstly, we want a unique type which maps to Bool,
-- which we them implement the semigroup/monoid for.
-- this is analogous to there being different monoid canditates for number types;
-- you have addition (wrapped type in the Prelude: "Sum")
-- and multiplication ("Product") as candidates.
-- for bool, we could have AND, OR, and others.
newtype Conjunction = Conjunction Bool
instance Show Conjunction where
  -- I'm not a fan of the derived show reading "Conjunction True"
  show (Conjunction e) = show e

instance Semigroup Conjunction where
  (<>) (Conjunction e1) (Conjunction e2) = Conjunction (e1 && e2)
-- identity element for AND operation is True -
-- True and False is False, True and True is True.
instance Monoid Conjunction where
  mempty = Conjunction True

