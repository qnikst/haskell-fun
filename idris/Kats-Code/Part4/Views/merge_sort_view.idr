import Data.List.Views

merge_sort : Ord a => List a -> List a
merge_sort xs with (splitRec xs)
  merge_sort xs | with_pat = ?merge_sort_rhs
