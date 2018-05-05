-- |Sure tree comparisons.
--
-- Essential to several operations is the comparison of trees.  Since
-- the trees are streamed through Pipes, it takes special care to
-- compare them.  This module contains 'twoTrees', which takes an
-- "old" and a "new" tree, and calls various attachments across the
-- two trees.

module Sure.Compare where
