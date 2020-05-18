{-# LANGUAGE TypeFamilies #-}

module Orphans (TreeF(..)) where

import           Data.Tree (Tree(..))
import           Data.Functor.Foldable

data TreeF a r = NodeF a [r]

instance Functor (TreeF a) where
  fmap f (NodeF a rs) = NodeF a (map f rs)

type instance Base (Tree a) = TreeF a

instance Recursive (Tree a) where
  project (Node a sub) = NodeF a sub

instance Corecursive (Tree a) where
  embed (NodeF a rs) = Node a rs