{-  stackcollapse-ghc - fold GHC prof files into flamegraph input
    Copyright (C) 2020 Marcin Rzeźnicki

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
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