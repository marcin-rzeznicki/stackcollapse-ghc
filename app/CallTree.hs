{-# LANGUAGE LambdaCase #-}

module CallTree (CallTree(..), insertTrace, CallForest, extractForest) where

import           Trace
import           Data.Tree.Zipper
import           Data.Tree

data CallTree = MkTree !Int (TreePos Full Trace)
              | EmptyTree

insertTrace :: Trace -> Int -> CallTree -> Either String CallTree
insertTrace trace 0 EmptyTree = Right $ MkTree 0 $ fromTree $ Node trace []
insertTrace _ _ EmptyTree = Left "the first line should not be indented"
insertTrace trace level (MkTree depth cursor)
  | level == succ depth = Right $ MkTree level $ insertChild cursor
  | level <= depth = MkTree level <$> insertSibling (depth - level) cursor
  | otherwise = Left "invalid indentation"
  where
    newNode = Node trace []

    insertChild = insert newNode . children

    insertSibling 0 = Right . insert newNode . nextSpace
    insertSibling up = (\case
                          Just parent' -> insertSibling (up - 1) parent'
                          Nothing      -> Left "bug: no parent")
      . parent

type CallForest = Forest Trace

extractForest :: CallTree -> CallForest
extractForest EmptyTree = []
extractForest (MkTree _ cursor) = toForest cursor
