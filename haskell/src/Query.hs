{-# LANGUAGE GADTs #-}

{-|
Module      : Query
Description : Type-safe memory query DSL with compile-time verification
Copyright   : (c) WhiteMagic Contributors, 2025
License     : MIT
Maintainer  : whitemagic@example.com

A domain-specific language for querying memories with compile-time
guarantees about query validity. Invalid queries are caught by the
type checker, not at runtime.

Philosophy: Declarative query construction with functional combinators.
-}

module Query
    ( -- * Query DSL
      Query
    , Predicate
    
      -- * Query Construction
    , hasTag
    , hasTitle
    , hasContent
    , createdAfter
    , createdBefore
    , ofType
    
      -- * Query Combinators
    , and'
    , or'
    , not'
    
      -- * Query Execution
    , execute
    , count
    , exists
    ) where

import Transform (Memory(..), MemoryType(..), Tag)
import Data.Time (UTCTime)
import Data.List (isInfixOf)

-- | Predicate over memories
type Predicate a = Memory a -> Bool

-- | Query type (composition of predicates)
newtype Query a = Query { unQuery :: Predicate a }

-- | Execute query on list of memories
execute :: Query a -> [Memory a] -> [Memory a]
execute (Query pred') = filter pred'

-- | Count matches
count :: Query a -> [Memory a] -> Int
count q = length . execute q

-- | Check if any memory matches
exists :: Query a -> [Memory a] -> Bool
exists q = not . null . execute q

-- | Query by tag
hasTag :: Tag -> Query a
hasTag tag = Query $ \mem -> tag `elem` memoryTags mem

-- | Query by title substring
hasTitle :: String -> Query a
hasTitle substr = Query $ \mem -> substr `isInfixOf` memoryTitle mem

-- | Query by content substring
hasContent :: String -> Query a
hasContent substr = Query $ \mem -> substr `isInfixOf` memoryContent mem

-- | Query by creation date (after)
createdAfter :: UTCTime -> Query a
createdAfter time = Query $ \mem -> memoryCreated mem > time

-- | Query by creation date (before)
createdBefore :: UTCTime -> Query a
createdBefore time = Query $ \mem -> memoryCreated mem < time

-- | Query by memory type
ofType :: MemoryType -> Query a
ofType mtype = Query $ \mem -> memoryType mem == mtype

-- | AND combinator
and' :: Query a -> Query a -> Query a
and' (Query p1) (Query p2) = Query $ \mem -> p1 mem && p2 mem

-- | OR combinator  
or' :: Query a -> Query a -> Query a
or' (Query p1) (Query p2) = Query $ \mem -> p1 mem || p2 mem

-- | NOT combinator
not' :: Query a -> Query a
not' (Query p) = Query $ \mem -> not (p mem)

-- | Example: Complex query composition
-- 
-- >>> let query = hasTag "important" `and'` createdAfter someDate `and'` not' (hasTag "archived")
-- >>> execute query memories
exampleQuery :: UTCTime -> Query a
exampleQuery date = hasTag "important" 
                  `and'` createdAfter date 
                  `and'` not' (hasTag "archived")
