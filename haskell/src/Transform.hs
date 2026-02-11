{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

{-|
Module      : Transform
Description : Type-safe memory transformations with compile-time guarantees
Copyright   : (c) WhiteMagic Contributors, 2025
License     : MIT
Maintainer  : whitemagic@example.com

Type-safe transformations ensure that invalid memory operations are caught
at compile time, not runtime. Haskell's type system provides mathematical
guarantees about transformation correctness.

Philosophy: "Make illegal states unrepresentable" - Invalid transformations
cannot be expressed in the type system.
-}

module Transform
    ( -- * Memory Types
      MemoryType(..)
    , Memory(..)
    , Tag
    
      -- * Type-Safe Transformations
    , Transform
    , apply
    , compose
    , identity
    
      -- * Common Transformations
    , addTag
    , removeTag
    , promote
    , archive
    , consolidate
    
      -- * Validation
    , isValid
    , validate
    ) where

import Data.List (nub, (\\))
import Data.Time (UTCTime, getCurrentTime)

-- | Memory type with phantom type for compile-time safety
data MemoryType = ShortTerm | LongTerm | Archived
    deriving (Eq, Show)

-- | Tag type alias
type Tag = String

-- | Memory record with type parameter
data Memory (t :: MemoryType) = Memory
    { memoryId      :: String
    , memoryTitle   :: String
    , memoryContent :: String
    , memoryTags    :: [Tag]
    , memoryCreated :: UTCTime
    , memoryType    :: MemoryType
    } deriving (Show, Eq)

-- | Type-safe transformation from one memory type to another
-- GADT ensures transformations preserve type safety
data Transform a b where
    -- Identity transformation
    Identity :: Transform a a
    
    -- Compose two transformations
    Compose :: Transform b c -> Transform a b -> Transform a c
    
    -- Primitive transformations
    AddTag    :: Tag -> Transform a a
    RemoveTag :: Tag -> Transform a a
    
    -- Type-changing transformations
    Promote :: Transform 'ShortTerm 'LongTerm
    Archive :: Transform a 'Archived
    
    -- Consolidation (multiple memories -> one)
    -- This is more complex and would need refinement
    Consolidate :: [Memory a] -> Transform a 'LongTerm

-- | Apply transformation to memory
apply :: Transform a b -> Memory a -> IO (Memory b)
apply Identity mem = return mem

apply (Compose f g) mem = do
    mem' <- apply g mem
    apply f mem'

apply (AddTag tag) mem = return $ mem
    { memoryTags = nub (tag : memoryTags mem)
    }

apply (RemoveTag tag) mem = return $ mem
    { memoryTags = memoryTags mem \\ [tag]
    }

apply Promote mem = return $ Memory
    { memoryId = memoryId mem
    , memoryTitle = memoryTitle mem
    , memoryContent = memoryContent mem ++ "\n\n*Auto-promoted to long-term*"
    , memoryTags = "promoted" : memoryTags mem
    , memoryCreated = memoryCreated mem
    , memoryType = LongTerm
    }

apply Archive mem = return $ Memory
    { memoryId = memoryId mem
    , memoryTitle = memoryTitle mem
    , memoryContent = memoryContent mem
    , memoryTags = "archived" : memoryTags mem
    , memoryCreated = memoryCreated mem
    , memoryType = Archived
    }

apply (Consolidate memories) mem = do
    now <- getCurrentTime
    return $ Memory
        { memoryId = "consolidated_" ++ memoryId mem
        , memoryTitle = "Consolidated: " ++ memoryTitle mem
        , memoryContent = consolidateContent memories
        , memoryTags = "consolidated" : consolidatedTags memories
        , memoryCreated = now
        , memoryType = LongTerm
        }
  where
    consolidateContent :: [Memory a] -> String
    consolidateContent mems = unlines
        [ "# Consolidated Memories\n"
        , unlines [memoryTitle m ++ ":\n" ++ memoryContent m | m <- mems]
        ]
    
    consolidatedTags :: [Memory a] -> [Tag]
    consolidatedTags mems = nub $ concatMap memoryTags mems

-- | Compose transformations (right to left, like function composition)
compose :: Transform b c -> Transform a b -> Transform a c
compose = Compose

-- | Identity transformation
identity :: Transform a a
identity = Identity

-- | Add tag transformation
addTag :: Tag -> Transform a a
addTag = AddTag

-- | Remove tag transformation
removeTag :: Tag -> Transform a a
removeTag = RemoveTag

-- | Promote short-term to long-term
promote :: Transform 'ShortTerm 'LongTerm
promote = Promote

-- | Archive any memory
archive :: Transform a 'Archived
archive = Archive

-- | Consolidate memories
consolidate :: [Memory a] -> Transform a 'LongTerm
consolidate = Consolidate

-- | Validate memory structure
isValid :: Memory a -> Bool
isValid mem = not (null (memoryTitle mem)) && not (null (memoryContent mem))

-- | Validate and return Either
validate :: Memory a -> Either String (Memory a)
validate mem
    | null (memoryTitle mem) = Left "Memory title cannot be empty"
    | null (memoryContent mem) = Left "Memory content cannot be empty"
    | otherwise = Right mem
