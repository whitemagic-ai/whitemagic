{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module      : FFI
Description : Foreign Function Interface for Python bindings
Copyright   : (c) WhiteMagic Contributors, 2025
License     : MIT
Maintainer  : whitemagic@example.com

C FFI bindings to expose Haskell functions to Python via ctypes.
This allows Python code to call type-safe Haskell transformations.

Philosophy: Best of both worlds - Python flexibility + Haskell correctness.
-}

module FFI
    ( -- * Exported Functions
      c_create_hexagram
    , c_transition_hexagram
    , c_hexagram_to_number
    , c_is_balanced_hexagram
    
      -- * Deep Dharma (Phase 14.5)
    , c_hexagram_element_index
    , c_resonance_score
    
      -- * Helper Functions
    , initFFI
    ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import IChing
import WuXing

-- | Initialize FFI (called once from Python)
foreign export ccall initFFI :: IO ()
initFFI :: IO ()
initFFI = putStrLn "WhiteMagic Haskell FFI initialized"

-- | Create hexagram from 6 integers (0=Yin, 1=Yang)
foreign export ccall c_create_hexagram :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Hexagram)
c_create_hexagram :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO (Ptr Hexagram)
c_create_hexagram l1 l2 l3 l4 l5 l6 = do
    let hex = hexagram 
            (intToLine l1) (intToLine l2) (intToLine l3)
            (intToLine l4) (intToLine l5) (intToLine l6)
    hexPtr <- malloc
    poke hexPtr hex
    return hexPtr
  where
    intToLine :: CInt -> Line
    intToLine 0 = Yin
    intToLine _ = Yang

-- | Transition hexagram (change lines at positions)
foreign export ccall c_transition_hexagram :: Ptr Hexagram -> Ptr CInt -> CInt -> IO (Ptr Hexagram)
c_transition_hexagram :: Ptr Hexagram -> Ptr CInt -> CInt -> IO (Ptr Hexagram)
c_transition_hexagram hexPtr posPtr numPos = do
    hex <- peek hexPtr
    positions <- peekArray (fromIntegral numPos) posPtr
    let newHex = transition (map fromIntegral positions) hex
    newHexPtr <- malloc
    poke newHexPtr newHex
    return newHexPtr

-- | Convert hexagram to number (1-64)
foreign export ccall c_hexagram_to_number :: Ptr Hexagram -> IO CInt
c_hexagram_to_number :: Ptr Hexagram -> IO CInt
c_hexagram_to_number hexPtr = do
    hex <- peek hexPtr
    return $ fromIntegral $ toNumber hex

-- | Check if hexagram is balanced
foreign export ccall c_is_balanced_hexagram :: Ptr Hexagram -> IO CInt
c_is_balanced_hexagram :: Ptr Hexagram -> IO CInt
c_is_balanced_hexagram hexPtr = do
    hex <- peek hexPtr
    return $ if isBalanced hex then 1 else 0

-- | Get the element index (0-4) governing the hexagram
foreign export ccall c_hexagram_element_index :: Ptr Hexagram -> IO CInt
c_hexagram_element_index :: Ptr Hexagram -> IO CInt
c_hexagram_element_index hexPtr = do
    hex <- peek hexPtr
    return $ fromIntegral $ fromEnum $ hexagramElement hex

-- | Calculate resonance score (0.0 - 1.2) against a WuXing state
-- wStatePtr must point to an array of 5 doubles [Wood, Fire, Earth, Metal, Water]
foreign export ccall c_resonance_score :: Ptr Hexagram -> Ptr CDouble -> IO CDouble
c_resonance_score :: Ptr Hexagram -> Ptr CDouble -> IO CDouble
c_resonance_score hexPtr wStatePtr = do
    hex <- peek hexPtr
    [w, f, e, m, wa] <- peekArray 5 wStatePtr
    let state = WuXingState (realToFrac w) (realToFrac f) (realToFrac e) (realToFrac m) (realToFrac wa)
    return $ realToFrac $ resonanceScore hex state

-- | Free hexagram memory
foreign export ccall c_free_hexagram :: Ptr Hexagram -> IO ()
c_free_hexagram :: Ptr Hexagram -> IO ()
c_free_hexagram = free

-- Helper to peek array from C
peekArray :: Storable a => Int -> Ptr a -> IO [a]
peekArray n ptr = mapM (peekElemOff ptr) [0..n-1]

-- | Make Hexagram storable for FFI
instance Storable Hexagram where
    sizeOf _ = 2 * sizeOf (undefined :: Trigram)
    alignment _ = alignment (undefined :: Trigram)
    
    peek ptr = do
        u <- peekByteOff ptr 0
        l <- peekByteOff ptr (sizeOf (undefined :: Trigram))
        return $ Hexagram u l
    
    poke ptr (Hexagram u l) = do
        pokeByteOff ptr 0 u
        pokeByteOff ptr (sizeOf (undefined :: Trigram)) l

instance Storable Trigram where
    sizeOf _ = 3 * sizeOf (undefined :: Line)
    alignment _ = alignment (undefined :: Line)
    
    peek ptr = do
        l1 <- peekByteOff ptr 0
        l2 <- peekByteOff ptr (sizeOf (undefined :: Line))
        l3 <- peekByteOff ptr (2 * sizeOf (undefined :: Line))
        return $ Trigram l1 l2 l3
    
    poke ptr (Trigram l1 l2 l3) = do
        pokeByteOff ptr 0 l1
        pokeByteOff ptr (sizeOf (undefined :: Line)) l2
        pokeByteOff ptr (2 * sizeOf (undefined :: Line)) l3

instance Storable Line where
    sizeOf _ = sizeOf (undefined :: CInt)
    alignment _ = alignment (undefined :: CInt)
    
    peek ptr = do
        val <- peek (castPtr ptr :: Ptr CInt)
        return $ if val == 0 then Yin else Yang
    
    poke ptr Yin = poke (castPtr ptr :: Ptr CInt) 0
    poke ptr Yang = poke (castPtr ptr :: Ptr CInt) 1
