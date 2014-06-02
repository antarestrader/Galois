{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Galois where

import Data.Bits
import Data.Word
import Numeric

-- Implimentation of GF(2^8) field

newtype GF = GF { getByte :: Word8 }
  deriving (Eq, Ord, Bounded, Enum, Bits)

instance Show GF where
  showsPrec _ n s = '{': (showHex (getByte n `shiftR` 4) $ (showHex (getByte n .&. 0xf)) $ '}' : s) 

a `g_xor` b = GF $ getByte a `xor` getByte b

a `g_mult` b = peasants a b 0
  where
    -- peasants algorithm (http://en.wikipedia.org/wiki/Finite_field_arithmetic#Multiplication)
    -- CAUTION: it is possible to leak side channel information because this may terminate early
    peasants 0 _ p = p
    peasants _ 0 p = p
    peasants a b p  = peasants a'  b' p'
      where
        -- shift a left if the left most bit was on CARRY (ie xor 0x1b)
        a' = if (a `testBit` 7) then (a `shiftL` 1 - 0x1b) else (a `shiftL` 1)
        -- shift b to the right
        b' = b `shiftR` 1
        -- if the right most bit of b is set add a to p
        p' = if (b `testBit` 0) then  (p + a) else p


instance Num GF where
  (+) = g_xor
  (-) = g_xor
  (*) = g_mult
  abs = id
  signum _ = 1
  fromInteger = GF . fromInteger

inv :: GF -> GF
inv 0 = 0
inv n = n ^ 254
