{-# LANGUAGE BangPatterns #-}

module GWord where

import Data.List
import Numeric
import Galois

data GWord = GW !GF !GF !GF !GF deriving Eq

gwxor :: GWord -> GWord -> GWord
gwxor (GW i0 i1 i2 i3) (GW j0 j1 j2 j3) = GW (i0+j0) (i1+j1) (i2+j2) (i3+j3)

gwmap :: (GF -> GF) -> GWord -> GWord
gwmap f (GW a b c d) = (GW (f a) (f b) (f c) (f d))

gshift (GW a b c d) = (GW b c d a)

-- using equation (4.12) of the AES Stanfard (page 17)
gwmult :: GWord -> GWord -> GWord
gwmult (GW a0 a1 a2 a3) (GW b0 b1 b2 b3) = (GW d0 d1 d2 d3)
  where
    d0 = (a0 * b0) + (a3*b1) + (a2*b2) + (a1*b3)
    d1 = (a1 * b0) + (a0*b1) + (a3*b2) + (a2*b3)
    d2 = (a2 * b0) + (a1*b1) + (a0*b2) + (a3*b3)
    d3 = (a3 * b0) + (a2*b1) + (a1*b2) + (a0*b3)

type GMatrix = [GWord]
type GRowMatrix = ([GF],[GF],[GF],[GF])

instance Show GWord where
  show (GW a b c d) = "[" ++ intercalate ", " [show a, show b, show c, show d] ++ "]"

instance Num GWord where
  (+) = gwxor
  (-) = gwxor
  (*) = gwmult
  abs = id
  signum _ = 1
  fromInteger i = undefined

instance Read GWord where
  readsPrec _ s | length s < 8 = []
  readsPrec _ s = [(gw,rest)]
    where
      (ws,rest) = splitAt 8 s
      (a,bcd) = splitAt 2 ws
      (b,cd) = splitAt 2 bcd
      (c,d) = splitAt 2 cd
      rw = fst . head . readHex
      gw = (GW (rw a) (rw b) (rw c) (rw d))
