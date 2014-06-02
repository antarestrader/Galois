module Key where

import Galois
import SBox

type GWord = (GF,GF,GF,GF)

gwxor :: GWord -> GWord -> GWord
gwxor (i0,i1,i2,i3) (j0,j1,j2,j3) = (i0+j0,i1+j1,i2+j2,i3+j3)

gwmap :: (GF -> GF) -> GWord -> GWord
gwmap f (a,b,c,d) = (f a, f b, f c, f d)

gshift (a, b, c, d) = (b, c, d, a) 

type Key128 = [GWord]

expandKey128 :: Key128 -> [GWord]
expandKey128 key = key ++ expand key 1
  where
    expand ws i = 
      let ws' = scanl' gwxor (core (last ws) i) ws
      in ws' ++ expand ws' (i + 1)

-- core :: GWord ->  GWord -> Int
core v i = gwmap sbox (gshift v) `gwxor` (rcon i, 0, 0, 0)

rcon :: Int -> GF
rcon i = (2 :: GF) ^ (i-1)

scanl' f init xs = tail $ scanl f init xs


