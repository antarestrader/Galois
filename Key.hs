module Key where

import Galois
import SBox

type GWord = (GF,GF,GF,GF)

-- could also be called gwadd
gwxor :: GWord -> GWord -> GWord
gwxor (i0,i1,i2,i3) (j0,j1,j2,j3) = (i0+j0,i1+j1,i2+j2,i3+j3)

gwmap :: (GF -> GF) -> GWord -> GWord
gwmap f (a,b,c,d) = (f a, f b, f c, f d)

gshift (a, b, c, d) = (b, c, d, a)

-- using equation (4.12) of the AES Stanfard (page 17)
gwmult :: GWord -> GWord -> GWord
gwmult (a0,a1,a2,a3) (b0,b1,b2,b3) = (d0,d1,d2,d3)
  where
    d0 = (a0 * b0) + (a3*b1) + (a2*b2) + (a1*b3)
    d1 = (a1 * b0) + (a0*b1) + (a3*b2) + (a2*b3)
    d2 = (a2 * b0) + (a1*b1) + (a0*b2) + (a3*b3)
    d3 = (a3 * b0) + (a2*b1) + (a1*b2) + (a0*b3)

type Key = [GWord]

expandKey :: Key -> [GWord]
expandKey key = key ++ expand (reverse key) nk nk
  where  -- NB we set i = to the key length initially because the key itself is
         -- the initial expansion.
    nk = length key
    expand :: [GWord] -- the current expansion in reverse order 
           -> Int -- the number of expansions performed to this point
           -> Int -- Nk the key length in bytes (constant)
           -> [GWord] -- an infinite list of GWords 
    expand ws i nk = (w : expand (w:ws) (i+1) nk)
      where
        w = (ws !! (nk-1)) `gwxor` temp
        temp = case (i `mod` nk) of
          0 -> (subWord $ rotWord (head ws)) `gwxor` (rcon (i `div` nk),0,0,0)
          4 | nk > 6 -> subWord (head ws)
          otherwise -> (head ws)

subWord :: GWord -> GWord
subWord = gwmap sbox

rotWord :: GWord -> GWord
rotWord = gshift

rcon :: Int -> GF
rcon i = (2 :: GF) ^ (i-1)

