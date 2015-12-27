module Key where

import Galois
import SBox
import GWord

type Key = GMatrix

expandKeyAES key = 
  let ws = expandKey key in
  case (length key) of
    4 -> take 44 ws
    6 -> take 52 ws
    8 -> take 60 ws

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
        w = (ws !! (nk-1)) + temp
        temp = case (i `mod` nk) of
          0 -> (subWord $ rotWord (head ws)) + (GW (rcon (i `div` nk)) 0 0 0)
          4 | nk > 6 -> subWord (head ws)
          otherwise -> (head ws)

subWord :: GWord -> GWord
subWord = gwmap sbox

rotWord :: GWord -> GWord
rotWord = gshift

rcon :: Int -> GF
rcon i = (2 :: GF) ^ (i-1)
