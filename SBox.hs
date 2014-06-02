module SBox where

import Galois
import Data.Vector
import Data.Bits

sBox = generate 256 (affine . inv . fromIntegral)

affine :: GF -> GF
affine n= (loop n n 4) + 0x63
  where
    loop _ x 0 = x
    loop s x i = 
      let s' = s `rotateL` 1
          x' = x + s'
          i' = i-1
      in  loop s' x' i' 

sbox :: GF->GF
sbox i = sBox ! (fromIntegral $ getByte i)

-- sbox_inv = generate 256
