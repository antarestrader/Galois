module Round where

import Galois
import Key
import SBox

-- The State can be pictured as a rectangular array of bytes. This array has
-- four rows, the number of columns is denoted by Nb  and is equal to the block
-- length divided by 32.
 
type State = [GWord]

round :: State -> Key -> State
round s k =  addRoundKey k $ mixColumns $ shiftRow $ byteSub s

round' :: State -> Key -> State
round' s k = invMixColumns $ addRoundKey k $ invSubBytes $ invShiftRows s

finalRound :: State -> Key -> State
finalRound s k =  addRoundKey k $ shiftRow $ byteSub s

finalRound' :: State -> Key -> State
finalRound' s k = addRoundKey k $ invSubBytes $ invShiftRows s

byteSub :: State -> State
byteSub s = map (gwmap sbox) s

invSubBytes s = map (gwmap invsbox) s

shiftRow :: State -> State
shiftRow = undefined

invShiftRows = undefined

mixColumns :: State -> State
mixColumns  = map (gwmult ax)
  where ax = ((GF 0x03),(GF 0x01), (GF 0x01), (GF 0x02))  -- Equation (5.5)

invMixColumns = map (gwmult ax)
  where  ax = ((GF 0x0b),(GF 0x0d), (GF 0x09), (GF 0x0e))  -- Equation (5.9)

addRoundKey :: Key -> State -> State
addRoundKey = zipWith gwxor
