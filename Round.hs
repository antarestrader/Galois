module Round where

import Galois
import Key
import SBox

-- The State can be pictured as a rectangular array of bytes. This array has
-- four rows, the number of columns is denoted by Nb  and is equal to the block
-- length divided by 32.
 
type State = [GWord]

testState = [(1,1,1,1),(2,2,2,2),(3,3,3,3),(4,4,4,4)] :: State

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
shiftRow s = s'
  where
    [as,bs,cs,ds] = rowMajor s
    s' = columnMajor [as, rotate 1 bs, rotate 2 cs, rotate 3 ds]

invShiftRows s = s'
    where
    [as,bs,cs,ds] = rowMajor s
    s' = columnMajor [as, rotate (-1) bs, rotate (-2) cs, rotate (-3) ds]

mixColumns :: State -> State
mixColumns  = map (gwmult ax)
  where ax = ((GF 0x03),(GF 0x01), (GF 0x01), (GF 0x02))  -- Equation (5.5)

invMixColumns = map (gwmult ax)
  where  ax = ((GF 0x0b),(GF 0x0d), (GF 0x09), (GF 0x0e))  -- Equation (5.9)

addRoundKey :: Key -> State -> State
addRoundKey = zipWith gwxor

rowMajor :: State -> [[GF]]
rowMajor xs = aux xs []
  where
    aux :: [GWord] -> [[GF]] -> [[GF]]
    aux [] ys = map reverse ys
    aux ((a,b,c,d):xs) [] = aux xs [[a],[b],[c],[d]]
    aux ((a,b,c,d):xs) [as,bs,cs,ds] = aux xs [a:as,b:bs,c:cs,d:ds]

rotate :: Int -> [a] -> [a]
rotate n xs | n < 0 = rotate (l + n) xs
  where 
    l = length xs
rotate n xs = b ++ a
 where
   (a,b) = splitAt n xs

columnMajor :: [[GF]] -> State
columnMajor xs = aux xs []
  where
    aux [[],[],[],[]] ys = reverse ys
    aux [a:as,b:bs,c:cs,d:ds] ys = aux [as,bs,cs,ds] ((a,b,c,d):ys)
    aux _ _ = []  -- so we have a total function
