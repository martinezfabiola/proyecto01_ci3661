-- Universidad Simón Bolívar
-- CI3661: Programming Languages I
--
-- First Project Haskell
--
-- Authors:
--
-- David Cabeza <13-10191@usb.ve>
-- Fabiola Martínez <13-10838@usb.ve>
--
-- February, 2018.

import Data.Char

-- converts an integer to its binary representation
toBinary :: Int -> [Int]
toBinary 0 = []
toBinary x = toBinary (x `div` 2) ++ [x `rem` 2]

-- verify if a list contains seven elements
verifyBinary :: [Int]-> [Int]
verifyBinary x
 | length x == 6 = 0:x
 | length x == 5 = [0,0] ++ x
 | length x == 4 = [0,0,0] ++ x
 | length x == 3 = [0,0,0,0] ++ x
 | length x == 2 = [0,0,0,0,0] ++ x
 | length x == 1 = [0,0,0,0,0,0] ++ x
 | length x == 0 = [0,0,0,0,0,0,0,0]
 | otherwise = take 7 x

-- coonverts an hexadecimal list into bit list
applyToBinary :: [Int] -> [[Int]]
applyToBinary letterBitMap  = 
    let applyVerifyBinary = map toBinary letterBitMap
    in map verifyBinary applyVerifyBinary

-- converts the whole hexadecimal list into bit list 
getBitList :: [[Int]] -> [[[Int]]]
getBitList [] = error"No se puede crear lista vacia"
getBitList hexList = map applyToBinary hexList

-- pairs list of char and bit list 
diccBitChar ::  [Char] -> [[[Int]]] -> [(Char,[[Int]])]
diccBitChar [] [] = error"Insertar lista"
diccBitChar [] bitList = error"Insertar lista"
diccBitChar charList  [] = error"Insertar lista"
diccBitChar charList bitList = zip charList bitList 




