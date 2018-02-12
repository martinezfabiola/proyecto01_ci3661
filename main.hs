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

-- converts an integer to its binary representation
toBinary :: Int-> [Int]
toBinary 0 = []
toBinary x = toBinary (x `div` 2) ++ [x `rem` 2]

verifyBinary :: [Int] -> [Int]
| 