module Main where

import Math.NumberTheory.Powers.Modular

main = do
  print $ powMod doorPubKey cardLoopSize modulus

cardPubKey = 6270530
doorPubKey = 14540258
modulus = 20201227

cardLoopSize = head [i | i <- [3..], powMod 7 i modulus == cardPubKey]
doorLoopSize = head [i | i <- [3..], powMod 11 i modulus == cardPubKey]
