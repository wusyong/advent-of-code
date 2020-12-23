{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Map as Map
import Data.Maybe ( fromJust )
import Data.Bifunctor as Bf ( Bifunctor(second, bimap) )
import Control.Applicative ( Alternative((<|>)) )
import Control.Monad ( (>=>) )

data Rule 
  = Or [Int] [Int]
  | Then [Int]
  | Constant Char

newtype RuleParser a = RuleParser (String -> Maybe String)

instance Semigroup (RuleParser a) where
  (RuleParser p1) <> (RuleParser p2) = RuleParser $ \s -> p1 s <|> (p2 s <|> Nothing) 

instance Monoid (RuleParser a) where
  mempty = RuleParser Just

pThen :: RuleParser a -> RuleParser a -> RuleParser a
pThen (RuleParser p1) (RuleParser p2) = 
  RuleParser (p1 >=> p2)

pConstant :: Char -> RuleParser a
pConstant c = 
  RuleParser $ \case 
      "" -> Nothing
      (x:xs) -> if x == c then Just xs else Nothing

runParser :: RuleParser a -> String -> Maybe String
runParser (RuleParser p) = p

parseN :: RuleParser a -> Int -> RuleParser a
parseN p n = foldl (\acc _ -> acc `pThen` p) mempty [1..n]

isValidRule :: RuleParser a -> String -> Bool
isValidRule p s = case runParser p s of Just [] -> True; _ -> False

getRule :: Ord k => Map.Map k a -> k -> a
getRule mapping i = fromJust $ Map.lookup i mapping

getParser :: Map.Map Int Rule -> Rule -> RuleParser a
getParser r (Or xs ys) = 
  foldl (\acc i -> acc `pThen` getParser r (getRule r i)) mempty xs
  <> 
  foldl (\acc i -> acc `pThen` getParser r (getRule r i)) mempty ys
getParser r (Then xs) = 
  foldl (\acc i -> acc `pThen` getParser r (getRule r i)) mempty xs
getParser r (Constant c) = pConstant c

-- For part 2, try parsing 8 an increasing amount of times 
-- until 11 successfully parses or until 8 fails.
createParser :: Bool -> Map.Map Int Rule -> RuleParser a
createParser isPart1 r = 
  if isPart1 then
    getParser r $ fromJust $ Map.lookup 0 r
  else
    RuleParser $ \s -> tryParse nEights s
    where nEights = map (parseN (getParser r (getRule r 42))) [1..]
          pEleven = getParser r (getRule r 11)
          tryParse (p:ps) s = 
            case runParser p s of  
              Just rest -> 
                case runParser pEleven rest of 
                  Just "" -> Just ""
                  _ -> tryParse ps s
              _ -> Nothing

parseInput :: String -> (Int, Rule)
parseInput s = 
  if r2 == "" then
    if '\"' `elem` r1 then
      (num, Constant (r1 !! 2))
    else
      let rule = read <$>  words r1
      in (num, Then rule)
  else
    let rs1 = read <$> words r1 
        rs2 = read <$> words r2
    in (num, Or rs1 rs2)
  where (num, rules) = Bf.bimap (read :: String -> Int) tail $ span (/= ':') s
        (r1, r2) = Bf.second (drop 1) $ span (/= '|') rules

parse :: [String] -> (Map.Map Int Rule, [String])
parse ss = 
  (rulemap, msgs)
  where (rules, msgs) = Bf.second tail $ span (/= "") ss
        rulemap = Map.fromList $ map parseInput rules

main :: IO ()
main = do 
  (rules, msgs) <- parse . lines <$> readFile "../../input/19.txt" 
  let p1 = createParser True rules
  print $ length $ filter (==True) $ map (isValidRule p1) msgs

  (rules, msgs) <- parse . lines <$> readFile "../../input/19p2.txt"
  let p2 = createParser False rules
  print $ length $ filter (==True) $ map (isValidRule p2) msgs