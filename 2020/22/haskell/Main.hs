import qualified Data.Set as S

main :: IO ()
main = do
    rawInput <- lines <$> readFile "../../input/22.txt"
    let (name1:raw_deck1, _sep:name2:raw_deck2) = break (=="") rawInput
    let deck1 = map read raw_deck1
    let deck2 = map read raw_deck2

    print $ score $ part1 (deck1, deck2)
    print $ score $ part2 S.empty (deck1, deck2)

score :: (Num p, Enum p) => ([p], [p]) -> p
score decks = case decks of 
    (winnerDeck, []) -> deckScore winnerDeck
    ([], winnerDeck) -> deckScore winnerDeck
    where deckScore = sum . zipWith (*) [1..] . reverse

part2 :: S.Set ([Int], [Int]) -> ([Int], [Int]) -> ([Int], [Int])
part2 _ (deck1, []) = (deck1, [])
part2 _ ([], deck2) = ([], deck2)
part2 history (deck1@(drawn1:rest1), deck2@(drawn2:rest2))
    | (deck1, deck2) `S.member` history = (deck1, [])
    | otherwise = part2 newHistory (playRecursiveCombatRound (deck1, deck2))
    where newHistory = (deck1, deck2) `S.insert` history

playRecursiveCombatRound :: ([Int], [Int]) -> ([Int], [Int])
playRecursiveCombatRound (deck1@(drawn1:rest1), deck2@(drawn2:rest2)) = (newDeck1, newDeck2)
    where newDeck1 = tail deck1 ++ if player1Wins then [drawn1, drawn2] else []
          newDeck2 = tail deck2 ++ if player1Wins then [] else [drawn2, drawn1]
          player1Wins = (ableToRecurse && player1WinsSubgame) || (not ableToRecurse && (drawn1 > drawn2))
          player1WinsSubgame = null . snd $ part2 S.empty (recurseDeck1, recurseDeck2)
          recurseDeck1 = take drawn1 rest1
          recurseDeck2 = take drawn2 rest2
          ableToRecurse = (length deck1 > drawn1) && (length deck2 > drawn2)

part1 :: ([Int], [Int]) -> ([Int], [Int])
part1 (deck1, []) = (deck1, [])
part1 ([], deck2) = ([], deck2)
part1 decks = part1 (playCombatRound decks)

playCombatRound :: ([Int], [Int]) -> ([Int], [Int])
playCombatRound (deck1@(drawn1:rest1), deck2@(drawn2:rest2)) = (newDeck1, newDeck2)
    where newDeck1 = rest1 ++ if player1wins then [drawn1, drawn2] else []
          newDeck2 = rest2 ++ if player1wins then [] else [drawn2, drawn1]
          player1wins = drawn1 > drawn2
