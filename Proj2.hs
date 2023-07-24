{-
    Author: Declan Dempsey
    Date: 27th May 2023
    StudentID: 1336622
    Email: ddempsey@student.unimelb.edu.au
    Purpose: This program plays the role of a guesser and an answerer in a card 
    guessing game.
    
    Description:
    The following program contains the code for both the answerer and guesser 
    parts of a card guessing game. The goal of the program is for the guesser 
    to correctly guess the set of answer cards in the least amount of guesses 
    as possible.

    The program starts with an input of a list of cards. This forms
    the target answer. The program then attempts to guess the answer cards 
    by conducting a sequence of steps to deduce the answer. For each guess,
    feedback is given based on the answer and includes:
    - the number of correctly guessed cards, 
    - how many cards in the answer have rank lower than the lowest rank in the 
      guess 
    - how many of the cards in the answer have the same rank as a card in the 
      guess 
    - how many cards in the answer have rank higher than the highest rank in 
      the guess
    - how many of the cards in the answer have the same suit as a card in the 
      guess

    Based on the feedback, the remaining list of possible answers is trimmed 
    down. The next guess is then selected by calculating which guess will leave
    the smallest remaining list of possible answers. 
-}
module Proj2 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List
import Data.Maybe

-- Declare the GameState type to be a list of list of cards
type GameState = [[Card]]

-- Declare the feedback type as a quintuple for readability
type Feedback = (Int, Int, Int, Int, Int)

{- 
initialGuess takes the number of answer cards as input and produces a initial 
guess and the initial gameState. 
- Calls initialHelper to generate the initial guess
- Calls generateGameState to produce the initial game state
-}
initialGuess :: Int -> ([Card],GameState)
initialGuess numCards = (initialHelper numCards, generateGameState numCards)

{- 
feedback is used to generate the quintuple of feedback numbers after comparing
the answer to the guess
- Calls countCorrect, lowerRank, countRank, higherRank and countSuit to 
    generate each item in the tuple
-}
feedback :: [Card] -> [Card] -> Feedback
feedback t g = (countCorrect t g, lowerRank t g, countRank t g,
    higherRank t g, countSuit t g)

{- 
nextGuess takes a input pair of guess and game state, gets the feedback for the guess
then produces a new guess and an updated game state
- getNewGameState is called to develop a set of remaining possible answers
- getGuess is called to produce the best possible guess
-}
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (prevGuess, answerSet) fdbk = (newGuess, newGameState)
    where newGameState = getNewGameState prevGuess fdbk answerSet
          newGuess = getGuess newGameState

{- 
getNewGameState takes a previous guess and its feedback, and filters out 
answers from the game state that don't match the feedback. Reduces the 
gamestate list to the remaining possible answers
-}
getNewGameState :: [Card] -> Feedback -> GameState -> GameState
getNewGameState prevGuess fdbk answerSet = filter (\ans -> fdbk == feedback 
    ans prevGuess) answerSet
    
{-
Returns the length of the list of cards in the target that match the guess
-}
countCorrect :: [Card] -> [Card] -> Int
countCorrect target guess = length (filter (`elem` guess) target)

{-
Returns the length of the list of cards in the target that are lower than the
lowest ranked card in the guess
-}
lowerRank :: [Card] -> [Card] -> Int
lowerRank target guess = length (filter (\x -> getRank x < guessRank) target)
    where guessRank  = minimum (map getRank guess)

{-
Call rankHelper after converting target and guess to rank form
-}
countRank :: [Card] -> [Card] -> Int
countRank target guess = countHelper guessTarget guessRanks
    where guessRanks = map getRank guess
          guessTarget = map getRank target
          
{-          
Counts ranks in the target that match ranks in the guess. Once there is a
match, it is removed from the guess
-}
countHelper :: Eq a => [a] -> [a] -> Int
countHelper [] guess = 0
countHelper (x:xs) guess = 
    if x `elem` guess
        then 1 + countHelper xs (delete x guess)
        else countHelper xs guess
{-
Returns the length of a list of cards in the target that have a higher rank
than the highest ranked card in the guess
-}
higherRank :: [Card] -> [Card] -> Int
higherRank target guess = length (filter (\x -> getRank x > guessRank) target)
    where guessRank  =  maximum (map getRank guess)

{-
Call countHelper after converting target and guess into suit form
-}
countSuit :: [Card] -> [Card] -> Int
countSuit target guess = countHelper targetSuits guessSuits
    where guessSuits = map getSuit guess
          targetSuits = map getSuit target

{-
Returns a Card's suit (Club, Spade, Diamond, Heart)
-}
getSuit :: Card -> Suit
getSuit (Card s _) = s

{-
Returns a Card's rank (R2..Ace)
-}
getRank :: Card -> Rank
getRank (Card _ r) = r

{-
Returns a list of cards depending on the number of cards in the answer. For 
for an n card answer, ranks are 13/(n+1) apart.
-}
initialHelper :: Int -> [Card]
initialHelper numCards
    | numCards == 2 = [Card Heart R6, Card Spade R10]
    | numCards == 3 = [Card Heart R4, Card Spade R8, Card Diamond Queen]
    | numCards == 4 = [Card Heart R3, Card Spade R6, Card Diamond R9, 
        Card Club Queen]
        
{-
Generate a list of all possible cards from a deck
-}
cardDeck = [minBound..maxBound]::[Card]

{-
Takes the number of cards in the answer and generates all posisble
combinations of answers by calling combinations
-}
generateGameState :: Int -> [[Card]]
generateGameState n = combinations n cardDeck

{-
Takes the number of cards n, a list of all cards in a deck and produces a  
list of all possible combinations of n cards
-}
combinations :: Int -> [Card] -> [[Card]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

{-
Takes a candidate guess and the GameState, then produces a grouped list of 
Feedback based on each item in the GameState being the answer.
-}
groupFeedback :: [Card] -> GameState -> [[Feedback]]
groupFeedback guess gameState = group (map (feedback guess) gameState)

{-
Takes a grouped list of Feedback and return a list of tuples, denoting the 
the size of that feedback group and the feedback itself
-}
groupCount :: [[Feedback]] -> [(Int, Feedback)]
groupCount feedbackLst = map (\x -> (length x, head x)) feedbackLst

{-
Returns the feedback of a candidate guess and answer, based on each item in 
the GameState being the answer. It first groups the feedback values together, 
then it calculates the size of each feedback group. Finally it produces a weight
by summing the sqaures of the group sizes divided by the sum of group sizes.
-}
weightedSum :: [Card]-> GameState -> Int
weightedSum guess gameState = sumSquares sizes
    where 
        groupFdbk = groupFeedback guess gameState
        groups = groupCount groupFdbk
        sizes = map fst groups

{-
Helper function to calculate the sum of the squares of the group sizes 
divided by the sum of the group sizes.
-}
sumSquares :: [Int] -> Int
sumSquares size = sum (map (^2) size) `div` sum size

{-
Produces the weight of each possible guess in the gamesate
-}
getWeightedSums :: GameState -> GameState -> [Int]
getWeightedSums [] _ = []
getWeightedSums (x:xs) gameState = weightedSum x gameState: 
    getWeightedSums xs gameState

{- 
Returns the best possible guess by selecting the index of the guess from the
GameState that has the minimum weight.
-}
getGuess :: GameState -> [Card]
getGuess [g] = g
getGuess gameState = gameState!!idx
    where
        wSums = getWeightedSums gameState gameState
        idx = fromMaybe (-1) $ elemIndex (minimum wSums) wSums