# haskell-card-game

Author: Declan Dempsey
Date: 27th May 2023
StudentID: 1336622
Email: ddempsey@student.unimelb.edu.au
Purpose: This program plays the role of a guesser and an answerer in a card 
guessing game.

# Description:
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

# Run
In ghci
```
:l Test.hs
```

For a test case, input the following
```
guessTest "3H 4D"
```
or any other set of 2-4 cards. 
