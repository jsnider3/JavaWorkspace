package main

import "fmt"
import "math/rand"
import "time"
import "sort"

var kCards = [...]string{"2", "3", "4", "5", "6", "7", "8", "9",
                      "10", "J", "Q", "K", "A"}

type GoFishGame struct {
  _Hands [][]string
  _Deck []string
  _Turn int  
  _Scores []int
}

func (self *GoFishGame) checkForBooks() {
  sort.Strings(self._Hands[0])
  sort.Strings(self._Hands[1])
  tPrev := " "
  tCount := 1
  for _, tChar := range self._Hands[self._Turn] {
    if tChar == tPrev {
      tCount++
      if tCount == 4 {
        fmt.Println("Book of", tChar)
        self.removeOccurences(tChar, self._Turn)
        self._Scores[self._Turn]++
        break
      }
    } else {
      tCount = 1
    }
    tPrev = tChar
  }
}

func (self *GoFishGame) isGameOver(aTurn int) (bool) {
  return self._Scores[0] + self._Scores[1] == 13
}

func (self *GoFishGame) drawCard() () {
  if (!self.isDeckEmpty()) {
    tCard := self._Deck[0]
    self._Deck = self._Deck[1:]
    fmt.Println("Drew", tCard)
    self._Hands[self._Turn] = append(self._Hands[self._Turn], tCard)
    //Check for books
    self.checkForBooks()
  }
}

func (self *GoFishGame) isDeckEmpty() (bool) {
  return len(self._Deck) == 0
}

func (self *GoFishGame) printGameOverMessage() {
  fmt.Println("Final score is",self._Scores[0],"to",self._Scores[1])
  if self._Scores[0] > self._Scores[1] {
    fmt.Println("Player wins!")
  } else if self._Scores[0] == self._Scores[1] {
    fmt.Println("It's a tie.")
  } else {
    fmt.Println("Computer wins!")
  }
}

func (self *GoFishGame) printHand() {
  sort.Strings(self._Hands[0])
  sort.Strings(self._Hands[1])
  fmt.Println("You have:", self._Hands[0])
  fmt.Println("and a score of",self._Scores[0])
  fmt.Println("Computer score is",self._Scores[1])
  fmt.Println("PC has:", self._Hands[1])
}

func (self *GoFishGame) computerTurn() {
  tOpponent := 0
  tGameOver := self.isGameOver(self._Turn)
  if (!tGameOver) {
    //What to guess?
    tCard := self.getPickComputer()
    //Take it from other deck and put in ours.
    tCount := self.removeOccurences(tCard, tOpponent)
    if (tCount > 0) {
      for tIndex := 0; tIndex < tCount; tIndex++ {
        self._Hands[self._Turn] = append(self._Hands[self._Turn], tCard)
      }
      //Check for books
      self.checkForBooks()
    } else {
      //Draw a card
      self.drawCard()
      //End our turn.
      self._Turn = tOpponent  
    }
  }
  tGameOver = self.isGameOver(1)
  if (tGameOver) {
    self.printGameOverMessage()
  } else if self._Turn == tOpponent {
    self.playerTurn()
  } else {
    self.computerTurn()
  }
}

func (self *GoFishGame) playerTurn() {
  tOpponent := 1
  self.printHand()
  tGameOver := self.isGameOver(0)
  if (!tGameOver) {
    //What to guess?
    tCard := self.getPickUser()
    //Take it from other deck and put in ours.
    tCount := self.removeOccurences(tCard, tOpponent)
    if (tCount > 0) {
      for tIndex := 0; tIndex < tCount; tIndex++ {
        self._Hands[self._Turn] = append(self._Hands[self._Turn], tCard)
      }
      //Check for books
      self.checkForBooks()
    } else {
      //Draw a card
      self.drawCard()
      //End our turn.
      self._Turn = tOpponent  
    }
  }
  tGameOver = self.isGameOver(0)
  if (tGameOver) {
    self.printGameOverMessage()
  } else if self._Turn == tOpponent {
    self.computerTurn()
  } else {
    self.playerTurn()
  }
}

func (self *GoFishGame) getPickComputer() (string) {
  tHand := self._Hands[1]
  tChoice := "A"
  if len(tHand) > 0 {
    tChoice = tHand[rand.Intn(len(tHand))]
  }
  fmt.Println("Computer picks", tChoice)
  return tChoice
}

func (self *GoFishGame) getPickUser() (string) {
  fmt.Println("What card do you want?")
  var tCard string
  fmt.Scanf("%s\n", &tCard)
  return tCard
}

func (self *GoFishGame) removeOccurences(aElem string, aSide int) (int) {
  tCount := 0
  tList := self._Hands[aSide]
  var tFiltered []string
  for _, tCard := range tList {
    if (tCard == aElem) {
      tCount++
    } else {
      tFiltered = append(tFiltered, tCard)
    } 
  } 
  self._Hands[aSide] = tFiltered
  return tCount
}

func makeDeck() ([]string) {
  rand.Seed(time.Now().UTC().UnixNano())
  tDeck := make([]string, 52)
  tPerm := rand.Perm(52)
  for tIndex := range tPerm {
    tVal := tPerm[tIndex]
    tCard := kCards[tVal/4]
    tDeck[tIndex] = tCard
  }
  return tDeck
}

func main() {
  tDeck := makeDeck()
  tPlayerHand := tDeck[0:9]
  tCompHand := tDeck[9:18]
  tDeck = tDeck[18:]
  tHands := make([][]string, 2, 2)
  tHands[0] = tPlayerHand
  tHands[1] = tCompHand
  tScores := make([]int, 2, 2)
  tScores[0] = 0
  tScores[1] = 0
  tGame := GoFishGame{tHands, tDeck, 0, tScores}
  tGame.playerTurn()
}
