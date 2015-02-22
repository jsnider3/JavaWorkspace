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
      }
    } else {
      tCount = 1
    }
    tPrev = tChar
  }
}

func (self *GoFishGame) checkForVictory(aTurn int) (bool) {
  return len(self._Hands[aTurn]) == 0
}

func (self *GoFishGame) drawCard() (string) {
  tCard := self._Deck[0]
  self._Deck = self._Deck[1:]
  return tCard
}

func (self *GoFishGame) isDeckEmpty() (bool) {
  return len(self._Deck) == 0
}
func (self *GoFishGame) printHand() {
  sort.Strings(self._Hands[self._Turn])
  fmt.Println("Player", self._Turn, "has:", self._Hands[self._Turn])
  fmt.Println("and a score of", self._Scores[self._Turn])
}

func (self *GoFishGame) computerTurn() {
  self.printHand()
  //while (!self._PlayerTurn)
  {
    //Check for books and victory
    //What to guess?
    //If the player has it.
      //Take his cards
    //else
      //Draw a card
      //Check for books and victory
      self._Turn = 0  
  }
  //If computer won
    //print something
  //else
    //self.playerTurn()
}

func (self *GoFishGame) playerTurn() {
  tOpponent := (self._Turn + 1) % 2
  self.checkForBooks()
  self.printHand()
  tVictory := self.checkForVictory(self._Turn)
  if (!tVictory) {
    //What to guess?
    tCard := getUserCard()
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
      if (!self.isDeckEmpty()) {
        tDraw := self.drawCard()
        fmt.Println("Drew", tDraw)
        self._Hands[self._Turn] = append(self._Hands[self._Turn], tDraw)
        //Check for books
        self.checkForBooks()
      }
      //End our turn.
      self._Turn = tOpponent  
    }
  }
  tVictory = self.checkForVictory(self._Turn)
  if (tVictory) {
    fmt.Println("Player", self._Turn, "won!")
  } else {
    self.playerTurn()
  }
}

func getUserCard() (string) {
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
