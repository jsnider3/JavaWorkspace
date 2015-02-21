package main

import "fmt"
import "math/rand"
import "time"
import "strings"

var kCards = [...]string{"2", "3", "4", "5", "6", "7", "8", "9",
                      "10", "J", "Q", "K", "A"}

type GoFishGame struct {
  _Hands [][]string
  _Deck []string
  _Turn int  
}

func (self *GoFishGame) checkForVictory(aTurn int) (bool) {
  return len(self._Hands[aTurn]) == 0
}

func (self *GoFishGame) drawCard() (string) {
  tCard := self._Deck[0]
  self._Deck = self._Deck[1:]
  return tCard
}
func (self *GoFishGame) printHands() {
  fmt.Print("Player has: ")
  fmt.Println(strings.Join(self._Hands[0], ", "))
  fmt.Print("Computer has: ")
  fmt.Println(strings.Join(self._Hands[1], ", "))
}

func (self *GoFishGame) computerTurn() {
  self.printHands()
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
  self.printHands()
  //Check for books
  tVictory := self.checkForVictory(self._Turn)
  fmt.Print("Current Player is ")
  fmt.Println(self._Turn)
  if (!tVictory) {
    //What to guess?
    //tCard := 
    getUserCard()
    //self.countOccurences(tCard,
    //If the other player has it.
      //Take his cards
    //else
      //Draw a card
      //Check for books
      self._Turn = tOpponent  
  }
  tVictory = self.checkForVictory(self._Turn)
  if (tVictory) {
    //print something
  } else {
    self.playerTurn()
  }
}

func getUserCard() (string) {
  fmt.Println("What card do you want?")
  var tCard string
  fmt.Scanf("%s\n", &tCard)
  fmt.Println("User gave us ", tCard)
  return tCard
}

func main() {
  rand.Seed(time.Now().UTC().UnixNano())
  tDeck := make([]string, 52)
  tPerm := rand.Perm(52)
  for tIndex := range tPerm {
    tVal := tPerm[tIndex]
    tCard := kCards[tVal/4]
    tDeck[tIndex] = tCard
  }
  tPlayerHand := tDeck[0:9]
  tCompHand := tDeck[9:18]
  tDeck = tDeck[18:]
  tHands := make([][]string, 2, 2)
  tHands[0] = tPlayerHand
  tHands[1] = tCompHand
  tGame := GoFishGame{tHands, tDeck, 0}
  tGame.playerTurn()
}
