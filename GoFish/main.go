package main

import "fmt"
import "math/rand"
import "sort"
import "time"

var kCards = [13]string{"2", "3", "4", "5", "6", "7", "8", "9",
	"10", "J", "Q", "K", "A"}

type GoFishGame struct {
	_Hands  [][]string
	_Deck   []string
	_Turn   int
	_Scores []int
}

/*
* We score a point if we have four
* of a kind. So, we sort our hand
* and if we have four in a row of
* the same pip, we take them out
* of our hand and get a point.
 */
func (self *GoFishGame) checkForBooks() {
	sort.Strings(self._Hands[0])
	sort.Strings(self._Hands[1])
	tPrev := ""
	tCount := 1
	for _, tChar := range self._Hands[self._Turn] {
		if tChar == tPrev {
			tCount++
			if tCount == 4 {
				fmt.Println("Book of", tChar)
				self.removeOccurences(tChar, self._Turn)
				self._Scores[self._Turn]++
				if self.isHandEmpty() {
					self.drawCard()
				}
			}
		} else {
			tCount = 1
		}
		tPrev = tChar
	}
}

/*
* Safely draw a card and put it in your hand.
 */
func (self *GoFishGame) drawCard() {
	if !self.isDeckEmpty() {
		tCard := self._Deck[0]
		self._Deck = self._Deck[1:]
		if self._Turn == 0 {
			fmt.Println("Drew", tCard)
		}
		self._Hands[self._Turn] = append(self._Hands[self._Turn], tCard)
		//Check for books
		self.checkForBooks()
	}
}

/*
* See if the game has ended.
* Else, let the next person go.
 */
func (self *GoFishGame) endPly() {
	tGameOver := self.isGameOver()
	if tGameOver {
		self.printGameOverMessage()
	} else if self._Turn == 1 {
		self.playerTurn(getPickComputer)
	} else {
		self.playerTurn(getPickUser)
	}
}

/*
* Pick a card that the computer has
* randomly.
 */
func getPickComputer(self *GoFishGame) string {
	tHand := self._Hands[1]
	tChoice := "A"
	if len(tHand) > 0 {
		tChoice = tHand[rand.Intn(len(tHand))]
	}
	fmt.Println("Computer picks", tChoice)
	return tChoice
}

/*
* Ask the user what they want to pick.
 */
func getPickUser(self *GoFishGame) string {
	fmt.Println("What card do you want?")
	var tCard string
	fmt.Scanf("%s\n", &tCard)
	return tCard
}

/*
* Convenience function.
 */
func (self *GoFishGame) isDeckEmpty() bool {
	return len(self._Deck) == 0
}

/*
* Convenience function.
 */
func (self *GoFishGame) isHandEmpty() bool {
	return len(self._Hands[self._Turn]) == 0
}

/*
* The game is over when all 13 pips have
* been made into sets.
 */
func (self *GoFishGame) isGameOver() bool {
	return self._Scores[0]+self._Scores[1] == 13
}

/*
* Make a deck contains 4 copies of each
* card and shuffle it.
 */
func makeDeck() []string {
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

/*
* returns true if the opponent's hand contains an aCard.
 */
func (self *GoFishGame) opponentHas(aCard string) bool {
	for _, tCard := range self._Hands[(self._Turn+1)%2] {
		if tCard == aCard {
			return true
		}
	}
	return false
}

/*
* Handle both the players and computers turns.
* Differences between them are handled by checking
* whose turn it is manually and through the getPick
* parameter.
 */
func (self *GoFishGame) playerTurn(getPick func(*GoFishGame) string) {
	tOpponent := (self._Turn + 1) % 2
	self.checkForBooks()
	if tOpponent == 1 {
		self.printHand()
	}
	if self.isHandEmpty() {
		self.drawCard()
	}
	tGameOver := self.isGameOver()
	if !tGameOver {
		tCard := getPick(self)
		if self.opponentHas(tCard) {
			tCount := self.removeOccurences(tCard, tOpponent)
			for tIndex := 0; tIndex < tCount; tIndex++ {
				self._Hands[self._Turn] = append(self._Hands[self._Turn], tCard)
			}
			self.checkForBooks()
		} else {
			self.drawCard()
			self._Turn = tOpponent
		}
	}
	self.endPly()
}

/*
* Determine and say who won.
 */
func (self *GoFishGame) printGameOverMessage() {
	fmt.Println("Final score is", self._Scores[0], "to", self._Scores[1])
	if self._Scores[0] > self._Scores[1] {
		fmt.Println("Player wins!")
	} else if self._Scores[0] == self._Scores[1] {
		fmt.Println("It's a tie.")
	} else {
		fmt.Println("Computer wins!")
	}
}

/*
* Print player's hand and current score.
 */
func (self *GoFishGame) printHand() {
	sort.Strings(self._Hands[0])
	sort.Strings(self._Hands[1])
	fmt.Println("You have:", self._Hands[0])
	fmt.Println("Score is", self._Scores[0], "to", self._Scores[1])
}

/*
* Remove all occurences of aElem from the hand
* represented by aSide.
 */
func (self *GoFishGame) removeOccurences(aElem string, aSide int) int {
	tCount := 0
	tList := self._Hands[aSide]
	var tFiltered []string
	for _, tCard := range tList {
		if tCard == aElem {
			tCount++
		} else {
			tFiltered = append(tFiltered, tCard)
		}
	}
	self._Hands[aSide] = tFiltered
	return tCount
}

/*
* Set up and begin the game.
 */
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
	tGame.playerTurn(getPickUser)
}
