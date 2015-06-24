package main;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.stream.Stream;

/**
* @author Tamir
*/
public class Hand {

private Card[] hand = new Card[2];

public enum HandRank {
    ROYAL_FLUSH,
    STRAIGHT_FLUSH,
    FOUR_OF_A_KIND,
    FULL_HOUSE,
    FLUSH,
    STRAIGHT,
    THREE_OF_A_KIND,
    TWO_PAIR,
    PAIR,
    HIGH_CARD;
}

public Hand() {
}

public Hand(Card[] hand) {
  this.hand = hand;
}

public Card[] getHand() {
  return hand;
}

public void setHand(Card[] hand) {
  this.hand = hand;
}

public void printHand() {
  for (Card c : hand) {
    System.out.println(c);
  }
}

public HandRank determineHandRank(Card[] flop) {
    if (isARoyalFlush(flop)) {
        return HandRank.ROYAL_FLUSH;
    } else if (isAStraightFlush(flop)) {
        return HandRank.STRAIGHT_FLUSH;

    } else if (isAFourOfAKind(flop)) {
        return HandRank.FOUR_OF_A_KIND;
    } else if (isAFullHouse(flop)) {
        return HandRank.FULL_HOUSE;
    } else if (isAFlush(flop)) {
        return HandRank.FLUSH;
    } else if (isAStraight(flop)) {
        return HandRank.STRAIGHT;
    } else if (isThreeOfAKind(flop)) {
        return HandRank.THREE_OF_A_KIND;
    } else if (isTwoPair(flop)) {
        return HandRank.TWO_PAIR;
    } else if (isPair(flop)) {
        return HandRank.PAIR;
    } else {
        return HandRank.HIGH_CARD;
    }
}

public boolean isARoyalFlush(Card[] flop) {
    if (isAStraight(flop) && isAFlush(flop)) {
        Card[] allCards = Stream.concat(Arrays.stream(flop), Arrays.stream(hand))
                .toArray(Card[]::new);
        boolean aceExists = false, kingExists = false, queenExists = false, jackExists = false, tenExists = false;
        for (Card c : allCards) {
            switch (c.getRank().getRank()) {
                case "ACE":
                    aceExists = true;
                    break;
                case "KING":
                    kingExists = true;
                    break;
                case "QUEEN":
                    queenExists = true;
                    break;
                case "JACK":
                    jackExists = true;
                    break;
                case "TEN":
                    tenExists = true;
                    break;

            }
        }
        return (aceExists && kingExists && queenExists && jackExists && tenExists);
    } else {
        return false;
    }
}

public boolean isAStraight(Card[] flop) {
    Card[] allCards = Stream.concat(Arrays.stream(flop), Arrays.stream(hand))
            .toArray(Card[]::new);
    Arrays.sort(allCards, byRank);
    int noOfCardsInARow = 0;
    int pos = 0;
    boolean isAStraight = false;
    while (pos < allCards.length - 1 && !isAStraight) {
        if (allCards[pos + 1].getRank().getValue() - allCards[pos].getRank().getValue() == 1) {
            noOfCardsInARow++;
            if (noOfCardsInARow == 4) {
                isAStraight = true;
            } else {
                pos++;
            }
        } else {
            noOfCardsInARow = 0;
            pos++;
        }
    }
    return isAStraight;
}

public boolean isAFlush(Card[] flop) {
    Card[] allCards = Stream.concat(Arrays.stream(flop), Arrays.stream(hand))
            .toArray(Card[]::new);
    int noOfClubs = 0;
    int noOfSpades = 0;
    int noOfHearts = 0;
    int noOfDiamonds = 0;
    //TODO This could be rewritten, by suit-sorting them
    //and checking for a run of five with the same suit.
    for (Card c : allCards) {
        switch (c.getSuit()) {
            case "HEART":
                noOfHearts++;
                break;
            case "SPADES":
                noOfSpades++;
                break;
            case "CLUBS":
                noOfClubs++;
                break;
            case "DIAMONDS":
                noOfDiamonds++;
                break;
        }
    }
    return (noOfClubs == 5 || noOfSpades == 5 || noOfHearts == 5 || noOfDiamonds == 5);
}

private boolean isThreeOfAKind(Card[] flop) {
    Card[] allCards = Stream.concat(Arrays.stream(flop), Arrays.stream(hand))
            .toArray(Card[]::new);
    int cardRepeats = 1;
    boolean isThreeOfAKind = false;
    int i = 0;
    int k = i + 1;
    while (i < allCards.length && !isThreeOfAKind) {
        cardRepeats = 1;
        while (k < allCards.length && !isThreeOfAKind) {
            if (allCards[i].getRank().getValue() == allCards[k].getRank().getValue()) {
                cardRepeats++;
                if (cardRepeats == 3) {
                    isThreeOfAKind = true;
                }
            }
            k++;
        }
        i++;
    }
    return isThreeOfAKind;
}

private boolean isTwoPair(Card[] flop) {
    Card[] allCards = Stream.concat(Arrays.stream(flop), Arrays.stream(hand))
            .toArray(Card[]::new);
    int cardRepeats = 1;
    int noOfCardRepeats = 0;
    boolean isTwoPair = false;
    int i = 0;
    int k = i + 1;
    while (i < allCards.length && !isTwoPair) {
        cardRepeats = 1;
        while (k < allCards.length && !isTwoPair) {
            if (allCards[i].getRank().getValue() == allCards[k].getRank().getValue()) {
                cardRepeats++;
                if (cardRepeats == 2) {
                    cardRepeats = 1;
                    noOfCardRepeats++;
                    if (noOfCardRepeats == 2) {
                        isTwoPair = true;

                    }
                }
            }
            k++;
        }
        i++;
    }
    return isTwoPair;
}

private boolean isPair(Card[] flop) {
    Card[] allCards = Stream.concat(Arrays.stream(flop), Arrays.stream(hand))
            .toArray(Card[]::new);
    int cardRepeats = 1;
    boolean isPair = false;
    int i = 0;
    int k = i + 1;
    while (i < allCards.length && !isPair) {
        cardRepeats = 1;
        while (k < allCards.length && !isPair) {
            if (allCards[i].getRank().getValue() == allCards[k].getRank().getValue()) {
                cardRepeats++;
                if (cardRepeats == 2) {
                    isPair = true;
                }
            }
            k++;
        }
        i++;
    }
    return isPair;
}
public Comparator<Card> byRank = (Card left, Card right) -> {
  //DEBUG Suspicious.
    if (left.getRank().getValue() < right.getRank().getValue()) {
        return -1;
    } else {
        return 1;
    }
};

private boolean isAFullHouse(Card[] flop) {
    Card[] allCards = Stream.concat(Arrays.stream(flop), Arrays.stream(hand))
            .toArray(Card[]::new);

    Arrays.sort(allCards, byRank);
    int noOfRepeats = 1;
    boolean isThreeOfAKind = false;
    boolean isTwoOfAKind = false;
    for (int i = 0; i < allCards.length - 1; i++) {
        if (allCards[i].getRank().getValue() == allCards[i + 1].getRank().getValue()) {
            noOfRepeats++;
            if (noOfRepeats == 3) {
                isThreeOfAKind = true;
                noOfRepeats = 1;
            } else if (noOfRepeats == 2) {
                isTwoOfAKind = true;
                noOfRepeats = 1;
            }
        } else {
            noOfRepeats = 1;
        }
    }
    return (isTwoOfAKind && isThreeOfAKind);

}

public boolean isAFourOfAKind(Card[] flop) {
    Card[] allCards = Stream.concat(Arrays.stream(flop), Arrays.stream(hand))
            .toArray(Card[]::new);
    int cardRepeats = 1;
    boolean isFourOfAKind = false;
    int i = 0;
    int k = i + 1;
    while (i < allCards.length && !isFourOfAKind) {
        cardRepeats = 1;
        while (k < allCards.length && !isFourOfAKind) {
            if (allCards[i].getRank().getValue() == allCards[k].getRank().getValue()) {
                cardRepeats++;
                if (cardRepeats == 4) {
                    isFourOfAKind = true;
                }
            }
            k++;
        }
        i++;
    }
    return isFourOfAKind;
}

private boolean isAStraightFlush(Card[] flop) {
    return isAFlush(flop) && isAStraight(flop)
}

public Card getHighCard(Card[] flop) {
    Card[] allCards = Stream.concat(Arrays.stream(flop), Arrays.stream(hand))
            .toArray(Card[]::new);
    Arrays.sort(allCards, byRank);
    return allCards[0];
}

public Card getHandHighCard() {
    Arrays.sort(hand, byRank);
    return hand[0];
}
}
