/**
 * Class for storing the state of the board during a TicTacToe game.
 *
 * @author Josh Snider
 */

package com.joshuasnider.workspace.games.tictactoe;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class TicTacToeBoard {

	public static final String[][] BLANK_ARRAY=
    {{" ", " ", " "},
     {" ", " ", " "},
     {" ", " ", " "}};

  private String[][] state = new String[3][3];
  private short score;
  private short recentMove;
  private boolean XJustMoved;
  //X is max. O is min.
  //If it's Xs Turn then the next piece put on the board will be an O and all children will have XsTurn=false.

  public TicTacToeBoard(boolean b) {
    state[0] = BLANK_ARRAY[0].clone();
    state[1] = BLANK_ARRAY[1].clone();
    state[2] = BLANK_ARRAY[2].clone();
    XJustMoved = b;
    tidyUpState();
    score = score();
  }

  public TicTacToeBoard(String[][] array, boolean b) {
    state[0] = array[0].clone();
    state[1] = array[1].clone();
    state[2] = array[2].clone();
    XJustMoved = b;
    tidyUpState();
    score = score();
  }

  public TicTacToeBoard(TicTacToeBoard t, short s) {
    state[0] = t.state[0].clone();
    state[1] = t.state[1].clone();
    state[2] = t.state[2].clone();
    XJustMoved = !t.XJustMoved;
    recentMove = s;
    if (XJustMoved) {
      state[s / 3][s % 3] = "X";
    }
    else {
      state[s / 3][s % 3] = "O";
    }
    tidyUpState();
    score = score();
  }

  private final void tidyUpState() {
    for (String[] ar : state) {
      for (String str : ar) {
        if (str == null) {
          str = " ";
        }
        else if (!(str.equals("X")||str.equals("O"))) {
          str = " ";
        }
      }
    }
  }

  /**
   * Given a list of possible futures, return the one that's best for X.
   */
  public final static TicTacToeBoard max(List<TicTacToeBoard> input) {
    short maxIndex = 0;
    short max = input.get(maxIndex).score;
    for (short x = 0; x < input.size(); x++) {
      if (input.get(x).score == 2) {
        return input.get(x);
      }
      else if (input.get(x).score > max) {
        max = input.get(x).score;
        maxIndex = x;
      }
    }
    return input.get(maxIndex);
  }

  /**
   * Given a list of possible futures, return the one that's best for O.
   */
  public final static TicTacToeBoard min(List<TicTacToeBoard> input) {
    short minIndex = 0;
    short min = input.get(minIndex).score;
    for (short x = 0; x < input.size(); x++) {
      if (input.get(x).score == -2) {
        return input.get(x);
      }
      else if (input.get(x).score<min) {
        min = input.get(x).score;
        minIndex = x;
      }
    }
    return input.get(minIndex);
  }

  public final List<TicTacToeBoard> children(){
    List<TicTacToeBoard> children = new ArrayList<TicTacToeBoard>();
    if (hasWon("X") || hasWon("O") || isDraw()) {
      children.add(this);
    }
    else {
      List<Short> empties = getEmptySpots();
      for (short empty : empties) {
        children.add(new TicTacToeBoard(this, empty));
      }
    }
    return children;
  }

  public final void Move(short s) {
    if (s >= 0 && s <= 8) {
      state[s / 3][s % 3] = (XJustMoved ? "O" : "X");
      XJustMoved = !XJustMoved;
    }
  }

  public short getRecentMove() {
    return recentMove;
  }

  /**
   * Does the player have a winning column?
   */
  public final boolean hasWinningColumn(String playerChar) {
    for (int x = 0; x < 3; x++) {
      if (hasWonColumn(playerChar, x))
        return true;
    }
    return false;
  }

  /**
   * Does the player have a winning diagonal?
   */
  public final boolean hasWinningDiagonal(String playerChar) {
    if (state[1][1].equals(playerChar)) {
      if (state[0][0].equals(playerChar) && state[1][1].equals(state[2][2])) {
        return true;//Top-left to bottom-right.
      }
      if (state[2][0].equals(playerChar) && state[1][1].equals(state[0][2])) {
        return true;//Top right to bottom left
      }
    }
    return false;
  }

  /**
   * Does the player have a winning row?
   */
  public final boolean hasWinningRow(String playerChar) {
    for (int x = 0; x < 3; x++) {
      if (hasWonRow(playerChar, x))
        return true;
    }
    return false;
  }

  /**
   * There are 8 possible ways for the given player to have won.
   *  There are three rows, three columns, and two diagonals.
   */
  public final boolean hasWon(String playerChar){
    return (hasWinningColumn(playerChar) ||
        hasWinningRow(playerChar) ||
        hasWinningDiagonal(playerChar));
  }

  /**
   * Check if the given player has their mark in the entire column.
   */
  public boolean hasWonColumn(String playerChar, int rowIndex) {
    return state[rowIndex][0].equals(playerChar) && state[rowIndex][0].equals(state[rowIndex][1])
        && state[rowIndex][1].equals(state[rowIndex][2]);
  }

  /**
   * Check if the given player has their mark in the entire row.
   */
  public boolean hasWonRow(String playerChar, int rowIndex) {
    return state[0][rowIndex].equals(playerChar) && state[0][rowIndex].equals(state[1][rowIndex])
        && state[1][rowIndex].equals(state[2][rowIndex]);
  }

  /**
   * Is the game drawn?
   */
  public final boolean isDraw() {
    if (hasWon("X") || hasWon("O")) {
      return false;
    }
    return getEmptySpots().isEmpty();
  }

  /**
   * Has the game finished in any way?
   */
  public final boolean isOver(){
    return isDraw() || hasWon("X") || hasWon("O");
  }

  /**
   * List the empty spots on the map.
   */
  public final List<Short> getEmptySpots() {
    List<Short> empties = new ArrayList<Short>();
    for (short x = 0; x < 9; x++) {
      if ("".equals(state[x / 3][x % 3]) || " ".equals(state[x / 3][x % 3])) {
        empties.add(x);
      }
    }
    return empties;
  }

  /**
   * Get the score for this position.
   * Higher is better for x. Range is [-2, 2].
   */
  public final short score() {
    if (hasWon("X")) {
      return 2;
    } else if (hasWon("O")) {
      return -2;
    } else if (isDraw()) {
      return 0;
    } else if (!XJustMoved) {
      short highest = Collections.max(scoreChildren());
      return highest == 2 ? 1 : highest;
    } else {
      short lowest = Collections.min(scoreChildren());
      return lowest == -2 ? -1 : lowest;
    }
  }

  /**
   * Collect the scores of our children.
   */
  public List<Short> scoreChildren() {
    List<Short> scores = new ArrayList<>();
    for (TicTacToeBoard child : children()) {
      scores.add(child.score);
    }
    return scores;
  }

  /**
   * What does our score mean in plain English?
   */
  public String scoreToString() {
    String msg = "";
    if (score == -2) {
      msg = "O has won.";
    } else if (score == -1) {
      msg = "O will win.";
    } else if (score == 0) {
      if (isDraw()) {
        msg = "Draw";
      } else {
        msg = "Eventual Draw";
      }
    } else if (score == 1) {
      msg = "X will win.";
    } else if (score == 2) {
      msg = "X has won.";
    }
    return msg;
  }

  public final String toString() {
    String str = "";
    str += state[0][0] + "|" + state[0][1] + "|" + state[0][2] + "\n";
    str += "-----" + "\n";
    str += state[1][0] + "|" + state[1][1] + "|" + state[1][2] + "\n";
    str += "-----" + "\n";
    str += state[2][0] + "|" + state[2][1] + "|" + state[2][2] + "\n";
    return str;
  }

  /**
   * Print out the board and various debugging information.
   */
  public final void printVerbose() {
    System.out.println(this);
    System.out.println(XJustMoved ? "X just went" : "O just went");
    System.out.println(scoreToString());
  }

  /**
   * Have the computer make its optimal move.
   */
  public void compMove() {
    short move = 0;
    if (XJustMoved) {
      move = min(children()).recentMove;
    } else {
      move = max(children()).recentMove;
    }
    Move(move);
  }
}
