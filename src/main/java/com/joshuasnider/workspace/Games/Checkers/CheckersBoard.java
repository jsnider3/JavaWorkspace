package com.joshuasnider.workspace.games.checkers;

import java.util.ArrayList;
import java.util.List;

public class CheckersBoard {
  private String[][] state;
  private int score;
  private int recentMove;
  private boolean XJustMoved;

  public CheckersBoard(boolean b) {
    state = new String[8][8];
    for (int y = 0; y < 8; y++) {
      state[y] = new String[8];
    }
    for (int y = 0; y < 8; y++) {
      for (int x = 0; x < 3; x++) {
        String spot = "-";
        if (isPlayableSquare(x, y)) {
          spot = "w";
        }
        state[x][y] = spot;
      }
      for (int x = 3; x < 5; x++) {
        String spot = "-";
        if (isPlayableSquare(x, y)) {
          spot = "+";
        }
        state[x][y] = spot;
      }
      for (int x = 5; x < 8; x++) {
        String spot = "-";
        if (isPlayableSquare(x, y)) {
          spot = "b";
        }
        state[x][y] = spot;
      }
    }

    XJustMoved = b;
    score = score();
  }

  public CheckersBoard(String[][] array, boolean b) {
    state = new String[8][8];
    for (int x = 0; x < 8; x++) {
      state[x] = array[x].clone();
    }
    XJustMoved = b;
    score = score();
  }

  /**
   * Provides best move for X.
   */
  public final CheckersBoard max(List<CheckersBoard> input) {
    //TODO
    return this;
  }

  /**
   * Provides best move for O.
   */
  public final CheckersBoard min(List<CheckersBoard> input) {
    //TODO
    return this;
  }

  public final List<CheckersBoard> children() {
    //TODO
    return null;
  }

  public final void Move(int s) {
    //TODO
  }

  /**
   * Detect if the given player has any live opponents.
   */
  public final boolean hasWon(String playerColor) {
    for (int x = 0; x < 8; x++) {
      for (int y = 0; y < 8; y++) {
        String spot = state[x][y];
        if (spot.equals("-") ||
            spot.equals("+") ||
            spot.equals(playerColor.toLowerCase()) ||
            spot.equals(playerColor.toUpperCase()))
        {

        } else {
          return false;
        }
      }
    }
    return true;
  }

  public final boolean isDraw() {
    //TODO
    return false;
  }

  /**
   * Is this one of the usable squares?
   */
  public static boolean isPlayableSquare(int x, int y) {
    return x % 2 != y % 2;
  }

  public final boolean isOver() {
    return hasWon("w") || hasWon("b");
  }

  public final int score() {
    //TODO
    if (hasWon("w")) {
      return 1;
    } else if (hasWon("b")) {
      return -1;
    } else {
      return 0;
    }
  }

  public final String toString() {
    StringBuffer buf = new StringBuffer();
    for (int x = 0; x < 8; x++) {
      for (int y = 0; y < 8; y++) {
        buf.append(state[x][y]);
      }
      if (x != 8 - 1) {
        buf.append("\n");
      }
    }
    return buf.toString();
  }

  public final void printVerbose() {
    System.out.println(this);
  }

  public void compMove() {
    //TODO
  }
}
