package com.joshuasnider.workspace.games.checkers;

import java.util.ArrayList;
import java.util.List;

public class CheckersBoard {

  /**
   * An 8x8 grid with checkers on it. White is at the top.
   */
  private String[][] state;

  /**
   * Higher values are good for w, lower for b. 0 is tie.
   */
  private int score;
  private int recentMove;
  private boolean BToMove;

  public CheckersBoard(boolean b) {
    state = makeStartingState();
    BToMove = b;
    score = score();
  }

  /**
   * Copy a game state with the given person's turn.
   */
  public CheckersBoard(String[][] array, boolean b) {
    state = new String[8][8];
    for (int x = 0; x < 8; x++) {
      state[x] = array[x].clone();
    }
    BToMove = b;
    score = score();
  }

  /**
   * Provides best move for white.
   */
  public final CheckersBoard max(List<CheckersBoard> input) {
    int best = -2;
    CheckersBoard next = null;
    for (CheckersBoard move : input) {
      if (move.score() > best) {
        best = move.score();
        next = move;
      }
    }
    return next;
  }

  /**
   * Get a board laid out for a new game.
   */
  private String[][] makeStartingState() {
    String[][] start = new String[8][8];
    for (int y = 0; y < 8; y++) {
      start[y] = new String[8];
    }
    for (int y = 0; y < 8; y++) {
      for (int x = 0; x < 3; x++) {
        String spot = "-";
        if (isPlayableSquare(x, y)) {
          spot = "w";
        }
        start[x][y] = spot;
      }
      for (int x = 3; x < 5; x++) {
        String spot = "-";
        if (isPlayableSquare(x, y)) {
          spot = "+";
        }
        start[x][y] = spot;
      }
      for (int x = 5; x < 8; x++) {
        String spot = "-";
        if (isPlayableSquare(x, y)) {
          spot = "b";
        }
        start[x][y] = spot;
      }
    }
    return start;
  }

  /**
   * Provides best move for black.
   */
  public final CheckersBoard min(List<CheckersBoard> input) {
    int best = 2;
    CheckersBoard next = null;
    for (CheckersBoard move : input) {
      if (move.score() < best) {
        best = move.score();
        next = move;
      }
    }
    return next;
  }

  /**
   * Get the possible next game states.
   * Jumping is mandatory if possible.
   * If multiple jumps are possible, any of them is legal.
   * Otherwise, pick a checkers place and move it diagonally.
   */
  public final List<CheckersBoard> children() {
    List<Move> moves = new ArrayList<Move>();
    //TODO
    //Find the positions of the current player's pieces.
    //Find the list of jump moves for them.
    if (moves.size() == 0) {
      //If there are none, get the list of simple moves.
    }
    List<CheckersBoard> children = new ArrayList<CheckersBoard>();
    for (Move move : moves) {
      //For the list of moves, create a checkers board with it.
    }
    return children;
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

  /**
   * What is the min-max score of this position.
   */
  public final int score() {
    if (hasWon("w")) {
      return 1;
    } else if (hasWon("b")) {
      return -1;
    } else {
      //TODO
      return 0;
    }
  }

  public final String toString() {
    StringBuffer buf = new StringBuffer();
    buf.append("-");
    for (int x = 0; x < 8; x++) {
      buf.append(x);
    }
    buf.append("\n");
    for (int x = 0; x < 8; x++) {
      buf.append(x);
      for (int y = 0; y < 8; y++) {
        buf.append(state[x][y]);
      }
      if (x != 8 - 1) {
        buf.append("\n");
      }
    }
    return buf.toString();
  }

  /**
   * Have the computer make its move.
   */
  public void compMove() {
    //TODO
  }
}
