package com.joshuasnider.workspace.games.checkers;

import java.util.ArrayList;

public class CheckersBoard {
	private String[][] state;
	private int score;
	private int recentMove;
	private boolean XJustMoved;

	public CheckersBoard(boolean b) {
	  state = new String[8][8];
		for (int x = 0; x < 8; x++) {
			state[x] = new String[8];
      for (int y = 0; y < 3; y++) {
        String spot = "-";
        if (x % 2 != y % 2) {
          spot = "w";
        }
        state[x][y] = spot;
      }
      for (int y = 3; y < 5; y++) {
        String spot = "-";
        if (x % 2 != y % 2) {
          spot = "+";
        }
        state[x][y] = spot;
      }
      for (int y = 5; y < 8; y++) {
        String spot = "-";
        if (x % 2 != y % 2) {
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

	public CheckersBoard(CheckersBoard t, int s) {
		//TODO
	}

  /**
   * Provides best move for X.
   */
	public final CheckersBoard max(ArrayList<CheckersBoard> input) {
		//TODO
		return this;
	}

  /**
   * Provides best move for O.
   */
	public final CheckersBoard min(ArrayList<CheckersBoard> input) {
		//TODO
		return this;
	}

	public final ArrayList<CheckersBoard> children() {
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
		//TODO
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

	public final boolean isOver() {
		//TODO
		return hasWon("Red") || hasWon("Black");
	}

	public final ArrayList<Short> getEmptySpots() {
		//TODO
		return null;
	}

	public final int score() {
		//TODO
		return 0;
	}

	public final String toString() {
		//TODO
    StringBuffer buf = new StringBuffer();
		for (int x = 0; x < 8; x++) {
      for (int y = 0; y < 8; y++) {
			  buf.append(state[x][y]);
      }
      buf.append("\n");
		}
		return buf.toString();
	}

	public final void printVerbose() {
		System.out.println(this);
		//TODO
	}

	public void compMove() {
		//TODO
	}
}
