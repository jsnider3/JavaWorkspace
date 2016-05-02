package com.joshuasnider.workspace.games.checkers;

import static org.junit.Assert.*;

import org.junit.Test;

public class CheckersTest {

  @Test
  public void test_win() {
    String[][] state = new String[8][8];
    for (int x = 0; x < 8; x++) {
      state[x] = new String[8];
      for (int y = 0; y < 3; y++) {
        String spot = "-";
        if (x % 2 != y % 2) {
          spot = "w";
        }
        state[x][y] = spot;
      }
      for (int y = 3; y < 8; y++) {
        String spot = "-";
        if (x % 2 != y % 2) {
          spot = "+";
        }
        state[x][y] = spot;
      }
    }
    assertTrue(new CheckersBoard(state, true).hasWon("w"));
    state[7][7] = "b";
    assertFalse(new CheckersBoard(state, true).hasWon("w"));
  }

}
