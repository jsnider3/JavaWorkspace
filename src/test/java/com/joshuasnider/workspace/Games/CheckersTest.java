package com.joshuasnider.workspace.games.checkers;

import static org.junit.Assert.*;

import org.junit.Test;

public class CheckersTest {

  @Test
  public void test_win() {
    String[][] state = new String[8][8];
    for (int y = 0; y < 8; y++) {
      state[y] = new String[8];
      for (int x = 0; x < 3; x++) {
        String spot = "-";
        if (x % 2 != y % 2) {
          spot = "w";
        }
        state[y][x] = spot;
      }
      for (int x = 3; x < 8; x++) {
        String spot = "-";
        if (x % 2 != y % 2) {
          spot = "+";
        }
        state[y][x] = spot;
      }
    }
    assertTrue(new CheckersBoard(state, true).hasWon("w"));
    assertTrue(new CheckersBoard(state, true).isOver());
    state[7][7] = "b";
    assertFalse(new CheckersBoard(state, true).hasWon("w"));
    assertFalse(new CheckersBoard(state, true).isOver());
  }

}
