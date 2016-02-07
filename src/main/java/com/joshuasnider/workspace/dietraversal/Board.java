/**
 * This represents a game board with numbers on each square.
 * It's used in an interesting puzzle.
 *
 * @author Josh Snider
 */

package com.joshuasnider.workspace.dietraversal;

import java.util.ArrayList;
import java.util.List;

public class Board {

  private final int numcolumns;
  private final int numrows;
  private List<List<Integer>> board;

  public Board(List<List<Integer>> board) {
    this.board = new ArrayList<List<Integer>>();
    numrows = board.size();
    numcolumns = board.get(0).size();
    for (List<Integer> row : board) {
      if (row.size() != numcolumns) {
        throw new IllegalArgumentException("Non-rectangular board.");
      }
      this.board.add(new ArrayList<Integer>());
      for (Integer value : row) {
        this.board.get(this.board.size() - 1).add(value);
      }
    }
  }

  public int getColumns() {
    return numcolumns;
  }

  public int getRows() {
    return numrows;
  }
}
