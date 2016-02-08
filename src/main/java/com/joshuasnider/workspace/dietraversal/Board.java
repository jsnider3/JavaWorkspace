/**
 * This represents a game board with numbers on each square.
 * It's used in an interesting puzzle.
 *
 * @author Josh Snider
 */

package com.joshuasnider.workspace.dietraversal;

import java.awt.Point;
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

  public Point getBottomRight() {
    return new Point(getColumns() - 1, getRows() - 1);
  }

  public int getColumns() {
    return numcolumns;
  }

  /**
   * Get the direction one point is from its neighbor.
   */
  public static Die.Side getDirection(Point from, Point to) {
    if (from.getX() - to.getX() == 1) {
      return Die.Side.WEST;
    } else if (from.getX() - to.getX() == -1) {
      return Die.Side.EAST;
    } else if (from.getY() - to.getY() == 1) {
      return Die.Side.NORTH;
    } else if (from.getY() - to.getY() == -1) {
      return Die.Side.SOUTH;
    } else {
      throw new IllegalArgumentException("Not neigbhors");
    }
  }


  public int getRows() {
    return numrows;
  }

  public int getValue(Point point) {
    return board.get((int)point.getY()).get((int)point.getX());
  }
}
