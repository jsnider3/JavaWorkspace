/**
 * Test class for Board.java.
 *
 * @author Josh Snider
 */

package com.joshuasnider.workspace.dietraversal;

import static org.junit.Assert.*;

import java.awt.Point;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

public class BoardTest {

  @Test
  public void testConstructor() {
    List<List<Integer>> source = makeThreeByThree();
    Board board = new Board(source);
    assertEquals(board.getColumns(), 3);
    assertEquals(board.getRows(), 3);
  }

  @Test
  public void testGetDirection() {
    List<List<Integer>> source = makeThreeByThree();
    Board board = new Board(source);
    assertEquals(Die.Side.EAST, board.getDirection(new Point(0, 0),
                                                   new Point(1, 0)));
    assertEquals(Die.Side.WEST, board.getDirection(new Point(1, 0),
                                                   new Point(0, 0)));
    assertEquals(Die.Side.SOUTH, board.getDirection(new Point(0, 0),
                                                   new Point(0, 1)));
    assertEquals(Die.Side.NORTH, board.getDirection(new Point(0, 1),
                                                   new Point(0, 0)));
  }

  @Test
  public void testGetValue() {
    List<List<Integer>> source = makeThreeByThree();
    Board board = new Board(source);
    Point point = new Point(0, 0);
    assertEquals(board.getValue(point), 0);
    int index = 0;
    for (int row = 0; row < 3; row++) {
      for (int col = 0; col < 3; col++) {
        point = new Point(col, row);
        assertEquals(board.getValue(point), index);
        index++;
      }
    }
  }

  public static List<List<Integer>> makeThreeByThree() {
    List<List<Integer>> source = new ArrayList<List<Integer>>();
    source.add(new ArrayList<Integer>());
    source.get(0).add(0);
    source.get(0).add(1);
    source.get(0).add(2);
    source.add(new ArrayList<Integer>());
    source.get(1).add(3);
    source.get(1).add(4);
    source.get(1).add(5);
    source.add(new ArrayList<Integer>());
    source.get(2).add(6);
    source.get(2).add(7);
    source.get(2).add(8);
    return source;
  }
}

