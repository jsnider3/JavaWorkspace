/**
 * Test class for Path.java.
 *
 * @author Josh Snider
 */

package com.joshuasnider.workspace.dietraversal;

import static org.junit.Assert.*;

import java.awt.Point;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

public class PathTest {

  @Test
  public void testConstructor() {
    Board board = new Board(BoardTest.makeThreeByThree());
    Die die = new Die(1, 2, 3, 4, 5, 6);
    Path path = new Path(board, die);
    assertFalse(path.reachesEnd());
  }

  @Test
  public void testReachesEnd() {
    Board board = new Board(BoardTest.makeThreeByThree());
    Die die = new Die(1, 2, 3, 4, 5, 6);
    Path path = new Path(board, die);
    path.addPoint(new Point(0, 1));
    path.addPoint(new Point(0, 2));
    path.addPoint(new Point(1, 2));
    path.addPoint(new Point(2, 2));
    System.out.println(path);
    assertTrue(path.reachesEnd());
  }

}
