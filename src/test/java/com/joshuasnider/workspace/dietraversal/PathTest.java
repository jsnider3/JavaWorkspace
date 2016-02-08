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
    source.get(2).add(0);
    Board board = new Board(source);
    Die die = new Die(0, 5, 4, 3, 1, 2);
    Path path = new Path(board, die);
    path.addPoint(new Point(1, 0));
    path.addPoint(new Point(2, 0));
    path.addPoint(new Point(2, 1));
    path.addPoint(new Point(2, 2));
    System.out.println(path);
    assertTrue(path.reachesEnd());
  }

}
