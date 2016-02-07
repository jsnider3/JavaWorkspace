/**
 * Test class for Board.java.
 *
 * @author Josh Snider
 */

package com.joshuasnider.workspace.dietraversal;

import static org.junit.Assert.*;

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

