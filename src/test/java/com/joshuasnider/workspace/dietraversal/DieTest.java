/**
 * Test class for Die.java.
 *
 * @author Josh Snider
 */

package com.joshuasnider.workspace.dietraversal;

import static org.junit.Assert.*;

import org.junit.Test;

public class DieTest {

  @Test
  public void testConstructor() {
    Die die = new Die(1, 2, 3, 4, 5, 6);
    assertEquals(die.getFace(Die.Side.TOP), 1);
    assertEquals(die.getFace(Die.Side.NORTH), 2);
    assertEquals(die.getFace(Die.Side.EAST), 3);
    assertEquals(die.getFace(Die.Side.SOUTH), 4);
    assertEquals(die.getFace(Die.Side.WEST), 5);
    assertEquals(die.getFace(Die.Side.BOTTOM), 6);
  }

  @Test
  public void testRotated() {
    Die die = new Die(1, 2, 3, 4, 5, 6);
    Die north = die.rotated(Die.Side.NORTH);
    Die south = die.rotated(Die.Side.SOUTH);
    Die east = die.rotated(Die.Side.EAST);
    Die west = die.rotated(Die.Side.WEST);
    assertEquals(die.getFace(Die.Side.TOP), south.getFace(Die.Side.SOUTH));
    assertEquals(die.getFace(Die.Side.TOP), east.getFace(Die.Side.EAST));
    assertEquals(die.getFace(Die.Side.TOP), west.getFace(Die.Side.WEST));
    assertEquals(die.getFace(Die.Side.TOP), north.getFace(Die.Side.NORTH));
  }
}

