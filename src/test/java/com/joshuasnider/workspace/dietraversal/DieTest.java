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
}

