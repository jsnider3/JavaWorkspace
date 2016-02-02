/**
 * Test class for FoxhoundGetter.java.
 *
 * @Author: Josh Snider
 */

package com.joshuasnider.workspace.comicgetter;

import static org.junit.Assert.*;

import org.junit.Test;

public class FoxhoundGetterTest {

  @Test
  public void testFirst() {
    FoxhoundGetter fox = new FoxhoundGetter();
    assertEquals(fox.getFirst(), "001");
  }

  @Test
  public void testNext() {
    FoxhoundGetter fox = new FoxhoundGetter();
    assertEquals(fox.getNext("001"), "002");
    assertEquals(fox.getNext("499"), "500");
    assertTrue(fox.getNext("500") == null);
  }

}


