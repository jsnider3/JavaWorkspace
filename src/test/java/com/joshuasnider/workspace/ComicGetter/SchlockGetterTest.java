/**
 * Test class for SchlockGetter.java.
 *
 * @Author: Josh Snider
 */

package com.joshuasnider.workspace.comicgetter;

import static org.junit.Assert.*;

import org.junit.Test;

public class SchlockGetterTest {

  @Test
  public void testFirst() {
    SchlockGetter schlock = new SchlockGetter();
    assertEquals(schlock.getFirst(), "20000612");
  }

  @Test
  public void testNext() {
    SchlockGetter schlock = new SchlockGetter();
    assertEquals(schlock.getNext("20000612"), "20000613");
    assertEquals(schlock.getNext("20120815"), "20120816");
    assertEquals(schlock.getNext(schlock.getToday("yyyyMMdd")), null);
  }

}


