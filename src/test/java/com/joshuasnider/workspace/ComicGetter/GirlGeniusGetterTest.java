/**
 * Test class for GirlGeniusImageGetter.java.
 *
 * @Author: Josh Snider
 */

package com.joshuasnider.workspace.comicgetter;

import static org.junit.Assert.*;

import org.junit.Test;

public class GirlGeniusGetterTest {

  @Test
  public void testFirst() {
    GirlGeniusImageGetter genius = new GirlGeniusImageGetter();
    assertEquals(genius.getFirst(), "20021104");
  }

  @Test
  public void testNext() {
    GirlGeniusImageGetter genius = new GirlGeniusImageGetter();
    assertEquals(genius.getNext("20021104"), "20021106");
    assertEquals(genius.getNext("20021106"), "20021108");
    assertEquals(genius.getNext("20021108"), "20021111");
    assertTrue(genius.getNext(genius.getToday("yyyyMMdd")) == null);
  }

}


