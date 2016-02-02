/**
 * Test class for XKCDImageGetter.java.
 *
 * @Author: Josh Snider
 */

package com.joshuasnider.workspace.comicgetter;

import static org.junit.Assert.*;

import org.junit.Test;

public class XKCDGetterTest {

  @Test
  public void testFirst() {
    XKCDImageGetter xkcd = new XKCDImageGetter();
    assertEquals(xkcd.getFirst(), "1");
  }

  @Test
  public void testNext() {
    XKCDImageGetter xkcd = new XKCDImageGetter();
    assertEquals(xkcd.getNext("1"), "2");
    assertEquals(xkcd.getNext("403"), "405");
  }

}


