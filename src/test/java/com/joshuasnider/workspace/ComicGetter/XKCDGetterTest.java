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
    assertEquals("1", xkcd.getFirst());
  }

  @Test
  public void testNext() {
    XKCDImageGetter xkcd = new XKCDImageGetter();
    assertEquals("2", xkcd.getNext("1"));
    assertEquals("405", xkcd.getNext("403"));
  }

}


