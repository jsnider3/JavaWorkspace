/**
 * Test class for XKCDImageGetter.java.
 *
 * @Author: Josh Snider
 */

package com.joshuasnider.workspace.comicgetter;

import static org.junit.Assert.*;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

public class XKCDGetterTest {

  @Test
  public void testFirst() {
    XKCDImageGetter xkcd = new XKCDImageGetter();
    assertEquals("1", xkcd.iterator().next());
  }

  @Test
  public void testNext() {
    XKCDImageGetter xkcd = new XKCDImageGetter();
    List<String> contents = new ArrayList<>();
    xkcd.iterator().forEachRemaining(contents::add);
    assertTrue(contents.contains("1"));
    assertEquals("2", contents.get(contents.indexOf("1") + 1));
    assertTrue(contents.contains("403"));
    assertEquals("405", contents.get(contents.indexOf("403") + 1));
  }

  @Test
  public void testSize() {
    ComicGetter comic = new XKCDImageGetter();
    List<String> contents = new ArrayList<>();
    comic.iterator().forEachRemaining(contents::add);
    assertTrue(contents.size() >= 1988);
  }

  @Test
  public void testConnection() {
    ComicGetter comic = new XKCDImageGetter();
    try {
      new URL(comic.getSrc("1")).openStream();
    } catch(Exception e) {
      e.printStackTrace();
      fail("Could not connect to " + comic.getSrc("1") + ".");
    }
  }

}
