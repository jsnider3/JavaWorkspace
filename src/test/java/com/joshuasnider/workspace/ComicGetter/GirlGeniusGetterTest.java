/**
 * Test class for GirlGeniusImageGetter.java.
 *
 * @Author: Josh Snider
 */

package com.joshuasnider.workspace.comicgetter;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

public class GirlGeniusGetterTest {

  @Test
  public void testFirst() {
    GirlGeniusImageGetter genius = new GirlGeniusImageGetter();
    assertEquals("20021104", genius.iterator().next());
  }

  @Test
  public void testNext() {
    GirlGeniusImageGetter genius = new GirlGeniusImageGetter();
    List<String> contents = new ArrayList<>();
    genius.iterator().forEachRemaining(contents::add);
    assertTrue(contents.contains("20021106"));
    assertEquals("20021106", contents.get(contents.indexOf("20021104") + 1));
    assertTrue(contents.contains("20021108"));
    assertEquals("20021108", contents.get(contents.indexOf("20021106") + 1));
    assertTrue(contents.contains("20021111"));
    assertEquals("20021111", contents.get(contents.indexOf("20021108") + 1));
  }

}


