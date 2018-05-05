/**
 * Test class for SchlockGetter.java.
 *
 * @Author: Josh Snider
 */

package com.joshuasnider.workspace.comicgetter;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

public class SchlockGetterTest {

  @Test
  public void testFirst() {
    SchlockGetter schlock = new SchlockGetter();
    assertEquals("20000612", schlock.iterator().next());
  }

  @Test
  public void testNext() {
    SchlockGetter schlock = new SchlockGetter();
    List<String> contents = new ArrayList<>();
    schlock.iterator().forEachRemaining(contents::add);
    assertTrue(contents.contains("20000612"));
    assertEquals("20000613", contents.get(contents.indexOf("20000612") + 1));
    assertTrue(contents.contains("20120815"));
    assertEquals("20120816", contents.get(contents.indexOf("20120815") + 1));
    assertEquals(schlock.getToday("yyyyMMdd"), contents.get(contents.size() - 1));
  }

}


