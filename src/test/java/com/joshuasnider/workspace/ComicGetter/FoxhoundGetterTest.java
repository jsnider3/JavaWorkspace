/**
 * Test class for FoxhoundGetter.java.
 *
 * @Author: Josh Snider
 */

package com.joshuasnider.workspace.comicgetter;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

public class FoxhoundGetterTest {

  @Test
  public void testFirst() {
    FoxhoundGetter fox = new FoxhoundGetter();
    assertEquals("001", fox.iterator().next());
  }

  @Test
  public void testNext() {
    FoxhoundGetter fox = new FoxhoundGetter();
    List<String> contents = new ArrayList<>();
    fox.iterator().forEachRemaining(contents::add);
    assertTrue(contents.contains("001"));
    assertEquals("002", contents.get(contents.indexOf("001") + 1));
    assertTrue(contents.contains("499"));
    assertEquals("500", contents.get(contents.indexOf("499") + 1));
    assertEquals("500", contents.get(contents.size() - 1));
  }

}
