/**
 * Test the Chicken Sort implementation.
 *
 * @author Josh Snider
 */

package com.joshuasnider.workspace.ChickenSort;

import static org.junit.Assert.*;

import org.junit.Test;

public class ChickenTest {

  @Test
  public void makeUnsortedArrayTest(){
    int[] tArray = ChickenSort.makeUnsortedArray(5);
    for (int tIndex = 0; tIndex < 5; tIndex++) {
      assert tArray[tIndex] == 5 - tIndex;
    }
  }

  @Test
  public void ksortTest(){
    for (int tSorted = 1; tSorted < 5; tSorted++) {
      int[] tArray = ChickenSort.makeUnsortedArray(5);
      ChickenSort.ksort(tArray, tSorted);
      assert ChickenSort.isKSorted(tArray, tSorted);
    }
  }

}
