/** ChickenSort is a quite stupid sorting algorithm
 * invented by Josh Snider and David Freelan in 2013.
 * The pseudocode is as follows
 *
 * while array A is unsorted:
 *   Pick two elements of A
 *   If they're out-of-order, swap them.
 *
 * I wrote this code in an attempt to discover the
 * algorithmic complexity empirically.
 *
 * @author Josh Snider
 */

package com.joshuasnider.workspace.ChickenSort

import java.util.Random;

public class ChickenSort {

  public static int sort(int[] aArray) {
    int tComparisons = 0;
    Random tRNG = new Random();
    int tSize = aArray.length;
    while(!isSorted(aArray)){
      tComparisons++;
      int tLow = tRNG.nextInt(tSize);
      int tHigh = tRNG.nextInt(tSize);
      while(tHigh == tLow)
        tHigh = tRNG.nextInt(tSize);
      if (tHigh < tLow) {
        int tSwap = tHigh;
        tHigh = tLow;
        tLow = tSwap;
      }
      if (aArray[tHigh] < aArray[tLow]) {
        int tSwap = aArray[tHigh];
        aArray[tHigh] = aArray[tLow];
        aArray[tLow] = tSwap;
      }
    }
    return tComparisons;
  }

  public static boolean isKSorted(int[] aArray, int aKval) {
    for (int tIndex = 0; tIndex < aArray.length - aKval; tIndex += aKval)
    {
      if (aArray[tIndex] > aArray[tIndex + aKval])
      {
        return false;
      }
    }
    return true;
  }

  public static boolean isSorted(int[] aArray) {
    return isKSorted(aArray, 1);
  }

  public static int[] makeUnsortedArray(int aSize) {
    int[] tRetval = new int[aSize];
    for (int tIndex = 0; tIndex < aSize; tIndex++)
    {
      tRetval[tIndex] = aSize - tIndex;
    }
    return tRetval;
  }

  public static void ksort(int[] aArray, int aKval) {
    //TODO DEBUG FIXME
    while (!isKSorted(aArray, aKval)) {
      for (int tIndex = 0; tIndex < aArray.length - aKval; tIndex += aKval) {
        if (aArray[tIndex] > aArray[tIndex + aKval]) {
          int tSwap = aArray[tIndex + aKval];
          aArray[tIndex + aKval] = aArray[tIndex];
          aArray[tIndex] = tSwap;
        }
      }
    }
  }

  public static void printArray(int[] aArray){
    System.out.print("[");
    for (int tIndex = 0; tIndex < aArray.length - 1; tIndex++) {
      System.out.print(aArray[tIndex] + ", ");
    }
    System.out.println(aArray[aArray.length - 1] + "]");
  }

  public static void main(String[] args){

    for (int tSize = 4; tSize < 256; tSize = tSize * 2)
    {
      for (int tSort = 2; tSort < tSize; tSort = tSort * 2){
        double tSum = 0;
        for (int tCount = 0; tCount < 10000; tCount++)
        {
          int[] tArray = makeUnsortedArray(tSize);
          ksort(tArray, tSort);
          tSum += sort(tArray);
        }
        System.out.println(tSize + ", " + tSort + ", " + tSum/10000);
      }
    }
  }
}
