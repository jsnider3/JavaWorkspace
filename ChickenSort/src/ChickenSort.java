import java.util.Random;

/* ChickenSort is a quite stupid sorting algorithm
 * invented by Josh Snider and David Freelan in 2013.
 * The pseudocode is as follows
 * 
 * while array A is unsorted:
 * 	 Pick two elements of A
 * 	 If they're out-of-order, swap them.
 * 
 * I wrote this code in an attempt to discover the 
 * algorithmic complexity empirically. 
 */

public class ChickenSort {
	
	public static int sort(int[] aArray){
		int tComparisons = 0;
		Random tRNG = new Random();
		int tSize = aArray.length;
		while(!isSorted(aArray)){
			tComparisons++;
			int tLow = tRNG.nextInt(tSize);
			int tHigh = tRNG.nextInt(tSize);
			while(tHigh == tLow)
				tHigh = tRNG.nextInt(tSize);
			if(tHigh < tLow)
			{
				int tSwap = tHigh;
				tHigh = tLow;
				tLow = tSwap;
			}
			if(aArray[tHigh] < aArray[tLow]){
				int tSwap = aArray[tHigh];
				aArray[tHigh] = aArray[tLow];
				aArray[tLow] = tSwap;
			}
		}
		return tComparisons;
	}
	
	public static boolean isSorted(int[] aArray){
		for(int tIndex = 0; tIndex < aArray.length - 1; tIndex++)
		{
			if(aArray[tIndex] > aArray[tIndex + 1])
			{
				return false;
			}
		}
		return true;
	}
	
	public static void main(String[] args){
		int[] tArr = {5,4,3,2,1,0,-1,-2,-3,-4};
		double tSum = 0;
		int tCount = 0;
		while(true)
		{
			tCount++;
			tSum += ChickenSort.sort(tArr.clone());
			System.out.println(tCount + ": " + tSum/tCount);
		}
	}
	
}
