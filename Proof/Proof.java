//(C) 2011 Josh Snider
public class Proof {
	public static void main(String[] args) throws InterruptedException {
		System.out.println("This program is intended to prove that Integer.MIN_VALUE is actually higher than Integer.MAX_VALUE.");
		System.out.println("Integer.MIN_VALUE is equal to " + Integer.MIN_VALUE);
		System.out.println("Integer.MAX_VALUE is equal to " + Integer.MAX_VALUE);
		System.out.println("If MIN_VALUE is lower than MAX_VALUE than MIN_VALUE-1 is lower than MAX_VALUE");
		System.out.println("Lets test that.");
		int min=Integer.MIN_VALUE-1;
		//2147483647
		System.out.println("MIN_VALUE - 1 ="+ min);
		System.out.println("Because MIN_VALUE - 1 is not less than then MAX_VALUE we can conclude that MAX_VALUE is less than MIN_VALUE.");
		System.out.println("QED");
	}
}
