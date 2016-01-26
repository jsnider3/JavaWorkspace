/**
 * One of the first programs I ever wrote,
 *  counts from x to y.
 *
 * @author Josh Snider
 */
import java.util.Scanner;

public class CountfromXtoY {

  public static void main(String[] args) {
    Scanner input = new Scanner(System.in);
    System.out.println("Enter the first number: ");
    int x = input.nextInt();
    System.out.println("Enter the second number: ");
    int y = input.nextInt();
    while (x != y) {
      System.out.println(x);
      if (x < y) {
        x++;
      } else {
        x--;
      }
    }
  }

}
