import java.util.Scanner;
public class CountfromXtoY {
  public static void main(String[] args) {
    Scanner input = new Scanner(System.in);
    int x, y;
    String name = "";
    System.out.println("Enter the first number: ");
    x = input.nextInt();
    System.out.println("Enter the second number: ");
    y = input.nextInt();
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
