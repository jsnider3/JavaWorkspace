
import java.util.Scanner;
public class Cipher {
  public static void main(String[] args) {
    Scanner input = new Scanner(System.in);
    System.out.println("Basic Cipher by Josh Snider");
    System.out.println("Do you want to encode or decode?");
    String text = input.nextLine();
    boolean encode = text.toLowerCase().equals("encode");
    System.out.println("Type in the text.");
    text = input.nextLine();
    System.out.println("What's the key?");
    int key = input.nextInt();
    if (encode)
    {
      for (char c : text.toCharArray()) {
        if (c >= 'a' && c <= 'z'){
          c += key;
          if (c > 122){
            c -= 26;
          }
        }
        else if ( c >= 'A' && c <= 'Z') {
          c += key;
          if (c > 90){
            c -= 26;
          }
        }
        key++;
        if(key > 25){
          key -= 26;
        }
        System.out.print(c);
      }
    } else {
      for (char c : text.toCharArray()) {
        if (c >= 'a' && c <= 'z') {
          c -= key;
          if(c < 97){
            c += 26;
          }
        }
        else if(c >= 'A' && c <= 'Z') {
          c -= key;
          if (c < 65) {
            c += 26;
          }
        }
        key++;
        if (key > 25) {
          key -= 26;
        }
        System.out.print(c);
      }
    }
    System.out.println("");
  }
}
