
public class listchar {
  public static void main(String[] args) {
    for(char c = 127; c > 32; c--)
    //The characters < 32 are control characters
    //and can't be displayed properly.
    {
      System.out.println(c);
    }
  }
}
