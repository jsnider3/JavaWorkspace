/** Print a dancing man to the terminal.
 *
 */
public class DancingMan {

  public static void main(String[] args) {
    int dancepose = 1;
    while (true)
    {
      if (dancepose == 3) {
        try {
          Thread.sleep(125);
        } catch (InterruptedException e) {
          e.printStackTrace();
        }
        System.out.println("<o>\n| \n/<\n");
        dancepose = 1;
      }
      if (dancepose == 1) {
        try {
          Thread.sleep(125);
        } catch (InterruptedException e) {
          e.printStackTrace();
        }
        System.out.println(" o>\n<|  \n >\\\n");
        dancepose = 2;
      }
      if (dancepose == 2) {
        try {
          Thread.sleep(125);
        } catch (InterruptedException e) {
          e.printStackTrace();
        }
        System.out.println(" o\n<|>\n /<\n");
        dancepose = 3;
      }
    }
  }
}
