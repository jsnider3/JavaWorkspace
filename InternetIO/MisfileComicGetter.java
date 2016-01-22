import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.net.URLConnection;
import java.net.URL;

public class MisfileComicGetter extends ComicGetter{

  public static final String dir = "Comics/Misfile/";

  public static void main(String[] args){
    try {
      int max = getMax();
      for (int count = getMin() + 1; count <= max; count++) {
        saveImage("http://www.misfile.com/overlay.php?pageCalled="+count,dir+Integer.toString(count)+".jpeg");
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  public static int getMin() {//Get the lowest comic number I don't have.
    String[] list = new File(dir).list();
    if (list == null)
      return 0;
    int rtrn = 1;
    for (String str : list) {
      try {
        int num = Integer.parseInt(str.substring(0, str.indexOf(".")));
        if (num > rtrn) {
          rtrn = num;
        }
      } catch(Exception e) { }
    }
    return rtrn;
  }

  /**
   * Get the number of the most recent comic.
   */
  public static int getMax() throws Exception {
    URL url = new URL("http://www.misfile.com");
    URLConnection webpage = url.openConnection();
    BufferedReader in = new BufferedReader(new InputStreamReader(webpage.getInputStream()));
    String input;
    while ((input = in.readLine()) != null) {
      if (input.contains("<img src=\"overlay.php?pageCalled=2034\">")) {
        String str = input.substring(input.indexOf("pageCalled=") +
          "pageCalled=".length());
        String temp = "";
        for (String c : str.split("")){
          if ("0123456789".contains(c)) {
            temp += c;
          } else {
            in.close();
            return Integer.parseInt(temp);
          }
        }
      }
    }
    in.close();
    System.err.println("Could not get comic number. Using default.");
    return 2000;
  }
}
