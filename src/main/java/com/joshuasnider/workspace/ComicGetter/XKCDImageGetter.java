/**
 * XKCD is a webcomic by Randall Munroe. It's funny, I swear.
 *
 * @author: Josh Snider
 */

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;

public class XKCDImageGetter extends ComicGetter {

  public static void main(String[] args) {
    new File("Webcomics/XKCD").mkdirs();
    new XKCDImageGetter().getAll();
  }

  private final int newest;

  public XKCDImageGetter() {
    int recent = -1;
    try {
      recent = getNewestComic();
    } catch (Exception e) { }
    newest = recent;
  }

  public String getFirst() {
    return "1";
  }

  public String getNext(String index) {
    String next = null;
    int num = Integer.parseInt(index);
    num += 1;
    if (num == 404) {
      num += 1;
    }
    if (num <= newest) {
      next = Integer.toString(num);
    }
    return next;
  }

  /**
   * Get the index of the newest xkcd comic.
   */
  public int getNewestComic() throws Exception {
    URL xkcd = new URL("http://www.xkcd.com/");
    URLConnection webpage = xkcd.openConnection();
    BufferedReader in = new BufferedReader(
      new InputStreamReader(webpage.getInputStream()));
    String input;
    StringBuffer html = new StringBuffer();
    while ((input = in.readLine()) != null) {
      html.append(input);
    }
    input = html.toString();
    int num = input.indexOf("|&lt");
    num = input.indexOf("href=", num);
    int end = input.indexOf("/", num + 7);
    int comicnumber = Integer.parseInt(input.substring(num + 7, end));
    return comicnumber + 1;
  }

  /**
   * Get the image URL for the given comic number.
   */
  public static String getHTML(String index) {
    String fileLoc = null;
    try {
      URL url = new URL("http://www.xkcd.com/" + index);
      URLConnection webpage = url.openConnection();
      BufferedReader in = new BufferedReader(
        new InputStreamReader(webpage.getInputStream()));
      String input;
      StringBuffer html = new StringBuffer();
      while ((input = in.readLine()) != null) {
        html.append(input);
      }
      input = html.toString();
      int start = input.indexOf("http://imgs.xkcd.com/comics");
      int end = input.indexOf('<', start);
      fileLoc = input.substring(start, end);
    } catch (Exception e) {}
    return fileLoc;
  }

  public String[] getToFrom(String index) {
    String[] tofrom = null;
    String html = getHTML(index);
    if (html != null) {
      tofrom = new String[2];
      tofrom[0] = getHTML(index);
      tofrom[1] = "Webcomics/XKCD/" + index + tofrom[0].substring(28);
    }
    return tofrom;
  }
}














