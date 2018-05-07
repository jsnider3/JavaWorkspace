/**
 * XKCD is a webcomic by Randall Munroe. It's funny, I swear.
 *
 * @author: Josh Snider
 */

package com.joshuasnider.workspace.comicgetter;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.util.Iterator;
import org.jsoup.Jsoup;

public class XKCDImageGetter extends ComicGetter {

  public static void main(String[] args) {
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

  public String getDest(String index) {
    String src = getSrc(index);
    if (src != null){
      return getDir() + index + getSrc(index).substring(29);
    }
    else {
      return null;
    }
  }

  public String getName() {
    return "XKCD";
  }

  /**
   * Get the index of the newest xkcd comic.
   */
  public int getNewestComic() throws Exception {
    String html = Jsoup.connect("http://www.xkcd.com").get().html();
    int num = html.indexOf("Permanent link to this comic");
    num = html.indexOf("com/", num) + 4;
    int end = html.indexOf("/", num);
    int comicnumber = Integer.parseInt(html.substring(num, end));
    return comicnumber;
  }

  /**
   * Get the image URL for the given comic number.
   */
  public static String getHTML(String index) {
    String fileLoc = null;
    try {
      String input = Jsoup.connect("http://www.xkcd.com/" + index).get().html();
      int start = input.indexOf("https://imgs.xkcd.com/comics");
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
      tofrom[0] = getSrc(index);
      tofrom[1] = getDest(index);
    }
    return tofrom;
  }

  public String getSrc(String index) {
    return getHTML(index);
  }

  private class ComicIterator implements Iterator<String> {

    private String current = "1";

    @Override
    public boolean hasNext() {
      return Integer.parseInt(current) <= newest;
    }

    @Override
    public String next() {
      String ret = current;
      String next = null;
      int num = Integer.parseInt(current);
      num += 1;
      if (num == 404) {
        num += 1;
      }
      current = Integer.toString(num);
      return ret;
    }

  }

  public Iterator<String> iterator() {
    return new ComicIterator();
  }

}
