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

  public String getFirst() {
    return "1";
  }

  public String getName() {
    return "XKCD";
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
      tofrom[1] = getDir() + index + tofrom[0].substring(28);
    }
    return tofrom;
  }
}

