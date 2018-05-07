/**
 * XKCD is a webcomic by Randall Munroe. It's funny, I swear.
 *
 * @author: Josh Snider
 */

package com.joshuasnider.workspace.comicgetter;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.io.IOException;
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
    } catch (IOException e) { }
    newest = recent;
  }

  public String getDest(String index) {
    String src = getSrc(index);
    if (src != null){
      return String.format("%s%04d_%s", getDir(), Integer.parseInt(index), src.substring(29));
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
  public int getNewestComic() throws IOException {
    String html = Jsoup.connect("http://www.xkcd.com").get().html();
    int num = html.indexOf("Permanent link to this comic");
    num = html.indexOf("com/", num) + 4;
    int end = html.indexOf("/", num);
    int comicnumber = Integer.parseInt(html.substring(num, end));
    return comicnumber;
  }

  public String[] getToFrom(String index) {
    String[] tofrom = null;
    String html = getSrc(index);
    if (html != null) {
      tofrom = new String[2];
      tofrom[0] = getSrc(index);
      tofrom[1] = getDest(index);
    }
    return tofrom;
  }

  /**
   * Get the image URL for the given comic number.
   */
  public String getSrc(String index) {
    String fileLoc = null;
    try {
      String input = Jsoup.connect("http://www.xkcd.com/" + index).get().html();
      int start = input.indexOf("https://imgs.xkcd.com/comics");
      int end = input.indexOf('<', start);
      fileLoc = input.substring(start, end);
      fileLoc = fileLoc.trim();
    } catch (IOException e) {}
    return fileLoc;
  }

  private class ComicIterator implements Iterator<String> {

    private int current = 1;

    @Override
    public boolean hasNext() {
      return current <= newest;
    }

    @Override
    public String next() {
      String ret = Integer.toString(current);
      current += 1;
      if (current == 404) {
        current += 1;
      }
      return ret;
    }

  }

  public Iterator<String> iterator() {
    return new ComicIterator();
  }

}
