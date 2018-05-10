/**
 * The Order of the Stick is a comedy webcomic by
 *  Rich Burlew set in a world that follows the rules of
 *  a pen-and-pencil rpg.
 *
 * @author: Josh Snider
 */

package com.joshuasnider.workspace.comicgetter;

import java.io.IOException;
import java.util.Iterator;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

public class OOTSGetter extends ComicGetter {

  public static void main(String[] args) {
    new OOTSGetter().getAll();
  }

  public String getDest(String index) {
    return getDir() + index + ".gif";
  }

  public String getName() {
    return "OOTS";
  }

  public String[] getToFrom(String index) {
    String[] tofrom = new String[2];
    tofrom[0] = getSrc(index);
    tofrom[1] = getDest(index);
    return tofrom;
  }

  public String getSrc(String index) {
	try {
    Document doc = Jsoup.connect(String.format("http://www.giantitp.com/comics/%s.html", index)).get();
    for (Element e : doc.select("img")) {
      if (e.attr("src").contains("comics")) {
        return String.format("http://www.giantitp.com%s", e.attr("src"));
      }
    }
  } catch (IOException e) {e.printStackTrace();}
    return null;
  }

  private class ComicIterator implements Iterator<String> {

    private int index = 1;
    private int newest;

    public ComicIterator() {
      try {
        Document doc = Jsoup.connect("http://www.giantitp.com/comics/oots.html").get();
        String comiclink = doc.getElementsByClass("ComicList").get(0).html();
        newest = Integer.parseInt(comiclink.split(" ")[0]);
      } catch (IOException e) {
        e.printStackTrace();
        newest = 1120;
      }
    }

    @Override
    public boolean hasNext() {
      return index <= newest;
    }

    @Override
    public String next() {
      String ret = String.format("oots%04d", index);
      index = index + 1;
      return ret;
    }

  }

  public Iterator<String> iterator() {
    return new ComicIterator();
  }

}
