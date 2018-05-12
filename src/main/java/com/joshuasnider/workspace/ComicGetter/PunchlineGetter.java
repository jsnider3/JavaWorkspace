/**
 * The Punchline is Machismo is a webcomic, partially focusing on video games.
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
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.SortedMap;
import java.util.TreeMap;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class PunchlineGetter extends ComicGetter {
	private SortedMap<String, String> archive;

  public static void main(String[] args) {
    new PunchlineGetter().getAll();
  }
  
  public PunchlineGetter() {
    archive = new TreeMap<String, String>();
    try {
      Document doc = Jsoup.connect("http://thepunchlineismachismo.com/archive").get();
      for (Element e : doc.getElementsByClass("comic-list"))
      {
        String date = e.getElementsByClass("comic-archive-date").get(0).html();
        try {
          Date d = new SimpleDateFormat("MMM dd, yyyy").parse(date);
          date = new SimpleDateFormat("yyyyMMdd").format(d);
          String link = e.select("a").get(0).attr("href");
          archive.put(date, link);
        } catch (ParseException ex) {ex.printStackTrace();}
      }
		} catch (IOException ex) {ex.printStackTrace();}
  }

  public String getDest(String index) {
    return getDir() + index + ".jpg";
  }

  public String getName() {
    return "Punchline";
  }

  /**
   * Get the image URL for the given comic number.
   */
  public String getSrc(String index) {
    String src = null;
    try {
      String page = archive.get(index);
      Document doc = Jsoup.connect(page).get();
      Element comic_table = doc.getElementsByClass("comic-table").get(0);
      src = comic_table.select("img").get(0).attr("src");
    } catch (Exception ex) {ex.printStackTrace();}
    return src;
  }

  public Iterator<String> iterator() {
    return archive.keySet().iterator();//new ComicIterator();
  }

}
