/**
 * Saturday Morning Breakfast Cereal is sort of like XKCD,
 *  but by Zach Weinersmith.
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
import java.util.ArrayList;
import java.util.List;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class SMBCGetter extends ComicGetter {
	private List<String> archive;

  public static void main(String[] args) {
    new SMBCGetter().getAll();
  }
  
  public SMBCGetter() {
    archive = new ArrayList<String>();
    try {
      Document doc = Jsoup.connect("https://smbc-comics.com/comic/archive").get();
      for (Element e : doc.select("option"))
      {
        archive.add(e.attr("value"));
        System.out.println(e.attr("value"));/*
		  
        String date = e.getElementsByClass("comic-archive-date").get(0).html();
        try {
          Date d = new SimpleDateFormat("MMM dd, yyyy").parse(date);
          date = new SimpleDateFormat("yyyyMMdd").format(d);
          String link = e.select("a").get(0).attr("href");
          archive.put(date, link);
        } catch (ParseException ex) {ex.printStackTrace();}*/
      }
		} catch (IOException ex) {ex.printStackTrace();}
  }

  public String getDest(String index) {
    return getDir() + index + ".jpg";
  }

  public String getName() {
    return "SMBC";
  }

  /**
   * Get the image URL for the given comic.
   */
  public String getSrc(String index) {
    String src = null;
    try {
      String page = "https://www.smbc-comics.com/comic/" + index;
      Document doc = Jsoup.connect(page).get();
      Element comic_body = doc.select("div#cc-comicbody").get(0);
      src = "https://www.smbc-comics.com" + comic_body.select("img").get(0).attr("src");
      System.out.println(src);
      //TODO Add mouseover text.
      //TODO Need to URLEncode src.
    } catch (Exception ex) {ex.printStackTrace();}
    return src;
  }

  public Iterator<String> iterator() {
    return archive.iterator();
  }

}
