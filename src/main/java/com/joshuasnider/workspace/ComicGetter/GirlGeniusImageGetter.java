/**
 * Girl Genius is a webcomic by Phil Foglio about a lost princess.
 *
 * @author: Josh Snider
 */

package com.joshuasnider.workspace.comicgetter;

import java.io.File;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.select.Elements;
import java.text.SimpleDateFormat;
import java.util.Calendar;

public class GirlGeniusImageGetter extends ComicGetter {

  public static String home = "http://www.girlgeniusonline.com";
  public static String title = home + "/ggmain/strips/ggmain";

  public static void main(String[] args) {
    new File("Webcomics/GirlGenius").mkdirs();
    new GirlGeniusImageGetter().getAll();
  }

  public String getFirst() {
    return "20021104";
  }

  public String getNext(String index) {
    String date = index;
    try {
      Calendar cal = Calendar.getInstance();
      cal.setTime(new SimpleDateFormat("yyyyMMdd").parse(date));
      switch (cal.get(Calendar.DAY_OF_WEEK)) {
        case Calendar.FRIDAY:
          date = getNextDay(date, "yyyyMMdd");
        case Calendar.MONDAY:
        case Calendar.WEDNESDAY:
        case Calendar.SATURDAY:
          date = getNextDay(date, "yyyyMMdd");
        case Calendar.TUESDAY:
        case Calendar.THURSDAY:
        case Calendar.SUNDAY:
          date = getNextDay(date, "yyyyMMdd");
          break;
      }
    } catch (Exception e) {}
    if (date.compareTo(getToday("yyyyMMdd")) > 0) {
      date = null;
    }
    return date;
  }

  /**
   * Try to find the link to the double page if the given comic is one.
   * If it isn't or we can't find it, return null.
   */
  public String getDoublePage(String url) {
    String link = null
    try {
      Document doc = Jsoup.connect(url).get();
      Elements els = doc.select("a[href*=doublespreads]");
      if (els.size() > 0) {
        link = home + els.get(0).attr("href");
        link = link.substring(0, link.lastIndexOf('.')) + ".jpg";
      }
    } catch (Exception e) {
      System.err.println(url + " failed");
    }
    return link;
  }

  public String[] getToFrom(String index) {
    String[] tofrom = new String[2];
    tofrom[0] = title + index + ".jpg";
    tofrom[1] = "Webcomics/GirlGenius/" + index + ".jpg";
    String doublePage = getDoublePage(
      "http://www.girlgeniusonline.com/comic.php?date=" + index);
    if (doublePage != null) {
      tofrom[0] = doublePage;
    }
    return tofrom;
  }
}
