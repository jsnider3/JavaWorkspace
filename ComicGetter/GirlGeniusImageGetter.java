/**
 * Girl Genius is a webcomic by Phil Foglio about a lost princess.
 *
 * @author: Josh Snider
 */

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Calendar;

public class GirlGeniusImageGetter extends ComicGetter{

  public static String title = "http://www.girlgeniusonline.com/ggmain/strips/ggmain";

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

  public String[] getToFrom(String index) {
    String[] tofrom = new String[2];
    tofrom[0] = title + index + ".jpg";
    tofrom[1] = "Webcomics/GirlGenius/" + index + ".jpg";
    return tofrom;
  }
}
