/**
 * Schlock Mercenary is a daily webcomic about a band of mercenaries
 *  with a spaceship.
 *
 * @author: Josh Snider
 */

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

public class SchlockGetter extends ComicGetter {

  public static void main(String[] args) {
    new File("Webcomics/Schlock").mkdirs();
    new SchlockGetter().getAll();
  }

  public String getFirst() {
    return "20000612";
  }

  /**
   * This code generates FileNotFoundExceptions  on Sundays with multiple images.
   */
  public String getNext(String index) {
    String next = getNextDay(index, "yyyyMMdd");
    if (next.compareTo(getToday("yyyyMMdd")) > 0) {
      next = null;
    }
    return next;
  }

  public String[] getToFrom(String index) {
    String[] tofrom = new String[2];
    tofrom[0] = "http://static.schlockmercenary.com/comics/schlock" + index + ".png";
    tofrom[1] = "Webcomics/Schlock/" + index + ".png";
    return tofrom;
  }

}
