/**
 * Schlock Mercenary is a daily webcomic about a band of mercenaries
 *  with a spaceship.
 *
 * @author: Josh Snider
 */

import java.io.FileNotFoundException;
import java.io.IOException;

public class SchlockGetter extends ComicGetter {

  public static void main(String[] args) {
    /*
     * This code generates FileNotFoundExceptions when currentComic equals 20000618,
     * 20000625, 20000702, 20000709, 20000716, 20000723, 20000730, 20000806, 20000813
     * 20000820, 20000827, 20000903. It probably continues but I stopped it at 20000907.
     * All of those days are Sundays and in all cases the comic can be reached by adding
     * an a after the number in the hyperlink.
     */
    String max = getNewestSchlockComic();
    String currentComic = "20000612";
    while ((max.compareTo(currentComic)) != -1) {
      String src = "http://static.schlockmercenary.com/comics/schlock"+currentComic+".png";
      saveImage(src, currentComic + ".png");
      System.out.println(currentComic);
      currentComic = getNextDay(currentComic);
    }
  }

  private static String getNewestSchlockComic() {
    //TODO return today
    return "20111230";
  }

}
