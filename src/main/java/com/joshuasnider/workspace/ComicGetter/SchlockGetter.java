/**
 * Schlock Mercenary is a daily webcomic about a band of mercenaries
 *  with a spaceship.
 *
 * @author: Josh Snider
 */

package com.joshuasnider.workspace.comicgetter;

import java.io.File;
import java.util.Iterator;

public class SchlockGetter extends ComicGetter {

  public static void main(String[] args) {
    new SchlockGetter().getAll();
  }

  public String getName() {
    return "Schlock";
  }

  public String[] getToFrom(String index) {
    String[] tofrom = new String[2];
    tofrom[0] = "http://static.schlockmercenary.com/comics/schlock" + index + ".png";
    tofrom[1] = getDir() + index + ".png";
    return tofrom;
  }

  private class ComicIterator implements Iterator<String> {

    private String index = "20000612";

    @Override
    public boolean hasNext() {
      return index.compareTo(getToday("yyyyMMdd")) <= 0;
    }

    /**
     * This code generates FileNotFoundExceptions on Sundays with multiple images.
     */
    @Override
    public String next() {
      String ret = index;
      index = getNextDay(index, "yyyyMMdd");
      return ret;
    }

  }

  public Iterator<String> iterator() {
    return new ComicIterator();
  }

}
