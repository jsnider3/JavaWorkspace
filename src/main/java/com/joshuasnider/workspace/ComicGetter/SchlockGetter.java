/**
 * Schlock Mercenary is a daily webcomic about a band of mercenaries
 *  with a spaceship.
 *
 * @author: Josh Snider
 */

package com.joshuasnider.workspace.comicgetter;

import java.io.File;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collections;
import java.util.Iterator;

public class SchlockGetter extends ComicGetter {

  public static void main(String[] args) {
    new SchlockGetter().getAll();
  }

  public String getDest(String index) {
    return getDir() + index + ".png";
  }

  @Override
  public String getName() {
    return "Schlock";
  }

  public String[] getToFrom(String index) {
    String[] tofrom = new String[2];
    tofrom[0] = getSrc(index);
    tofrom[1] = getDest(index);
    return tofrom;
  }

  public String getSrc(String index) {
    return "http://static.schlockmercenary.com/comics/schlock" + index + ".png";
  }

  private class ComicIterator implements Iterator<String> {

    private Calendar index = null;

    public ComicIterator() throws ParseException {
      index = Calendar.getInstance();
      index.setTime(new SimpleDateFormat("yyyyMMdd").parse("20000612"));
    }

    @Override
    public boolean hasNext() {
      return index.compareTo(Calendar.getInstance()) <= 0;
    }

    /**
     * This code generates FileNotFoundExceptions on Sundays with multiple images.
     */
    @Override
    public String next() {
      SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMdd");
      String ret = dateFormat.format(index.getTime());
      index.add(Calendar.DATE, 1);
      return ret;
    }

  }

  public Iterator<String> iterator() {
    try {
      return new ComicIterator();
    } catch (ParseException e) {
      e.printStackTrace();
      return Collections.emptyIterator();
    }
  }

}
