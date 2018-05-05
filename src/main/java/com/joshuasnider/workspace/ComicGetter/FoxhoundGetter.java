/**
 * The Last Days of Foxhound is a webcomic exploring
 *  what Liquid Snake and Foxhound must have been doing before
 *  the plot of Metal Gear Solid.
 *
 * @author: Josh Snider
 */

package com.joshuasnider.workspace.comicgetter;

import java.util.Iterator;

public class FoxhoundGetter extends ComicGetter {

  public static void main(String[] args) {
    new FoxhoundGetter().getAll();
  }

  public String getName() {
    return "Foxhound";
  }

  public String[] getToFrom(String index) {
    String[] tofrom = new String[2];
    tofrom[0] = "http://www.doctorshrugs.com/foxhound/images/" + getImageName(index);
    tofrom[1] = getDir() + getImageName(index);
    return tofrom;
  }

  /**
   * Get the comic's base filename. (Wording?)
   * @TODO: Broken for #200 and guest comics.
   */
  public static String getImageName(String index) {
    return  "foxhound_" + index + ".png";
  }

  private class ComicIterator implements Iterator<String> {

    private int index = 1;

    @Override
    public boolean hasNext() {
      return index <= 500;
    }

    @Override
    public String next() {
      String ret = String.format("%03d", index);
      index = index + 1;
      return ret;
    }

  }

  public Iterator<String> iterator() {
    return new ComicIterator();
  }

}
