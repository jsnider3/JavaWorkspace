/**
 * The Last Days of Foxhound is a webcomic exploring
 *  what Liquid Snake and Foxhound must have been doing before
 *  the plot of Metal Gear Solid.
 *
 * @author: Josh Snider
 */

import java.io.File;

public class FoxhoundGetter extends ComicGetter {

  public static void main(String[] args) {
    new File("Webcomics/Foxhound").mkdirs();
    new FoxhoundGetter().getAll();
  }

  public String getFirst() {
    return "001";
  }

  public String getNext(String index) {
    int num = Integer.parseInt(index) + 1;
    String retval = null;
    if (num <= 500) {
      retval = String.format("%03d", num);
    }
    return retval;
  }

  public String[] getToFrom(String index) {
    String[] tofrom = new String[2];
    tofrom[0] = "http://www.doctorshrugs.com/foxhound/images/" + getImageName(index);
    tofrom[1] = "Webcomics/Foxhound/" + getImageName(index);
    return tofrom;
  }

  /**
   * Get the comic's base filename. (Wording?)
   * @TODO: Broken for #200 and guest comics.
   */
  public static String getImageName(String index) {
    return  "foxhound_" + index + ".png";
  }

}
