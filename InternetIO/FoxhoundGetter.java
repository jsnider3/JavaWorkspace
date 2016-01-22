/**
 * The Last Days of Foxhound is a webcomic exploring
 *  what Liquid Snake and Foxhound must have been doing before
 *  the plot of Metal Gear Solid.
 *
 * @author: Josh Snider
 */

public class FoxhoundGetter extends ComicGetter {

  public static void main(String[] args) {
    for (int i = 1; i <= 500; i++) {
      String image = getImageName(i);
      String foxhound = "http://www.doctorshrugs.com/foxhound/images/" + image;
      String file = "Webcomics/Foxhound/" + image;
      saveImage(foxhound, file);
    }
  }

  /**
   * Get the comic's base filename. (Wording?)
   * @TODO: Broken for #200 and guest comics.
   */
  public static String getImageName(int index) {
    return  "foxhound_" + String.format("%03d", index) + ".png";
  }

}
