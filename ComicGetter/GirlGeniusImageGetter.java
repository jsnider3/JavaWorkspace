/**
 * Girl Genius is a webcomic by Phil Foglio about a lost princess.
 *
 * @author: Josh Snider
 */

public class GirlGeniusImageGetter extends ComicGetter{

  public static String title = "http://www.girlgeniusonline.com/ggmain/strips/ggmain";

  public static void main(String[] args){
    String date = getStartDate();
    while (date.compareTo(getToday()) <= 0) {
      String fileLoc = title + date + ".jpg";
      String saveName = date + ".jpg";
      saveImage(fileLoc, saveName);
      date = getNextDay(date);
      date = getNextDay(date);
      fileLoc = title + date + ".jpg";
      saveName = date + ".jpg";
      saveImage(fileLoc, saveName);
      date = getNextDay(date);
      date = getNextDay(date);
      fileLoc = title + date + ".jpg";
      saveName = date + ".jpg";
      saveImage(fileLoc, saveName);
      date = getNextDay(date);
      date = getNextDay(date);
      date = getNextDay(date);
    }
  }

  public static String getStartDate(){
    return "20021104";
  }

}
