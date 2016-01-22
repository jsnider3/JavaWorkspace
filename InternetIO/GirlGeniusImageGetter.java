/**
 * Girl Genius is a webcomic by Phil Foglio about a lost princess.
 *
 * @author: Josh Snider
 * @TODO: This code is bad and I feel bad.
 */

public class GirlGeniusImageGetter extends ComicGetter{

  public static String title = "http://www.girlgeniusonline.com/ggmain/strips/ggmain";//+.jpg

  public static void main(String[] args){
    String date = getStartDate();
    try {
      while (true) {
        String fileLoc = title + date + ".jpg";
        String saveName = fileLoc.substring(52);
        CommonFunctions.saveImage(fileLoc, saveName);
        date = getNextDay(date);
        date = getNextDay(date);
        fileLoc = title + date + ".jpg";
        saveName = fileLoc.substring(52);
        CommonFunctions.saveImage(fileLoc, saveName);
        date = getNextDay(date);
        date = getNextDay(date);
        fileLoc = title + date + ".jpg";
        saveName = fileLoc.substring(52);
        CommonFunctions.saveImage(fileLoc, saveName);
        date = getNextDay(date);
        date = getNextDay(date);
        date = getNextDay(date);
      }
    } catch(Exception e){
      System.out.println(date);
    }
  }

  public static String getStartDate(){
    return "20100104";
  }

}
