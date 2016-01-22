/**
 * Base class for a variety of small scripts whose purpose is
 *  to download every episode of a webcomic. This was largely
 *  a hobby project during my freshmen year of college. I am
 *  currently refactoring it as a hobby.
 *
 * @Author: Josh Snider
 */
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;

public class ComicGetter {

  /**
   * Get the day immediately after the given day.
   * @TODO: This is horrible, use a library.
   */
  public static final String getNextDay(String input){
    int[] array = new int[]{31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    int year = Integer.parseInt(input.substring(0, 4));
    int month = Integer.parseInt(input.substring(4, 6));
    int day = Integer.parseInt(input.substring(6));
    day++;
    if (month == 2 && year % 4 == 0) {
      if (day > 29) {
        month++;
        day = 1;
      }
    }
    else if (day > array[month - 1]) {
      month++;
      day = 1;
    }

    if (month == 13) {
      month = 1;
      year++;
    }
    String result;
    if (month < 10) {
      result = Integer.toString(year) + "0" + Integer.toString(month);
    } else {
      result = Integer.toString(year) + Integer.toString(month);
    }
    if (day < 10) {
      result = result + "0" + Integer.toString(day);
    } else{
      result = result + Integer.toString(day);
    }
    return result;
  }

  /**
   * Get an image from the URL at fileLoc and save it as title.
   */
  public static void saveImage(String fileLoc, String title){
    try {
      ReadableByteChannel in1 = Channels.newChannel(
        new URL(fileLoc).openStream());
      FileOutputStream out = new FileOutputStream(title);
      out.getChannel().transferFrom(in1, 0, 1 << 24);
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}













