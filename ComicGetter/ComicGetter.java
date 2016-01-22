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
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;

public class ComicGetter {

  /**
   * Get the day immediately after the given day.
   */
  public static final String getNextDay(String input) {
    String result = input;
    try {
      SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMdd");
      Calendar cal = Calendar.getInstance();
      cal.setTime(dateFormat.parse(input));
      cal.add(Calendar.DATE, 1);
      result = dateFormat.format(cal.getTime());
    } catch (ParseException e) { }
    return result;
  }

  /**
   * Get today as a formatted string.
   */
  public static final String getToday() {
    SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMdd");
    Calendar cal = Calendar.getInstance();
    return dateFormat.format(cal.getTime());
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













