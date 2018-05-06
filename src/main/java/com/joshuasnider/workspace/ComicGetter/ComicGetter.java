/**
 * Base class for a variety of small scripts whose purpose is
 *  to download every episode of a webcomic. This was largely
 *  a hobby project during my freshmen year of college. I am
 *  currently refactoring it as a hobby.
 *
 * @Author: Josh Snider
 */

package com.joshuasnider.workspace.comicgetter;

import com.google.common.reflect.ClassPath;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Iterator;

public abstract class ComicGetter implements Iterable<String> {

  /**
   * Downloads every webcomic in this directory.
   */
  public static void main(String[] args) {
    try {
      Class cls = Class.forName("com.joshuasnider.workspace.comicgetter.ComicGetter");
      // returns the ClassLoader object associated with this Class.
      ClassLoader cLoader = cls.getClassLoader();
      ClassPath class_path = ClassPath.from(cLoader);
      System.out.println("ComicGetter.main");
      for (ClassPath.ClassInfo clas : class_path.getTopLevelClasses("com.joshuasnider.workspace.comicgetter")) {
        System.out.println("Class: " + clas.getSimpleName());
        Class loaded = clas.load();
        if (loaded != cls)
        {
          try {
            ComicGetter gtr = (ComicGetter)loaded.newInstance();
            gtr.getAll();
          } catch (ClassCastException e) {
            System.err.println("ERROR: Non-ComicGetter class in com.joshuasnider.workspace.comicgetter.ComicGetter: " + clas.getSimpleName());
          } catch (InstantiationException e) {
            System.err.println("ERROR: Could not instantiate class in com.joshuasnider.workspace.comicgetter.ComicGetter: " + clas.getSimpleName());
          } catch (Exception e) {
            System.err.println("ERROR: Failed to download " + clas.getSimpleName() + ".");
            e.printStackTrace();
          }
        }
      }
    } catch (Exception e) {
        e.printStackTrace();
    }
  }

  /**
   * Download every comic.
   */
  public void getAll() {
    new File(getDir()).mkdirs();
    for (String index : this) {
      System.out.println(index);
      String[] tofrom = getToFrom(index);
      if (tofrom != null && !(new File(tofrom[0]).exists())) {
        saveImage(tofrom[0], tofrom[1]);
      }
    }
  }

  /**
   * Get the directory for saving this webcomic.
   */
  public String getDir() {
    return "Webcomics" + File.separator + getName() + File.separator;
  }

  /**
   * Get the name of the webcomic in string form.
   */
  public abstract String getName();

  /**
   * Get the place to download the image and the place to save it.
   */
  public abstract String[] getToFrom(String index);

  /**
   * Get the day immediately after the given day.
   */
  public static final String getNextDay(String input, String fmt) {
    String result = input;
    try {
      SimpleDateFormat dateFormat = new SimpleDateFormat(fmt);
      Calendar cal = Calendar.getInstance();
      cal.setTime(dateFormat.parse(input));
      cal.add(Calendar.DATE, 1);
      result = dateFormat.format(cal.getTime());
    } catch (ParseException e) {
      e.printStackTrace();
    }
    return result;
  }

  /**
   * Get today as a formatted string.
   */
  public static final String getToday(String fmt) {
    SimpleDateFormat dateFormat = new SimpleDateFormat(fmt);
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
