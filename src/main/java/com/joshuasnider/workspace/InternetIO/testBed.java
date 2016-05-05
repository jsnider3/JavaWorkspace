package com.joshuasnider.workspace.internetio;

import java.awt.image.BufferedImage;
import java.io.*;
import java.net.*;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.ImageWriter;
import javax.imageio.metadata.IIOMetadataNode;
import javax.imageio.stream.ImageInputStream;
import javax.swing.*;
import javax.swing.text.html.HTMLDocument.Iterator;

public class testBed {

  public static BufferedImage image;
  public static URL imagesource;
  public static File tempfile;

  public static void main(String[] args) {
    getImage();
    showImage(image);
    saveImage(image);

  }

  private static void getImage() {
    try {
      imagesource = new URL(
        "http://pinkie.ponychan.net/chan/files/src/132287935236.gif");
      /* Test Images:
       * http://www.cs.gmu.edu/~sean/sean.jpg
       * http://pinkie.ponychan.net/chan/files/src/132287935236.gif
      */
      //ImageIO.write(image, "gif", outputfile);
      image = ImageIO.read(imagesource);
    } catch (MalformedURLException e) {
      e.printStackTrace();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  private static void showImage(BufferedImage image) {
    ImageIcon icon = new ImageIcon(image);
    JLabel label = new JLabel(icon, JLabel.CENTER);
    JOptionPane.showMessageDialog(null, label, "icon", -1);
    saveImage(image);
  }

  private static void saveImage(BufferedImage image) {
    try {
      //File outputfile = new File("saved.gif");
      //ImageIO.write(ImageIO.read(url), "gif", outputfile);
      ReadableByteChannel in = Channels.newChannel(imagesource.openStream());
      FileOutputStream out = new FileOutputStream("saved.gif");//+//detectFormat());
      out.getChannel().transferFrom(in, 0, 1 << 24);
      //outputfile.write(icon.toString());
      /*This returns the error message.
       * Exception in thread "main" java.lang.ClassCastException: sun.awt.image.ToolkitImage cannot be cast to java.awt.image.RenderedImage
       *    at Anima.main(Anima.java:25)
       */
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  private static String detectFormat() {
    String imagetitle = imagesource.toString();
    int len = imagetitle.length();
    return  imagetitle.substring(len-3,len);
  }

  public static void GIFtest() {
    Object input = new File("saved.gif");
    // or Object input = new FileInputStream("animated.gif");
    ImageInputStream stream = null;
    try {
      stream = ImageIO.createImageInputStream(input);
      java.util.Iterator<ImageReader> readers = ImageIO.getImageReaders(stream);
      if (!readers.hasNext())
        throw new RuntimeException("no image reader found");
      ImageReader reader = (ImageReader) readers.next();
      reader.setInput(stream); // don't omit this line!
      int n = 0;
      n = reader.getNumImages(true);
      // don't use false!
      System.out.println("numImages = " + n);
      for (int i = 0; i < n; i++) {
        BufferedImage image = null;
        image = reader.read(i);
        System.out.println("image[" + i + "] = " + image);
        stream.close();
      }
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }

}
