package com.joshuasnider.workspace.internetio;

import java.io.*;
import java.net.*;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import javax.swing.*;

public class ImageGetter {

  public static URL imagesource;

  /* High-Level Design Intention Statement
   * Write a program that when given a URl
   * displays each image on the corresponding
   * website with a prompt asking the user
   * whether they want to delete or save the
   * images it finds on the webpage. The
   * website that I have in mind is 4chan.
   */

  /* If I have trouble working with a website
   * then I will simply copy the source code
   * into a text file and use that as input.
   */

  /* Product Requirements:
   * Html Lexer and Parser
   * Image format detector
   * Better user interface
   */
  public static void main(String[] args){
    getWebpage();
    getImage();
    //showImage();
    showAndSaveImage();
  }

  private static void getWebpage() {

  }

  private static void getImage(){
    try {
      imagesource = new URL("http://en.wikipedia.org/wiki/Hello_World");//"http://pinkie.ponychan.net/chan/files/src/132287935236.gif");
      /* Test Images:
       * http://www.cs.gmu.edu/~sean/sean.jpg
       * http://pinkie.ponychan.net/chan/files/src/132255438967.png
       * http://pinkie.ponychan.net/chan/files/thumb/132169880059s.jpg
       * http://pinkie.ponychan.net/chan/files/src/132287935236.gif
      */
      
    } catch (MalformedURLException e) {
      e.printStackTrace();
    }
  }
  
  /*private static void showImage() {
        //ImageIcon icon = new ImageIcon(imagesource);
        //JLabel label = new JLabel(icon, JLabel.CENTER);
        boolean userResponse=true;
        JScrollBar scrollBar=new JScrollBar();
        //label.add(scrollBar);
        //JOptionPane.showMessageDialog(null, label, "icon", -1);
        if(userResponse){
          saveImage();
        }
        else{nextImage();}
    }*/
  
  public static void showAndSaveImage() {
    //String format = getImageFormat();
    
    try {
        ReadableByteChannel in = Channels.newChannel(imagesource.openStream());
          FileOutputStream out = new FileOutputStream("test.html");//"temp."+format);
          out.getChannel().transferFrom(in, 0, 1 << 24);
          out.close();
    } catch (IOException e) {
      e.printStackTrace();
    }
    ImageIcon icon = new ImageIcon("temp.png");//+format);
    JLabel label = new JLabel(icon, JLabel.CENTER);
    JOptionPane.showMessageDialog(null, label, "icon", -1);
  }
  
  public static String getImageFormat() {
    String imagetitle = imagesource.toString();
    int len = imagetitle.length();
    return  imagetitle.substring(len-3,len);
  }

  public static void nextImage() {
    //This 
  }
}
