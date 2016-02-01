package com.joshuasnider.workspace.internetio;

import java.io.IOException;
import java.net.URL;
import javax.swing.JOptionPane;


public class CraigslistListener {

  public static void main(String[] args) throws IOException, InterruptedException{
    URL craigslist = new URL("http://washingtondc.craigslist.org/search/cas?query=w4m");
    //Initialize a string buffer;
    String buffer1 = CommonFunctions.getWebpageAsString(craigslist.toString());
    System.out.println(buffer1);
    while(true){
      Thread.sleep(150000);
      String buffer2 = CommonFunctions.getWebpageAsString(craigslist.toString());
      if(!buffer1.equals(buffer2)){
        JOptionPane.showMessageDialog(null,"New post.");//Pop up notice.
        buffer1=buffer2;
        System.out.println(buffer2);//Print buffer2
      }
    }
  }
}
