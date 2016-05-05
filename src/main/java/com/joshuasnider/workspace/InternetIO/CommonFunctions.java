package com.joshuasnider.workspace.internetio;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.net.URLConnection;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JOptionPane;


public class CommonFunctions {

  public static String getWebpageAsString(String url) throws IOException{
    URL page = new URL(url);
    URLConnection con = page.openConnection();
    Reader r = new InputStreamReader(con.getInputStream());
    StringBuilder buf = new StringBuilder();
    while (true) {
      int ch = r.read();
        if (ch < 0)
          break;
        buf.append((char) ch);
    }
    String temp = buf.toString();
    return temp;
  }

  public static boolean saveImage(String fileLoc,String saveTo){
    try {
      ReadableByteChannel in = Channels.newChannel(new URL(fileLoc).openStream());
      FileOutputStream out = new FileOutputStream(saveTo);
      out.getChannel().transferFrom(in, 0, 1 << 24);
      out.close();
      return true;
    } catch (IOException e) {
      e.printStackTrace();
      return false;
    }
  }

  public static List<String> getLinks(String page) throws IOException{
    //TODO This isn't working.
    Pattern pattern = Pattern.compile("(https?|ftp|file)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]");
    Matcher matcher = pattern.matcher(page);
    ArrayList<String> links = new ArrayList<String>();
    while (matcher.find())
      links.add(matcher.group());
    return links;
  }

}
