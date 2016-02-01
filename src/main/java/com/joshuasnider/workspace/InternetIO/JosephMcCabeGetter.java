package com.joshuasnider.workspace.internetio;

/**
* The purpose of this program is to download some old Joseph McCabe books from
*   http://www.infidels.org/library/historical/joseph_mccabe/big_blue_books/, so
*   that I could read them offline.
* TODO: As a whole this is terrible. No surprise given its age.
*   One thing that stands out is that this uses regexes to parse HTML.
*   This is forbidden and should be done with jsoup instead.

* @author Josh Snider.
*/

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;


public class JosephMcCabeGetter {
  public static void main(String[] args) throws Exception{
    String root = "http://www.infidels.org/library/historical/joseph_mccabe/big_blue_books/";
    for(int count = 1; count < 20; count++){
      String chapter = String.format("book_%02d", count);
      String page = CommonFunctions.getWebpageAsString(root + chapter + ".html");
      int delete = page.indexOf("Order books by and about Joseph McCabe now.");
      int len = "Order books by and about Joseph McCabe now.".length();
      page = page.substring(delete + len);
      ArrayList<String> pages = new ArrayList<>();
      pages.add(page);
      page = removeHtml(pages).get(0);
      saveFile(page, count);
    }
  }

  private static void print(ArrayList<String> s){
    for (String str : s)
    {
      System.out.println(str);
    }
  }

  private static ArrayList<String> removeHtml(ArrayList<String> p){
    //TODO This doesn't remove the ending properly.
    int x;
    for (x = 0; x < p.size(); x++) {
      String str = p.get(x);
      /*if(str.contains("Top of Page")){
        break;
        //TODO There's a bug that makes my program get stuck near the end of the page.
        //  This code is my solution
      }*/
      int start = str.indexOf("<");
      int cnt = 0;
      while (str.contains("<") && cnt < 5)
      {
        int end = str.indexOf(">");
        //TODO This line is terrible.
        str = str.substring(start, end).equals("<P") ? 
          str.substring(0, start) + "\n\n\t" + str.substring(end + 1) : 
          str.substring(0, start) + str.substring(end + 1);
        start = str.indexOf("<");
        cnt++;
      }
      p.set(x, str);
    }
    p = new ArrayList<String>(p.subList(0, x));
    return p;
  }

  private static void saveFile(String page, int x) throws IOException{
    //TODO This regex looks broken.
    String title = page.substring(0, page.indexOf("\n"));
    title.trim();
    if (title.equals("")) {
      title = "JosephMcCabe";
    }
    FileWriter txt = new FileWriter(x + "." + title + ".txt", false);
    BufferedWriter out = new BufferedWriter(txt);
    out.write(page);
    out.close();
    txt.close();
  }
}
