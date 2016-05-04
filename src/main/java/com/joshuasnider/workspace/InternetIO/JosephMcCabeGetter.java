/**
* The purpose of this program is to download some old Joseph McCabe books from
*   http://www.infidels.org/library/historical/joseph_mccabe/big_blue_books/, so
*   that I could read them offline.
* TODO: As a whole this is terrible. No surprise given its age.
*   One thing that stands out is that this uses regexes to parse HTML.
*   This is forbidden and should be done with jsoup instead.

* @author Josh Snider.
*/

package com.joshuasnider.workspace.internetio;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

import java.util.List;
import java.util.ArrayList;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class JosephMcCabeGetter {

  public static void main(String[] args) {
    String root = "http://www.infidels.org/library/historical/joseph_mccabe/big_blue_books/";
    for (int count = 1; count < 20; count++) {
      String chapter = String.format("book_%02d", count);
      try {
        Document doc =
          Jsoup.connect(root + chapter + ".html").get();
        String page = doc.select("div[id=webpage]").get(0).toString();
        page = removeHtml(page);
        try {
          saveFile(page, count);
        } catch (IOException e) {
          System.err.println("Could not save book " + count + ".");
        }
      } catch (IOException e) {
        System.err.println("Could not get chapter " + chapter + ".");
      }
    }
  }

  /**
   * Remove some html from the given string to make it prettier.
   * @Deprecated: Should be possible to do with Jsoup.
   */
  private static String removeHtml(String str) {
    int start = str.indexOf("<hr />");
    return str.substring(start + 6);
  }

  /**
   * Get the title for a page and save it there.
   */
  private static void saveFile(String page, int x)
      throws IOException {
    Document doc = Jsoup.parse(page);
    Element header = doc.select("h1").get(0);
    String title = header.html();
    try (FileWriter txt = new FileWriter(x + "." + title + ".txt", false)) {
      try (BufferedWriter out = new BufferedWriter(txt)) {
        out.write(page);
      }
    }
  }
}
