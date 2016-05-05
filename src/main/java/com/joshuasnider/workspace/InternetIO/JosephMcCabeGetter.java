/**
* The purpose of this program is to download some old Joseph McCabe books from
*   http://www.infidels.org/library/historical/joseph_mccabe/big_blue_books/, so
*   that I could read them offline.

* @author Josh Snider.
*/

package com.joshuasnider.workspace.internetio;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.FileOutputStream;
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
        Document book = Jsoup.parse(
          doc.select("div[id=webpage]").get(0).toString());
        try {
          saveFile(book, count);
        } catch (IOException e) {
          System.err.println("Could not save book " + count + ".");
        }
      } catch (IOException e) {
        System.err.println("Could not get chapter " + chapter + ".");
      }
    }
  }

  /**
   * Get the book's title.
   */
  private static String getTitle(Document doc) {
    String title = "JosephMcCabe";
    Elements els = doc.select("h1");
    if (els.size() > 0) {
      Element header = els.get(0);
      title = header.html();
    }
    return title;
  }

  /**
   * Save a file as .html.
   */
  private static void saveFile(Document doc, int x)
      throws IOException {
    String title = getTitle(doc);
    try (FileWriter txt = new FileWriter(x + "." + title + ".html", false)) {
      try (BufferedWriter out = new BufferedWriter(txt)) {
        out.write(doc.toString());
      }
    }
  }

}
