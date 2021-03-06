package com.joshuasnider.workspace.internetio;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Collections;

public class WebCrawler {

  public static List<String> prevVisited = new ArrayList<String>();

  public static void main(String[] args) throws IOException {
    crawl("http://www.cnn.com");
  }

  public static void crawl(String url) throws IOException {
    System.out.println(url);
    String page = CommonFunctions.getWebpageAsString(url);
    //System.out.println(page);
    List<String> links = CommonFunctions.getLinks(page);
    links.removeAll(prevVisited);
    prevVisited.addAll(links);
    Collections.shuffle(links);
    for (String link : links) {
      try {
        crawl(link);
      } catch (Exception e){}
    }
  }
}
