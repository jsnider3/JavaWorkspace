package com.joshuasnider.workspace.internetio;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;

import java.util.ArrayList;
import java.util.List;

public class UHaul {

  public static boolean control = false;

  public static void main(String[] args) throws IOException {
    List<String> page = getWebpage("http://www.uhaul.com/Locations/Trucks-for-sale-near-Spotsylvania-VA-22553/045044/");
    ArrayList<String> output = new ArrayList<String>();
    output.add("/Trucks-for-sale-near-Spotsylvania-VA-22553/045044/");
    while (control) {
      //scanForTitle(page,output);
      scanForTrucks(page, output);//Add the trucks to output.
      scanForNext(page, output);//Add the name of the next page to output and go there.
    }
    for (String s : page) {
      System.out.println(s);
    }
  }

  private static void scanForNext(List<String> page, List<String> output) {
    // TODO Auto-generated method stub

  }

  private static void scanForTrucks(List<String> page, List<String> output) {
    // TODO Auto-generated method stub

  }

  private static void scanForTitle(List<String> page, List<String> output) {
    // TODO Auto-generated method stub

  }

  public static List<String> getWebpage(String url) throws IOException {
    URLConnection webpage = new URL(url).openConnection();
    BufferedReader in = new BufferedReader(
      new InputStreamReader(webpage.getInputStream()));
    ArrayList<String> page = new ArrayList<String>();
    String input;
    while ((input = in.readLine()) != null) {
      page.add(input);
    }
    in.close();
    return page;
  }
}
