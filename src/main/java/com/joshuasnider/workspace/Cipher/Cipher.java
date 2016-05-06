/**
 * Implements a modified Caesar Cipher.
 * The key of the Caesar Cipher increases by one after each character.
 * One of my first programs. Slightly refactored since.
 */

package com.joshuasnider.workspace.cipher;

import java.util.Scanner;
public class Cipher {

  private int key;

  public Cipher(int key) {
    this.key = key;
  }

  /**
   * Decode the text using the starting key.
   */
  public String decode(String text) {
    StringBuffer out = new StringBuffer();
    for (char c : text.toCharArray()) {
      if (c >= 'a' && c <= 'z') {
        c -= key;
        c = moduluBounds(c, (int)'a', (int)'z');
      } else if (c >= 'A' && c <= 'Z') {
        c -= key;
        c = moduluBounds(c, (int)'A', (int)'Z');
      }
      key++;
      if (key > 25) {
        key -= 26;
      }
      out.append(c);
    }
    return out.toString();
  }

  /**
   * Encode the text using the starting key.
   */
  public String encode(String text) {
    StringBuffer out = new StringBuffer();
    for (char c : text.toCharArray()) {
      if (c >= 'a' && c <= 'z'){
        c += key;
        c = moduluBounds(c, (int)'a', (int)'z');
      } else if ( c >= 'A' && c <= 'Z') {
        c += key;
        c = moduluBounds(c, (int)'A', (int)'Z');
      }
      key++;
      if (key > 25) {
        key -= 26;
      }
      out.append(c);
    }
    return out.toString();
  }

  /**
   * Make sure c is within the bounds [lo, hi].
   */
  public char moduluBounds(char c, int lo, int hi) {
    int range = hi - lo;
    if (c > hi) {
      return (char)(c - range);
    } else if (c < lo) {
      return (char)(c + range);
    }
    return c;
  }

  public static void main(String[] args) {
    Scanner input = new Scanner(System.in);
    System.out.println("Basic Cipher by Josh Snider");
    System.out.println("Do you want to encode or decode?");
    String text = input.nextLine();
    boolean encode = text.equalsIgnoreCase("encode");
    System.out.println("Type in the text.");
    text = input.nextLine();
    System.out.println("What's the key?");
    int key = input.nextInt();
    String output = "";
    if (encode)
    {
      output = new Cipher(key).encode(text);
    } else {
      output = new Cipher(key).decode(text);
    }
    System.out.println(output);
  }
}
