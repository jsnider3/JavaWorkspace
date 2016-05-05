/** Print a dancing man to the terminal.
 *  One of my first ever programs. Has since been refactored.
 */

package com.joshuasnider.workspace.dancingman;

public class DancingMan {

  public static void main(String[] args) {
    int dancepose = 0;
    String[] poses = {"<o>\n| \n/<\n",
                      " o>\n<|  \n >\\\n",
                      " o\n<|>\n /<\n"};
    while (true) {
      sleepOneFrame();
      System.out.println(poses[dancepose]);
      dancepose = (dancepose + 1) % 3;
    }
  }

  public static void sleepOneFrame() {
    try {
      Thread.sleep(125);
    } catch (InterruptedException e) {
      e.printStackTrace();
    }
  }
}
