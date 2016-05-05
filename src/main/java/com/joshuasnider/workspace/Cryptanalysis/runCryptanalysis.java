package com.joshuasnider.workspace.Cryptanalysis;

import javax.swing.JOptionPane;

public class runCryptanalysis {

  public static String cyphertext;

  public static int[] lettercnt = new int[26];

  public static void main(String[] args) {
    cyphertext = JOptionPane.showInputDialog("Input the cypher text.");
    run();
  }

  public static void run() {
    String command = JOptionPane.showInputDialog(
        "What method do you want to run?");
    if (command.equals("lettercnt")) {
      char[] alphabet = {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',
                         'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
                         'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'};
      lettercnt = Cryptanalysis.lettercnt(cyphertext);
      for (int x = 0; x < 26; x++) {
        System.out.println(alphabet[x] + ": " + lettercnt[x]);
      }
    }
  }
}
