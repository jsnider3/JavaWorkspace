package com.joshuasnider.workspace.games.checkers;

import java.util.Scanner;

//TODO Sets up the game and starts it.
public class CheckersGame {

  public static void main(String[] args) {
    CheckersBoard board = new CheckersBoard(false);
    Scanner sc = new Scanner(System.in);
    while (!board.isOver()) {
      System.out.println(board);
      System.out.println(
        "Type the position x,y of the piece you want to move.");
      String input = sc.next();
      String[] v = input.split(",");
      System.out.println(v[0] + " " + v[1]);
    }
  }
}
