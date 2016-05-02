/**
  * Command-line implementation of TicTacToe.
  * The AI uses min-max to make its moves.
  *
  * Originally written in 2012. Refactored heavily since.
  *
  * @author: Josh Snider
  */

package com.joshuasnider.workspace.games.tictactoe;

import java.util.Scanner;

public class TicTacToeCLI implements Runnable {

  public static final String[][] BLANK_ARRAY =
      {{" ", " ", " "},
       {" ", " ", " "},
       {" ", " ", " "}};

  private TicTacToeBoard board;

  private Scanner sc;

  public TicTacToeCLI() {
    sc = new Scanner(System.in);
    board = new TicTacToeBoard(BLANK_ARRAY, false);
  }

  public static void main(String[] args){
    new TicTacToeCLI().run();
  }

  public void run() {
    System.out.println("Do you want to go first? Yes/No");
    boolean playerFirst = sc.next().toLowerCase().startsWith("y");
    //If the user types in Yes.
    System.out.println("0|1|2");
    System.out.println("-----");
    System.out.println("3|4|5");
    System.out.println("-----");
    System.out.println("6|7|8");
    if (!playerFirst) {
      board.compMove();
      System.out.print(board);
    }
    while (!board.isOver()) {
      //While the game is not over
      board.Move(getMove());
      if (!board.isOver()) {
        board.compMove();
      }
      System.out.print(board);
    }
    if (board.hasWon("X")) {
      System.out.print("X wins.");
    } else if (board.hasWon("O")) {
      System.out.print("O wins.");
    } else if (board.isDraw()) {
      System.out.print("Draw.");
    }
  }

  private short getMove(){
    System.out.println("Enter a number for your move.");
    short move = sc.nextShort();
    while (move > 8 || move < 0 ||
           !board.getEmptySpots().contains(move)) {
      System.out.println("That spot's taken.");
      System.out.println("Enter a new one");
      move = sc.nextShort();
    }
    return move;
  }
}
