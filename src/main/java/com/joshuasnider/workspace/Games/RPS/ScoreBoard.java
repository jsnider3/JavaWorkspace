/**
 * Represents the running score in a 2-player rock-paper-scissors game.
 *
 * @author Josh Snider
 */

package com.joshuasnider.workspace.games.rps;

public class ScoreBoard {

  private int cwins;
  private int uwins;
  private int ties;

  public ScoreBoard() {
    cwins = 0;
    uwins = 0;
    ties = 0;
  }

  public int getComputerWins() {
    return cwins;
  }

  public int getPlayerWins() {
    return uwins;
  }

  public int getTies() {
    return ties;
  }

  public void recordComputerWin() {
    cwins += 1;
  }

  public void recordPlayerWin() {
    uwins += 1;
  }

  public void recordTie() {
    ties += 1;
  }
}
