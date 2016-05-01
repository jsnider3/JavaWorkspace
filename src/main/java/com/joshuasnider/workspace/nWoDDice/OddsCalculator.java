/**
 * This class is used to calculate the odds of rolling certain
 *  numbers of "successes" using the New World of Darkness system.
 *
 * @author Josh Snider
 */

package com.joshuasnider.workspace.nwoddice;

public class OddsCalculator {

  private int again;
  Double[][] memo;

  public OddsCalculator(int aAgain) {
    again = aAgain;
    if (again < 8 || again > 10) {
      again = 10;
    }
    memo = new Double[10][10];
  }

  public double calculateEndAt(int goal, int dice) {
    //TODO This is only valid for non-rote.
    if (goal == 0 && dice == 0) {
      return 1;
    }
    if (goal == 0) {
      return Math.pow(getFailProportion(), dice);
    }
    if (dice == 0) {
      return 0;
    }
    if (getMemoValue(goal, dice) != null) {
        return getMemoValue(goal, dice);
    }
    double result = getFailProportion() * calculateEndAt(goal, dice - 1) +
         getSimpleSuccessProportion() * calculateEndAt(goal - 1, dice - 1) +
         getRerollProportion() * calculateEndAt(goal - 1, dice);
    setMemoValue(goal, dice, result);
    return result;
  }

  /**
   * Get the odds that a dice does not succeed.
   */
  public double getFailProportion() {
    return .7;
  }

  /**
   * Check if we've memoized this.
   */
  public Double getMemoValue(int goal, int dice) {
    Double memoValue = null;
    if (goal < 11 && dice < 11) {
      if (memo[goal-1][dice-1] != null) {
        memoValue = memo[goal - 1][dice - 1];
      }
    }
    return memoValue;
  }

  /**
   * Get the odds that a dice succeeds, but is not rerolled.
   */
  public double getSimpleSuccessProportion() {
    return 0.3 - getRerollProportion();
  }

  /**
   * Get the odds that a dice is rerolled.
   */
  public double getRerollProportion() {
    return (10 + 1 - again)/10.0;
  }

  /**
   * Memoize a calculation.
   */
  public void setMemoValue(int goal, int dice, double result) {
    if (goal < 11 && dice < 11) {
      memo[goal - 1][dice - 1] = result;
    }
  }

  /**
   * Calculate the odds that we meet our goal with a given
   *  number of dice.
   */
  public double calculateAtLeast(int goal, int dice) {
    double result = 1;
    for (int x = 0; x < goal; x++) {
      result -= calculateEndAt(x, dice);
    }
    return result;
  }

  /**
   * Calculate the odds that we roll less than our goal with a given
   *  number of dice.
   */
  public double calculateLessThan(int goal, int dice) {
    double result = 0;
    for (int x = 0; x < goal; x++) {
      result += calculateEndAt(x, dice);
    }
    return result;
  }

  public static void main(String[] args){
    int x = 0;
    double total = 0;
    OddsCalculator calc = new OddsCalculator(10);
    while (true) {
      double result = calc.calculateEndAt(10, x);
      total += result;
      System.out.println(x + " Result: " + result);
      x++;
    }
  }

}
