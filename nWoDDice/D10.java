/**
 * Class to simulate rolling a single D10 in nWoD.
 */

import java.util.Random;

public class D10 {

  private Random rng;
  private int value;

  public D10() {
    rng = new Random();
  }

  public int getValue() {
    return value;
  }

  /**
   * If this die roll, succeeded.
   */
  public boolean isSuccess() {
    return value >= 8;
  }

  /**
   * Get new value by rolling.
   */
  public int roll() {
    value = rng.nextInt(10) + 1;
    return value;
  }

}
