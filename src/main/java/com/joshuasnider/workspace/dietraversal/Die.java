/**
 * This represents a die with numbers on each side.
 * It's used in an interesting puzzle.
 *
 * @author Josh Snider
 */

package com.joshuasnider.workspace.dietraversal;

import java.util.HashMap;
import java.util.Map;

public class Die {

  public enum Side {
    TOP,
    NORTH,
    EAST,
    SOUTH,
    WEST,
    BOTTOM
  }

  private Map<Side, Integer> faces;
  private Side currentTop;

  public Die(int top, int north, int east, int south,
      int west, int bottom) {
    faces = new HashMap<Side, Integer>();
    faces.put(Side.TOP, top);
    faces.put(Side.NORTH, north);
    faces.put(Side.EAST, east);
    faces.put(Side.SOUTH, south);
    faces.put(Side.WEST, west);
    faces.put(Side.BOTTOM, bottom);
    currentTop = Side.TOP;
  }

  public int getFace(Side side) {
    return faces.get(side);
  }

}
