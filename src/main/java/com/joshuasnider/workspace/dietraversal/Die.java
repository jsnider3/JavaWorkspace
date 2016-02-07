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

  public Die(int top, int north, int east, int south,
      int west, int bottom) {
    faces = new HashMap<Side, Integer>();
    faces.put(Side.TOP, top);
    faces.put(Side.NORTH, north);
    faces.put(Side.EAST, east);
    faces.put(Side.SOUTH, south);
    faces.put(Side.WEST, west);
    faces.put(Side.BOTTOM, bottom);
  }

  public int getFace(Side side) {
    return faces.get(side);
  }

  /**
   * Tip this die in the given direction.
   */
  public Die rotated(Side direction) {
    Die rotated = null;
    switch (direction) {
      case NORTH:
        rotated = new Die(getFace(Side.SOUTH), getFace(Side.TOP),
                    getFace(Side.EAST), getFace(Side.BOTTOM),
                    getFace(Side.WEST), getFace(Side.NORTH));
        break;
      case EAST:
        rotated = new Die(getFace(Side.WEST), getFace(Side.NORTH),
                    getFace(Side.TOP), getFace(Side.SOUTH),
                    getFace(Side.BOTTOM), getFace(Side.EAST));
        break;
      case SOUTH:
        rotated = new Die(getFace(Side.NORTH), getFace(Side.BOTTOM),
                    getFace(Side.EAST), getFace(Side.TOP),
                    getFace(Side.WEST), getFace(Side.SOUTH));
        break;
      case WEST:
        rotated = new Die(getFace(Side.EAST), getFace(Side.NORTH),
                    getFace(Side.BOTTOM), getFace(Side.SOUTH),
                    getFace(Side.TOP), getFace(Side.WEST));
        break;
      default:
        throw new IllegalArgumentException("Can't move in " + direction);
    }
    return rotated;
  }

}
