/**
 * This represents the path of a die on the board.
 * It's used in an interesting puzzle.
 *
 * @author Josh Snider
 */

package com.joshuasnider.workspace.dietraversal;

import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

public class Path {

  private List<Point> points;
  private Board board;
  private Die die;

  public Path(Board board, Die die) {
    this.board = board;
    this.die = die;
    points = new ArrayList<>();
    points.add(new Point(0, 0));
  }

  public Path(Board board, Die die, Path attempt) {
    this.board = board;
    this.die = die;
    points = new ArrayList<>();
    for (Point p : attempt.getPoints()) {
      addPoint(p);
    }
  }

  /**
   * Add a point to the end of our points list.
   */
  public void addPoint(Point point) {
    if (point.distance(points.get(points.size() - 1)) == 1 &&
        !points.contains(point)) {
      Die rotated = die.rotated(
        Board.getDirection(points.get(points.size() - 1), point));
      if (rotated.getFace(Die.Side.TOP) == board.getValue(point)) {
        points.add(point);
        die = rotated;
      } else {
        System.out.println(point);
        System.out.println(board.getValue(point));
        throw new IllegalArgumentException("Die with top " +
          rotated.getFace(Die.Side.TOP) + " can't move onto a " +
          board.getValue(point) + ".");
      }
    } else {
      throw new IllegalArgumentException("Not a valid position.");
    }
  }

  public List<Point> getPoints() {
    return points;
  }

  /**
   * Does this path reach from the top-left to bottom-right?
   */
  public boolean reachesEnd() {
    return points.size() > 1 && points.get(0).equals(new Point(0, 0)) &&
      points.get(points.size() - 1).equals(board.getBottomRight());
  }
}
