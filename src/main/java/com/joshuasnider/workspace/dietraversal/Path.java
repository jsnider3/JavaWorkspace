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
      points.add(point);
    } else {
      throw new IllegalArgumentException("Can't move there.");
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
