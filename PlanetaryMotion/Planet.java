/**
 * Represents a planet in the Solar System.
 *
 * @author: Josh Snider
 */

public class Planet {

  private String name;
  private double radius;
  private double period;
  //TODO This should be the planet's position at a reference date.
  private double radians;

  public Planet (String name, double radius, double period, double radians) {
    this.name = name;
    this.radius = radius;//TODO should be radius
    this.period = period;
    this.radians = radians;
  }

  public double[] getCoordinates() {
    double[] output = new double[2];
    output[0] = Math.cos(radians) * radius;
    output[1] = Math.sin(radians) * radius;
    return output;
  }

  public double[] getCoordinates(double time) {
    double[] output = new double[2];
    output[0] = Math.cos((double)2 * time * Math.PI/period) * radius;
    output[1] = Math.sin((double)2 * time * Math.PI/period) * radius;
    return output;
  }

  /**
   * Get this planet's name.
   */
  public String getName() {
    return name;
  }

  /**
   * Get the period it takes for this planet to orbit the sun.
   * @TODO Units?
   */
  public double getPeriod() {
    return period;
  }

  /**
   * Get this planet's orbital radius.
   * @TODO Units?
   */
  public double getRadius() {
    return radius;
  }

  public double getRadians() {
    return radians;
  }

  public void setRadians(double rads) {
    radians = rads;
  }
}
