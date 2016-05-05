/**
 * Small app to print the positions of the planets in their orbits.
 *
 * @author: Josh Snider
 */

import javax.swing.JOptionPane;

public class PlanetaryMotion {

  public static Planet[] planets = new Planet[8];

  public static void main(String[] args){
    //To calculate the x coordinate of a planet take the cosine
    //of the time multiplied by 2pi divided by the orbital period.
    //To calculate the y coordinate do the same but with the sine.
    planets[0] = new Planet("Mercury", 0.4, 88.0, 0.0);
    planets[1] = new Planet("Venus", 0.72, 224.7, 0.0);
    planets[2] = new Planet("Earth", 1.0, 365.25, 0.0);
    planets[3] = new Planet("Mars", 1.52, 686.971, 0.0);
    planets[4] = new Planet("Jupiter", 5.204, 4332.59, 0.0);
    planets[5] = new Planet("Saturn", 9.582, 10759.22, 0.0);
    planets[6] = new Planet("Uranus", 19.229, 30799.095, 0.0);
    planets[7] = new Planet("Neptune", 30.103, 60190, 0.0);

    String temp = JOptionPane.showInputDialog("Type in a name of a planet.");
    Planet planet = null;
    for (Planet iter : planets) {
      if (temp.equals(iter.getName())) {
        planet = iter;
        break;
      }
    }
    if (planet == null) {
      System.out.println("That's not a recognized planet.");
    } else {
      System.out.println("Giving coordinates for the planet " + planet.getName() + ".");
      for (double x = 0; x < 1000; x++) {
        double[] coordinates = planet.getCoordinates(x);
        System.out.println("The X-Coordinate on Day " + x + " is " + coordinates[0] + " AUs.");
        System.out.println("The Y-Coordinate is " + coordinates[1] + " AUs.");
      }

      advancePlanets(100.0);
      double[][] test = getCoordinates();
      for (int x = 0; x < 8; x++) {
        System.out.println(test[x][0] + " " + test[x][1]);
      }
    }
  }

  public static double[][] getCoordinates() {
    double[][] output = new double[8][2];
    for (int x = 0; x < 8; x++) {
      output[x][0] = planets[x].getCoordinates()[0];
      output[x][1] = planets[x].getCoordinates()[1];
    }
    return output;
  }

  public static void advancePlanets(double time) {
    for (Planet p : planets) {
      double period = p.getPeriod();
      p.setRadians((p.getRadians() + 2 * time * Math.PI / period) % (2 * Math.PI));
    }
  }

}
