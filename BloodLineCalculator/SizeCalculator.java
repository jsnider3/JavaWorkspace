/** Calculate how many generations it takes a population to grow past
 * a certain threshold.
 *
 */
public class SizeCalculator {
  public static void main(String[] args){
    int start = 3;
    int avgDescendentNumber = 4;
    int max = 1000000;
    int total = start;
    int Generations = 0;
    while (total < max) {
      total += total*avgDescendentNumber;
      Generations++;
    }
    System.out.println(Generations);
  }
}
