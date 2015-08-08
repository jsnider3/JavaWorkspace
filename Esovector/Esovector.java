
import java.util.Arrays;
public class Esovector {
  private int[] first;
  private int[] secnd;
  private int size;
  private int split;

  /** Create a basic esovector.
  * Initial capacity is 2.
  */
  public Esovector() {
    first = new int[2];
    size = 0;
    split = 2;
  }

  /** Add push to the end of this esovector.
  */
  public void append(int push) {
    if (size < first.length) {
      first[size] = push;
    }
    else {
      if (secnd == null) {
        secnd = new int[2 * first.length];
      }
      secnd[size] = push;
      split--;
      secnd[split] = first[split];
      if (split <= 0) {
        first = secnd;
        secnd = null;
        split = first.length;
      }
    }
    size++;
  }

  /** Convert this into an int[].
  */
  public int[] asArray() {
    int[] copy = new int[size];
    for (int ind = 0; ind < size; ind++) {
      copy[ind] = get(ind);
    }
    return copy;
  }
  
  /** Get the nth element of this esovector.
  */
  public int get(int index) {
    assert(0 <= index && index < size);
    if (index < split) {
      return first[index];
    } else {
      return secnd[index];
    }
  }

  /** Remove the last element.
  */
  public int pop() {
    assert (size > 0);
    int ret = get(size - 1);
    size--;
    if (size == 0) {  
      first = new int[2];
      secnd = null;
      size = 0;
      split = 2;
    }
    else if (secnd != null) {
      first[split] = secnd[split];
      split++;
      if (split >= size) {
        secnd = null;
        split = first.length; 
      }
    }
    return ret;
  }

  /** Print useful debug information.
  */
  public void printDebug() {
    System.out.println(Arrays.toString(first));
    System.out.println(Arrays.toString(secnd));
    System.out.println(split + " " + size);
  }

  /** Set the nth element.
  */
  public void set(int index, int change) {
    assert(0 <= index && index < size);
    if (index < split) {
      first[index] = change;
    } else {
      secnd[index] = change;
    }
  }

  /** Get the number of elements.
  */
  public int getSize() {
    return size;
  }

}
