import static org.junit.Assert.*;

import org.junit.Test;

public class Esotest {

  @Test
  public void firstFour() {
    Esovector eso = new Esovector();
    eso.append(1);
    eso.append(2);
    eso.append(3);
    eso.append(4);
    int[] ls = eso.asArray();
    assertTrue(ls[0] == 1);
    assertTrue(ls[1] == 2);
    assertTrue(ls[2] == 3);
    assertTrue(ls[3] == 4);
    assertTrue(eso.get(0) == 1);
    assertTrue(eso.get(1) == 2);
    assertTrue(eso.get(2) == 3);
    assertTrue(eso.get(3) == 4);
  }

  @Test
  public void firstThree() {
    Esovector eso = new Esovector();
    eso.append(1);
    eso.append(2);
    eso.append(3);
    int[] ls = eso.asArray();
    assertTrue(ls[0] == 1);
    assertTrue(ls[1] == 2);
    assertTrue(ls[2] == 3);
    assertTrue(eso.get(0) == 1);
    assertTrue(eso.get(1) == 2);
    assertTrue(eso.get(2) == 3);
  }

  @Test
  public void firstTwo() {
    Esovector eso = new Esovector();
    eso.append(1);
    eso.append(2);
    int[] ls = eso.asArray();
    assertTrue(ls[0] == 1);
    assertTrue(ls[1] == 2);
    assertTrue(eso.get(0) == 1);
    assertTrue(eso.get(1) == 2);
  }

  @Test
  public void pushpop() {
    Esovector eso = new Esovector();
    for (int x = 0; x < 5; x++) {
      eso.append(x);
    }
    for (int x = 0; x < 5; x++) {
      assertTrue(eso.pop() == 4-x);
    }
    assertTrue(eso.getSize() == 0);
    for (int x = 0; x < 100; x++) {
      eso.append(x);
    }
    for (int x = 0; x < 100; x++) {
      assertTrue(eso.pop() == 99-x);
    }
    for (int x = 0; x < 100; x++) {
      eso.append(x);
    }
    for (int x = 0; x < 100; x++) {
      assertTrue(eso.pop() == 99-x);
    }
    assertTrue(eso.getSize() == 0);
    eso.append(6);
    eso.append(7);
    eso.append(8);
    assertTrue(eso.getSize() == 3);
    int[] arr = eso.asArray();
    assertTrue(arr[0] == 6);
    assertTrue(arr[1] == 7);
    assertTrue(arr[2] == 8);
  }

  @Test
  public void reverse() {
    Esovector eso = new Esovector();
    eso.append(1);
    eso.append(2);
    eso.append(3);
    int last = eso.get(2);
    eso.set(2, eso.get(0));
    eso.set(0, last);
    assertTrue(eso.get(0) == 3);
    assertTrue(eso.get(1) == 2);
    assertTrue(eso.get(2) == 1);
  }
}
